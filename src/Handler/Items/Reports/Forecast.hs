module Handler.Items.Reports.Forecast where

import Import
import Items.Types
import qualified Data.Csv as Csv
import Handler.CsvUtils
import Handler.Items.Category.Cache
import Items.Internal
import qualified Data.IntMap as IntMap
import System.FilePath.Glob (glob)
import FA as FA hiding (unUserKey)
import Control.Monad.Fail (MonadFail(..))
import GL.Utils
import GL.Payroll.Settings(DayOfWeek(..))
import Database.Persist.MySQL -- (BackendKey(SqlBackendKey))
import qualified Data.Conduit.List as CL
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty(..))
import Util.ForConduit
import qualified Data.Vector.Generic as V -- not Generic as Generics but generic interface over all types of vector
import qualified Data.Map as Map

-- * Profiles 
-- | Read a map of season profiles from a valid csv
-- collection,month,weight
data CollectionProfileRow = CollectionProfileRow
 { cpCollection :: Collection
 , cpMonth :: Int
 , cpWeight :: Double
 } deriving Show
instance Csv.FromNamedRecord CollectionProfileRow where
  parseNamedRecord m = do
    collection <- fmap Collection ( m Csv..: "collection")
    weight <- m Csv..: "weight"
    month' <- m Csv..: "month"
    month <- parseMonth month'
    return $ CollectionProfileRow collection month weight
    
     
parseMonth :: Text -> Csv.Parser Int
parseMonth m = case m of
  "Jan" -> return 1
  "Feb" -> return 2
  "Mar" -> return 3
  "Apr" -> return 4
  "May" -> return 5
  "Jun" -> return 6
  "Jul" -> return 7
  "Aug" -> return 8
  "Sep" -> return 9
  "Oct" -> return 10
  "Nov" -> return 11
  "Dec" -> return 12
  _ -> fail "Can't parse month"
    
    
readProfiles :: FilePath -> IO (Map Collection SeasonProfile)
readProfiles path = do
  content <- readFile path
  let Right cols = parseSpreadsheet mempty Nothing content
      monthMap (CollectionProfileRow _ month weight) = IntMap.singleton month weight
      grouped = groupAsMap cpCollection monthMap cols
  return $ fmap seasonProfileFromMap grouped


-- * Sku Speed 
-- | Row coming from a sku speed file.
newtype Collection = Collection Text
  deriving (Show, Eq, Ord)

data SkuSpeedRow = SkuSpeedRow
  { ssSku :: Sku
  , ssWeight :: Double
  , ssCollection :: Collection
  }deriving (Show)
instance Csv.FromNamedRecord SkuSpeedRow where
  parseNamedRecord m = SkuSpeedRow  <$> fmap Sku (m Csv..: "stock_id")
                                    <*> m Csv..: "eQty"
                                    <*> fmap Collection (m Csv..: "collection")

                  
-- | Load sku speed from a csv
loadSkuSpeed :: FilePath -> IO  [SkuSpeedRow]
loadSkuSpeed filepath = do
  content <- readFile filepath
  case parseSpreadsheet mempty Nothing content of
    Left err -> error $ show err
    Right rows -> return rows

-- | Generate fake transactions corresponding to forecast sales
loadItemForecast ::  Maybe InOutward -> FilePath -> (Map Sku ItemInitialInfo) -> Day -> Day -> Handler [(TranKey, TranQP)]
loadItemForecast io forecastDir infoMap start end = do
  settings <- getsYesod appSettings
  catFinder <- categoryFinderCached (appForecastCollectionCategory settings)
  profiles <- liftIO $ readProfiles (forecastDir </> "collection_profiles.csv")
  let profile (Sku sku) = (catFinder  (FA.StockMasterKey sku) >>= (\col ->  lookup (Collection col) profiles)
                          ) <|> Just flatProfile
      flatProfile = seasonProfile []
  skuFiles <- liftIO $ glob (unpack $ forecastDir </> "*sku_forecast.csv" )
  when (null skuFiles) $ do
     setError "Can't find any sku speed files. Please check with your administrator."

  skuSpeedMap <- liftIO $ mapM loadSkuSpeed skuFiles
  let skuSpeeds = concat skuSpeedMap 
  return $ concatMap (skuSpeedRowToTransInfo infoMap profile start end io) skuSpeeds

skuSpeedRowToTransInfo :: Map Sku ItemInitialInfo
                       -> (Sku -> Maybe SeasonProfile)
                       -> Day
                       -> Day
                       -> Maybe InOutward
                       -> SkuSpeedRow
                       -> [(TranKey, TranQP)]
skuSpeedRowToTransInfo infoMap profileFor start end iom (SkuSpeedRow sku speed _) =
  let io = fromMaybe Outward iom  --   ^ like sales
      extra = maybe [] ioToQPType iom
  in case (profileFor sku, lookup sku infoMap) of
    (Just profile, Just info) -> do -- []
      (day0, weight) <- weightsForRange profile start end
      guard (weight > 1e-6)
      let key = TranKey day0
                    Nothing
                    sku
                    Nothing -- style
                    Nothing -- var
                    mempty
                    mempty
                    (case io of
                       Inward -> ST_PURCHORDER
                       Outward -> ST_SALESINVOICE)
                    Nothing Nothing mempty

          qp = mkQPrice io (weight * speed) (fromMaybe 0 $ iiSalesPrice info)
          tqp = tranQP' extra QPSalesForecast qp
      return (key, tqp)
    _ -> []
    

-- * Forecast error

-- | Load actual sales for a whole year 
loadYearOfActualCumulSalesByWeek :: Day -> (Day, SqlConduit () (ForMap Sku UWeeklyAmount) ())
loadYearOfActualCumulSalesByWeek end = 
   let endOfPreviousWeek = calculateDate (BeginningOfWeek  Monday) end
       start = calculateDate (AddDays $ -365) endOfPreviousWeek -- exactly 52 weeks
       mkVec (ForMap sku week'amounts) = let
           v0 = V.replicate 52 0 :: UWeeklyAmount
           va = v0 V.// week'amounts
           
           in -- traceShow (week'amounts, v0) $
              (sku, va)
       source = actualSalesSource start endOfPreviousWeek
                 .| mapC mkVec
                 .| mapC  (\(sku, amounts) -> ForMap sku  $ V.postscanl' (+) 0 amounts)
   in (start, source)

-- | load sales from stock moves between the given date (end excluded)
-- sorted by sku 
actualSalesSource :: Day -> Day -> SqlConduit () (ForMap Sku [(Int, Amount)]) ()
actualSalesSource start end = do
   let _sql = "SELECT stock_id, (MIN(TO_DAYS(tran_date)) - TO_DAYS(?))/7 AS days, -sum(qty_done*unit_price)  ":
           --                  offset week so that the first one is 0
           " FROM 0_debtor_trans_details " :
           " JOIN 0_debtor_trans ON (0_debtor_trans_details.debtor_trans_no = 0_debtor_trans.trans_no AND debtor_trans_type = type) " :
           " JOIN 0_stock_moves using(trans_no, type, stock_id, tran_date)" : -- ignore credit note without move => damaged
           " WHERE type IN ("  : (tshow $ fromEnum ST_CUSTDELIVERY) : ",": (tshow $ fromEnum ST_CUSTCREDIT) : ") " :
           " AND qty_done != 0" :
           -- " AND stock_id = ?" :
           " AND tran_date >= ? AND tran_date < ? " :
           " GROUP BY stock_id, YEARWEEK(tran_date,5) " :
           " order BY stock_id, YEARWEEK(tran_date,5)  " :
           []
   -- only use stock moves to make it much faster
   let sql = "SELECT stock_id, (MIN(TO_DAYS(tran_date)) - TO_DAYS(?))/7 AS days, -sum(qty*price) " :
           " FROM 0_stock_moves " :
           " WHERE type IN ("  : (tshow $ fromEnum ST_CUSTDELIVERY) : ",": (tshow $ fromEnum ST_CUSTCREDIT) : ") " :
           " AND qty != 0" :
           " AND stock_id like 'M%'" :
           " AND tran_date >= ? AND tran_date < ? " :
           " GROUP BY stock_id, YEARWEEK(tran_date,5) " :
           " order BY stock_id, YEARWEEK(tran_date,5) " :
           []
       weekSource = rawQuery  (mconcat sql) [toPersistValue start, toPersistValue start, toPersistValue end] 
       myCoerce :: [PersistValue] -> (Sku, (Int, Amount))
       myCoerce vs = case rawSqlProcessRow vs  of
                          Left e -> error $ unpack e
                          Right v -> coerce (v :: (Single Text, (Single Int, Single Amount)))
       run :: NonEmpty (Sku, (Int, Amount)) -> ForMap Sku [(Int, Amount)]
       run nonEmpty = let (sku :|  _, week'amounts) = unzip nonEmpty
                      in ForMap sku (toList week'amounts)
                 
   weekSource .| mapC myCoerce
              .| CL.groupOn fst
              .| mapC run



loadYearOfForecastCumulByWeek :: Day -> FilePath -> IO (Map Sku UWeeklyAmount)
loadYearOfForecastCumulByWeek end forecastDir = do
  rawProfiles <- readProfiles $ forecastDir  </> "collection_profiles.csv"
  skuSpeed <- loadSkuSpeed $ forecastDir </> "mw_sku_forecast.csv"
  let weekProfiles = fmap (expandProfileWeekly end) rawProfiles
      linear = V.replicate 52 (1/52)
      weeklyForRow (SkuSpeedRow _ weight collection) = V.map (*weight) weelky where
          weelky = findWithDefault linear collection weekProfiles
                          
      skuMap = Map.fromList [(ssSku row, weeklyForRow row)
                            | row <- skuSpeed
                            ]
  return skuMap
  
  

expandProfileWeekly :: Day -> SeasonProfile -> UWeeklyAmount
expandProfileWeekly _end (SeasonProfile profile) = let
   v0 = V.replicate 52 0
   v1 =  v0 V.// zip [0,4..54] profile
   in V.postscanl'  (+) 0 v1

   
   
