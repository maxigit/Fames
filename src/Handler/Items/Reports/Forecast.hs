{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module Handler.Items.Reports.Forecast where

import Import
import Items.Types
import qualified Data.Csv as Csv
import Handler.CsvUtils
import Handler.Items.Category.Cache
import Handler.Items.Common(StockFilter, stockFilterToSqlWithColumn, stockFilterToSql)
import Items.Internal
import qualified Data.IntMap as IntMap
import System.FilePath.Glob (glob)
import FA as FA hiding (unUserKey)
import Control.Monad.Fail (MonadFail(..))
import GL.Utils
-- import GL.Payroll.Settings(DayOfWeek(..))
import Database.Persist.MySQL -- (BackendKey(SqlBackendKey))
import qualified Data.Conduit.List as CL
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty(..))
import Util.ForConduit
import qualified Data.Vector.Generic.Sized as V -- not Generic as Generics but generic interface over all types of vector
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
    
data ForecastGrouper k where 
         SkuGroup :: ForecastGrouper Sku
         CategoryGroup :: Text  -> ForecastGrouper Text
         CustomerGroup :: ForecastGrouper Text
   
mkForecastKey :: ForecastGrouper k -> Text -> k
mkForecastKey SkuGroup txt = Sku txt
mkForecastKey (CategoryGroup _) txt = txt
mkForecastKey CustomerGroup txt = txt

unForecastKey :: ForecastGrouper k -> k -> Text
unForecastKey SkuGroup (Sku sku) = sku
unForecastKey (CategoryGroup category) name = category++":"++name
unForecastKey CustomerGroup name = name
     
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
loadYearOfActualCumulSalesByWeek :: ForecastGrouper key -> StockFilter -> Day -> (Day, Day, SqlConduit () (ForMap key (U53Weeks Quantity)) ())
loadYearOfActualCumulSalesByWeek grouper stockFilter start = 
   let -- find first monday >= start
       end = calculateDate (Chain [ AddYears 1, AddDays $ -1 ]) start
       mkVec (ForMap sku week'quantitys) = let
           va = 0 `V.unsafeUpd` week'quantitys
           
           in -- traceShow (week'quantitys, v0) $
              (sku, va)
       source = actualSalesSource grouper stockFilter start end
                 .| mapC mkVec
                 .| mapC  (\(sku, quantitys) -> ForMap sku  $ V.postscanl' (+) 0 quantitys)
   in (start, end, source)

-- | load sales from stock moves between the given date (end excluded)
-- sorted by sku 
actualSalesSource :: forall key . ForecastGrouper key -> StockFilter -> Day -> Day -> SqlConduit () (ForMap key [(Int, Quantity)]) ()
actualSalesSource grouper stockFilter start end = do
   let (stockJoinM, stockWhereM, stockParams) = stockFilterToSqlWithColumn "moves.stock_id" stockFilter
   let sql = "SELECT " <> groupKey <> " AS groupKey, DATEDIFF(tran_date,?) DIV 7 AS days, -sum(qty)" :
           " FROM 0_stock_moves moves " :
           " LEFT JOIN 0_debtor_trans USING(type, trans_no, tran_date) " :
           " LEFT JOIN fames_customer_category_cache AS clearance ON (debtor_no = customer_id AND category = 'clearance') " :
           sqlJoin ?:
           stockJoinM ?:
           " WHERE type IN ("  : (tshow $ fromEnum ST_CUSTDELIVERY) : ",": (tshow $ fromEnum ST_CUSTCREDIT) : ") " :
           (fmap (" AND " <>) stockWhereM) ?:
           " AND qty != 0" :
           " AND (clearance.value is null OR clearance.value <> 'Yes' ) " :
           " AND moves.stock_id like 'M%'" :
           -- " AND stock_id like 'ML17-FD7-NAY'" :
           -- " AND stock_id like 'ML13-AD1-IVY'" :
           " AND tran_date >= ? AND tran_date <= ? " :
           " GROUP BY groupKey, days " :
           " order BY groupKey, days " :
           []
       (groupKey, joinParams, sqlJoin) = case grouper of 
                                   SkuGroup -> ("moves.stock_id"
                                               , []
                                               , Nothing
                                               )
                                   CategoryGroup category -> ("category.value"
                                                             , [PersistText  category ]
                                                             , Just "JOIN fames_item_category_cache AS category  ON (category.category = ? AND moves.stock_id = category.stock_id )"
                                                             )
                                   CustomerGroup -> ("0_debtors_master.name"
                                                    , []
                                                    , Just "JOIN 0_debtors_master USING (debtor_no) "
                                                    )
       weekSource = rawQuery  (mconcat sql) $ toPersistValue start : joinParams ++ stockParams ++ [ toPersistValue start, toPersistValue end] 
       --                                     ^^^^^^^^^^^^^^^^^^^
       --                                        |
       --                                        +---- selecting week number
       myCoerce :: [PersistValue] -> (Text, (Int, Quantity))
       myCoerce vs = case rawSqlProcessRow vs  of
                          Left e -> error $ unpack e
                          Right v -> coerce (v :: (Single Text, (Single Int, Single Quantity)))
       run :: NonEmpty (Text, (Int, Quantity)) -> ForMap key [(Int, Quantity)]
       run nonEmpty = let (sku :|  _, week'quantitys) = unzip nonEmpty
                      in ForMap (mkForecastKey grouper sku) (toList week'quantitys)
                 
   weekSource .| mapC myCoerce
              .| CL.groupOn fst
              .| mapC run



loadYearOfForecastCumulByWeek :: Ord key => ForecastGrouper key -> StockFilter -> Day -> FilePath -> Handler (Map key (U53Weeks Quantity))
loadYearOfForecastCumulByWeek grouper stockFilter start forecastDir = do
  -- load forecast from files
  rawProfiles <- liftIO $ readProfiles $ forecastDir  </> "collection_profiles.csv"
  skuSpeed <- liftIO $ loadSkuSpeed $ forecastDir </> "mw_sku_forecast.csv"
  -- creates weekly profiles for each month
  mkKey <- case grouper of
                SkuGroup -> return \sku -> Sku sku
                CategoryGroup category -> do
                     catFinder <- categoryFinderCached category
                     return \sku -> case catFinder (FA.StockMasterKey sku) of
                                         Nothing -> category
                                         Just name -> name
                CustomerGroup  -> return \cust -> cust
  -- load filtered object
  keepSku <- case stockFilterToSql stockFilter of
               (Nothing, Nothing, _) -> return $ const True
               (stockJoinM, stockWhereM, params) -> do
                   let sql = "SELECT stock_id FROM 0_stock_master " <> fromMaybe "" stockJoinM
                                                                    <> " WHERE "
                                                                    <> fromMaybe "1" stockWhereM
                   singles <- runDB $ rawSql sql params
                   let stockSet = setFromList $ map unSingle singles :: Set Text
                   return $ \(Sku sku) -> sku `member` stockSet
                        
  let weekProfiles = fmap expandProfileWeekly rawProfiles
      weekProfiles ::  Map Collection (U53Weeks Quantity)
      monthWeekly :: [U53Weeks Quantity] 
      monthWeekly = monthFractionPerWeek  (calculateDate (Chain [ AddYears 1,  AddDays (-1)]) start)
      expandProfileWeekly :: SeasonProfile -> U53Weeks Quantity
      expandProfileWeekly (SeasonProfile profile) =  let v = V.postscanl' (+) 0 $
                                                                        foldl1Ex' (+)  $ zipWith (\monthWeight weeks -> V.map (*monthWeight) weeks)
                                                                                                   profile
                                                                                                   monthWeekly
                                                     in v
      linear = V.map (min 1) $ V.postscanl' (+) 0 $ V.replicate (1/52)  
      weeklyForRow (SkuSpeedRow _ weight collection) = V.map ((*1).(*weight)) weekly where
          weekly = findWithDefault linear collection weekProfiles
                          
      skuMap = Map.fromListWith (+) [(mkKey . unSku $ ssSku row, weeklyForRow row )
                                     | row <- skuSpeed
                                     , keepSku (ssSku row)
                                     ]
  return skuMap
  
  

-- | Compute for each month its year fraction for each weeks
monthFractionPerWeek :: Day -> [U53Weeks Quantity]
monthFractionPerWeek start = let
   monthForWeek = [ (fromIntegral (d `div` 7),  month)
                  | (d, day) <- zip [0.. ] [start .. calculateDate (Chain [AddYears 1 , AddDays (-1)]) start ]
                  , let (_,month, _) = toGregorian day
                  ]
   in [ V.accum (+) 0 updates
      | month <- [1..12]
      , let updates = let ups = [ (w, 1/monthLength) | (w, m) <- monthForWeek, m == month ]
                          monthLength = fromIntegral $ length ups
                      in ups
      ]
       




   
   
