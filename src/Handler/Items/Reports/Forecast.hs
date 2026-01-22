{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
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
-- import GL.Payroll.Settings(DayOfWeek(..))
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
    
data ForecastGrouper k where 
         SkuGroup :: ForecastGrouper Sku
         CategoryGroup :: Text  -> ForecastGrouper Text
   
mkForecastKey :: ForecastGrouper k -> Text -> k
mkForecastKey SkuGroup txt = Sku txt
mkForecastKey (CategoryGroup _) txt = txt

unForecastKey :: ForecastGrouper k -> k -> Text
unForecastKey SkuGroup (Sku sku) = sku
unForecastKey (CategoryGroup category) name = category++":"++name
     
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
loadYearOfActualCumulSalesByWeek :: ForecastGrouper key -> Day -> (Day, Day, SqlConduit () (ForMap key UWeeklyQuantity) ())
loadYearOfActualCumulSalesByWeek grouper start = 
   let -- find first monday >= start
       -- monday = calculateDate (Chain [AddDays 6, BeginningOfWeek Monday]) start
       end = calculateDate (AddDays $ 364) start -- exactly 52 weeks
       mkVec (ForMap sku week'quantitys) = let
           v0 = V.replicate 52 0 :: UWeeklyQuantity
           va = v0 V.// week'quantitys
           
           in -- traceShow (week'quantitys, v0) $
              (sku, va)
       source = actualSalesSource grouper start end
                 .| mapC mkVec
                 .| mapC  (\(sku, quantitys) -> ForMap sku  $ V.postscanl' (+) 0 quantitys)
   in (start, end, source)

-- | load sales from stock moves between the given date (end excluded)
-- sorted by sku 
actualSalesSource :: forall key . ForecastGrouper key -> Day -> Day -> SqlConduit () (ForMap key [(Int, Quantity)]) ()
actualSalesSource grouper start end = do
   let sql = "SELECT " <> groupKey <> " AS groupKey, (MIN(TO_DAYS(tran_date)) - TO_DAYS(?))/7 AS days, -sum(qty)" :
           " FROM 0_stock_moves moves " :
           " LEFT JOIN 0_debtor_trans USING(type, trans_no, tran_date) " :
           " LEFT JOIN fames_customer_category_cache AS clearance ON (debtor_no = customer_id AND category = 'clearance') " :
           sqlJoin ?:
           " WHERE type IN ("  : (tshow $ fromEnum ST_CUSTDELIVERY) : ",": (tshow $ fromEnum ST_CUSTCREDIT) : ") " :
           " AND qty != 0" :
           " AND (clearance.value is null OR clearance.value <> 'Yes' ) " :
           " AND moves.stock_id like 'M%'" :
           -- " AND stock_id like 'ML17-FD7-NAY'" :
           -- " AND stock_id like 'ML13-AD1-IVY'" :
           " AND tran_date >= ? AND tran_date < ? " :
           " GROUP BY groupKey, YEARWEEK(tran_date,5) " :
           " order BY groupKey, YEARWEEK(tran_date,5) " :
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
       weekSource = rawQuery  (mconcat sql) $ toPersistValue start : joinParams ++ [ toPersistValue start, toPersistValue end] 
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



loadYearOfForecastCumulByWeek :: Ord key => ForecastGrouper key -> Day -> FilePath -> Handler (Map key UWeeklyQuantity)
loadYearOfForecastCumulByWeek grouper start forecastDir = do
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
  let weekProfiles = fmap expandProfileWeekly rawProfiles
      weekProfiles ::  Map Collection UWeeklyQuantity
      monthWeekly :: [UWeeklyQuantity] 
      monthWeekly = monthFractionPerWeek  (calculateDate (AddDays 364) start)
      expandProfileWeekly :: SeasonProfile -> UWeeklyQuantity
      expandProfileWeekly (SeasonProfile profile) =  let v = V.postscanl' (+) 0 $
                                                                        foldl1Ex' vadd  $ zipWith (\monthWeight weeks -> V.map (*monthWeight) weeks)
                                                                                                   profile
                                                                                                   monthWeekly
                                                     in v
      linear = V.postscanl' (+) 0 $ V.replicate 52 (1/52)
      weeklyForRow (SkuSpeedRow _ weight collection) = V.map ((*1).(*weight)) weekly where
          weekly = findWithDefault linear collection weekProfiles
                          
      skuMap = Map.fromListWith vadd [(mkKey . unSku $ ssSku row, weeklyForRow row )
                                     | row <- skuSpeed
                                     ]
  return skuMap
  
  

-- | Compute for each month its year fraction for each weeks
monthFractionPerWeek :: Day -> [UWeeklyQuantity]
monthFractionPerWeek start = let
   monthForWeek = [ (d `div` 7,  month)
                  | (d, day) <- zip [0.. 363] [start .. ]
                  , let (_,month, _) = toGregorian day
                  ]
   v0 = V.replicate 52 0
   in [ V.accum (+) v0 updates
      | month <- [1..12]
      , let updates = let ups = [ (w, 1/monthLength) | (w, m) <- monthForWeek, m == month ]
                          monthLength = fromIntegral $ length ups
                      in ups
      ]
       



vadd, vsub, vdiv, vmul  :: UWeeklyQuantity -> UWeeklyQuantity -> UWeeklyQuantity
vadd = V.zipWith (+)
vsub = V.zipWith (-)
vdiv = V.zipWith (/)
vmul = V.zipWith (*)

   
   
