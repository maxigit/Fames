{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module Handler.Items.Reports.Forecast where

import Import
import Items.Types
import Measure as M
import qualified Data.Csv as Csv
import Handler.CsvUtils
import Handler.Items.Category.Cache
import Handler.Items.Common(StockFilter, stockFilterToSqlWithColumn, stockFilterToSql)
import Handler.Items.Forecast.Model
import Items.Internal
import qualified Data.IntMap as IntMap
import System.FilePath.Glob (glob)
import System.FilePath (takeBaseName)
import System.Directory(listDirectory, getModificationTime, doesDirectoryExist)
import FA as FA hiding (unUserKey)
import Control.Monad.Fail (MonadFail(..))
import GL.Utils
-- import GL.Payroll.Settings(DayOfWeek(..))
import Database.Persist.MySQL -- (BackendKey(SqlBackendKey))
import qualified Data.Conduit.List as CL
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty(..))
import Util.ForConduit
import Util.Cache(cacheMinute)
import qualified Data.Vector.Generic.Sized as V -- not Generic as Generics but generic interface over all types of vector
import qualified Data.Map as Map

-- * Profiles 
-- | Read a map of season profiles from a valid csv
-- collection,month,weight
data CollectionProfileRow = CollectionProfileRow
 { cpCollection :: Collection
 , cpMonth :: Int
 , cpWeight :: Years
 } deriving Show
instance Csv.FromNamedRecord CollectionProfileRow where
  parseNamedRecord m = do
    collection <- fmap Collection ( m Csv..: "collection")
    weight <- m Csv..: "weight"
    month' <- m Csv..: "month"
    month <- parseMonth month'
    return $ CollectionProfileRow collection month (Measure weight)
instance Csv.ToNamedRecord CollectionProfileRow where
  toNamedRecord CollectionProfileRow{..} = let
      (Collection collection) = cpCollection
      (Measure weight) = cpWeight
      in Csv.namedRecord [ "collection" Csv..= collection
                         , "month" Csv..= unparseMonth cpMonth
                         , "weight" Csv..= weight
                         ]
instance Csv.DefaultOrdered CollectionProfileRow where
   headerOrder _ = Csv.header [ "collection", "month", "weight" ]
    
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
    
unparseMonth :: Int -> Text
unparseMonth m = indexEx [ "Jan", "Feb", "Mar"
                         , "Apr", "May", "Jun"
                         , "Jul", "Aug", "Sep"
                         , "Oct", "Nov", "Dec"
                         ]   
                         (m-1) 
forecastPathToDay :: FilePath -> Maybe Day
forecastPathToDay = readMay . take 10 . takeBaseName
        
readProfiles :: FilePath -> IO (Map Collection SeasonProfile)
readProfiles path = do
  content <- readFile path
  let Right cols = parseSpreadsheet mempty Nothing content
      monthMap (CollectionProfileRow _ month weight) = IntMap.singleton month weight
      grouped = groupAsMap cpCollection monthMap cols
  return $ fmap seasonProfileFromMap grouped


-- * Sku Speed 
-- | Row coming from a sku speed file.
data SkuSpeedRow = SkuSpeedRow
  { ssSku :: Sku
  , ssWeight :: YearlyQuantity
  , ssCollection :: Collection
  , ssComment :: Text
  }deriving (Show)
instance Csv.FromNamedRecord SkuSpeedRow where
  parseNamedRecord m = SkuSpeedRow  <$> fmap Sku (m Csv..: "stock_id")
                                    <*> (Measure <$> m Csv..: "eQty")
                                    <*> fmap Collection (m Csv..: "collection")
                                    <*> pure "TODO"
instance Csv.ToNamedRecord SkuSpeedRow where
  toNamedRecord SkuSpeedRow{..} = let 
      (Collection collection) = ssCollection
      (Measure weight) = ssWeight
      in Csv.namedRecord [ "stock_id" Csv..= unSku ssSku
                         , "eQty" Csv..= weight
                         , "collection" Csv..= collection
                         , "comment" Csv..= ssComment
                         ]
  
instance Csv.DefaultOrdered SkuSpeedRow where
  headerOrder _ = Csv.header ["stock_id", "eQty", "collection", "comment" ]

                  
-- | Load sku speed from a csv
loadSkuSpeed :: FilePath -> IO  [SkuSpeedRow]
loadSkuSpeed filepath = do
  content <- readFile filepath
  case parseSpreadsheet mempty Nothing content of
    Left err -> error $ show err
    Right rows -> return rows

-- | load csv forecast or evaluate model if needed
loadSkuSpeedFromDir :: FilePath -> Handler ([SkuSpeedRow], Map Collection SeasonProfile)
-- loadSkuSpeedFromDir forecastDir = cache0 False (cacheMinute 15) ("sku-speed" </> forecastDir)  $ do
loadSkuSpeedFromDir forecastDir = do
  mtime <- liftIO $ lastModifiedTime forecastDir
  cache0 False (cacheMinute 15) ("forecast/sku-speed", mtime, forecastDir)  do
         skuFiles <- skuFilesFromDir forecastDir
         case skuFiles of
            (_:_) ->  do -- load csv by default in case we cached the hs result
                      rawProfiles <- liftIO $ readProfiles $ forecastDir  </> "collection_profiles.csv"
                      skuSpeeds <- liftIO $ mapM (loadSkuSpeed . (forecastDir </> )) skuFiles
                      return (concat skuSpeeds, rawProfiles)
            [] | Just forecastDay <- forecastPathToDay forecastDir -> do -- try loading model
                      speedE <- estimateSkuSpeedFromDir forecastDay forecastDir 
                      case speedE of
                         Left err -> error $ "Can't find sku speed files or hs model in directory " <> show forecastDir <> "\n" <> unpack err
                         Right speed -> do
                               (skuToCollection, profiles) <- estimateCollectionProfile forecastDay forecastDir
                               return (toList $ fmap (\(sku, qty, comment) -> SkuSpeedRow sku qty (skuToCollection sku)comment )speed, profiles)
                              
            _ -> error $ "Can't find sku speed files." <> show forecastDir


lastModifiedTime :: FilePath -> IO UTCTime
lastModifiedTime path = do
   isDirectory <- doesDirectoryExist path
   ts <- if isDirectory
         then listDirectory path >>= mapM (lastModifiedTime . (path</>))
         else fmap pure ( getModificationTime path)
   case ts of
      [] -> getModificationTime path
      _ -> return $ maximumEx ts

skuFilesFromDir :: FilePath -> Handler [FilePath]
skuFilesFromDir forecastDir = liftIO $ glob (unpack $ forecastDir </> "*sku_forecast.csv" )
  
   
-- | Generate fake transactions corresponding to forecast sales
loadItemForecast ::  Maybe InOutward -> FilePath -> (Map Sku ItemInitialInfo) -> Day -> Day -> Handler [(TranKey, TranQP)]
loadItemForecast io forecastDir infoMap start end = do
  settings <- getsYesod appSettings
  catFinder <- categoryFinderCached (appForecastCollectionCategory settings)
  
  (skuSpeeds, profiles) <- loadSkuSpeedFromDir forecastDir
  let profile (Sku sku) = (catFinder  (FA.StockMasterKey sku) >>= (\col ->  lookup (Collection col) profiles)
                          ) <|> Just flatProfile
      flatProfile = seasonProfile []
  return $ concatMap (skuSpeedRowToTransInfo infoMap profile start end io) skuSpeeds

skuSpeedRowToTransInfo :: Map Sku ItemInitialInfo
                       -> (Sku -> Maybe SeasonProfile)
                       -> Day
                       -> Day
                       -> Maybe InOutward
                       -> SkuSpeedRow
                       -> [(TranKey, TranQP)]
skuSpeedRowToTransInfo infoMap profileFor start end iom (SkuSpeedRow sku speed _ _) =
  let io = fromMaybe Outward iom  --   ^ like sales
      extra = maybe [] ioToQPType iom
  in case (profileFor sku, lookup sku infoMap) of
    (Just profile, Just info) -> do -- []
      (day0, weight) <- weightsForRange profile start end
      guard (weight > 1e-6)
      let key = TranKey day0
                    Nothing
                    (Just sku)
                    Nothing -- style
                    Nothing -- var
                    mempty
                    mempty
                    (case io of
                       Inward -> ST_PURCHORDER
                       Outward -> ST_SALESINVOICE)
                    Nothing Nothing mempty
                    0

          qp = mkQPrice io (measured $ weight ^* speed) (fromMaybe 0 $ iiSalesPrice info)
          tqp = tranQP' extra QPSalesForecast qp
      return (key, tqp)
    _ -> []
    

-- * Forecast error

-- | Load actual sales for a whole year 
loadYearOfActualCumulSalesByWeek :: ForecastGrouper key -> StockFilter -> Day -> Maybe FA.SalesTypeId -> (Day, Day, SqlConduit () (ForMap key (U53Weeks QuantityD)) ())
loadYearOfActualCumulSalesByWeek grouper stockFilter start priceIdM = 
   let -- find first monday >= start
       end = calculateDate (Chain [ AddYears 1, AddDays $ -1 ]) start
       mkVec (ForMap sku week'quantitys) = let
           va = 0 `V.unsafeUpd` week'quantitys
           
           in -- traceShow (week'quantitys, v0) $
              (sku, va)
       source = actualSalesSource grouper stockFilter start end priceIdM
                 .| mapC mkVec
                 .| mapC  (\(sku, quantitys) -> ForMap sku  $ V.postscanl' (+) 0 quantitys)
   in (start, end, source)

-- | load sales from stock moves between the given date (end excluded)
-- sorted by sku 
actualSalesSource :: forall key . ForecastGrouper key -> StockFilter -> Day -> Day -> Maybe FA.SalesTypeId -> SqlConduit () (ForMap key [(Int, QuantityD)]) ()
actualSalesSource grouper stockFilter start end priceListIdM = do
   let (stockJoinM, stockWhereM, stockParams) = stockFilterToSqlWithColumn "moves.stock_id" stockFilter
   let sql = "SELECT " <> groupKey <> " AS groupKey, DATEDIFF(tran_date,?) DIV 7 AS days, " : sales : --  -sum(qty)" :
           " FROM 0_stock_moves moves " :
           " LEFT JOIN 0_debtor_trans USING(type, trans_no, tran_date) " :
           " LEFT JOIN fames_customer_category_cache AS clearance ON (debtor_no = customer_id AND category = 'clearance') " :
           sqlJoin ?:
           stockJoinM ?:
           priceListJoinM ?:
           " WHERE type IN ("  : (tshow $ fromEnum ST_CUSTDELIVERY) : ",": (tshow $ fromEnum ST_CUSTCREDIT) : ") " :
           (fmap (" AND " <>) stockWhereM) ?:
           " AND qty != 0" :
           " AND (clearance.value is null OR clearance.value <> 'Yes' ) " :
           -- " AND moves.stock_id like 'M%'" :
           " AND moves.stock_id rlike '^[MC]'" :
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
       (sales, priceListJoinM) = case priceListIdM of
           Nothing -> ("-sum(qty)" , Nothing)
           Just pId -> ("-sum(qty*prices.price)", Just $ " JOIN 0_prices AS prices ON (prices.stock_id = moves.stock_id AND sales_type_id = " <> tshow (unSalesTypeKey pId ) <> " AND curr_abrev = 'GBP' )")

       weekSource = rawQuery  (mconcat sql) $ toPersistValue start : joinParams ++ stockParams ++ [ toPersistValue start, toPersistValue end] 
       --                                     ^^^^^^^^^^^^^^^^^^^
       --                                        |
       --                                        +---- selecting week number
       myCoerce :: [PersistValue] -> (Text, (Int, QuantityD))
       myCoerce vs = case rawSqlProcessRow vs  of
                          Left e -> error $ unpack e
                          Right v -> coerce (v :: (Single Text, (Single Int, Single QuantityD)))
       run :: NonEmpty (Text, (Int, QuantityD)) -> ForMap key [(Int, QuantityD)]
       run nonEmpty = let (sku :|  _, week'quantitys) = unzip nonEmpty
                      in ForMap (mkForecastKey grouper sku) (toList week'quantitys)
                 
   weekSource .| mapC myCoerce
              .| CL.groupOn fst
              .| mapC run



loadYearOfForecastCumulByWeek :: Ord key => ForecastGrouper key -> StockFilter -> Maybe SalesTypeId -> Day -> FilePath -> Handler (Map key (U53Weeks (Quantity, Amount)))
loadYearOfForecastCumulByWeek grouper stockFilter priceListIdM start forecastDir = do
  -- load forecast from files
  (skuSpeed, rawProfiles) <- loadSkuSpeedFromDir forecastDir
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
  keepSkuWithPrice <- case stockFilterToSql stockFilter of
               -- (Nothing, Nothing, _) | Nothing <- priceListIdM -> return $ const $ Just 1
               (stockJoinM, stockWhereM, params) -> do
                   let sql = case priceListIdM of 
                              Nothing -> "SELECT stock_id, 1 FROM 0_stock_master " <> fromMaybe "" stockJoinM
                                                                                <> " WHERE "
                                                                                <> fromMaybe "1" stockWhereM
                              Just pId  -> "SELECT stock_id, price FROM 0_prices " <> fromMaybe "" stockJoinM
                                                                    <> " WHERE "
                                                                    <> fromMaybe "1" stockWhereM
                                                                    <> " AND  curr_abrev = 'GBP'"
                                                                    <> " AND  sales_type_id = " <> tshow (unSalesTypeKey pId)
                   sku'prices :: [(Single Text, Single Double)] <- runDB $ rawSql sql params
                   let skuPriceMap = mapFromList $ map (\(Single sku, Single price) -> (Sku sku, Measure price)) sku'prices :: Map Sku Items.Types.Price
                   return $ \(sku) -> sku `lookup` skuPriceMap
                        
  let weekProfiles = fmap expandProfileWeekly rawProfiles
      weekProfiles ::  Map Collection (U53Weeks Years)
      monthWeekly :: [U53Weeks Scalar] 
      monthWeekly = monthFractionPerWeek  (calculateDate (Chain [ AddYears 1,  AddDays (-1)]) start)
      expandProfileWeekly :: SeasonProfile -> U53Weeks Years
      expandProfileWeekly (SeasonProfile profile) = 
            let v = V.postscanl' (+)
                                 0
                                 $ foldl1Ex' (+)
                                 $ zipWith (\monthWeight weeks -> V.map (^* monthWeight) weeks)
                                           profile
                                           monthWeekly
            in v
      linear = V.map (min 1) $ V.postscanl' (+) 0 $ V.replicate (1/52)  
      weeklyForRow (SkuSpeedRow _ weight collection _) price = V.map (\x -> let xweight = x^* weight
                                                                          in (xweight, xweight ^* price)
                                                                   ) weekly where
          weekly = findWithDefault linear collection weekProfiles
                          
      skuMap = Map.fromListWith (\u v -> V.zipWith (\(x,y) (x',y') -> (x+x', (y+y'))) u v) [(mkKey . unSku $ ssSku row, weeklyForRow row price)
                                     | row <- skuSpeed
                                     , price <- toList $  keepSkuWithPrice (ssSku row)
                                     ]
  return skuMap
  
  

-- | Compute for each month its year fraction for each weeks
monthFractionPerWeek :: Day -> [U53Weeks Scalar]
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
       




   
   
