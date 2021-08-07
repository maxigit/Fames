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

-- * Profiles
-- | Read a map of season profiles from a valid csv
-- collection,month,weight
data CollectionProfileRow = CollectionProfileRow
 { cpCollection :: Text
 , cpMonth :: Int
 , cpWeight :: Double
 } deriving Show
instance Csv.FromNamedRecord CollectionProfileRow where
  parseNamedRecord m = do
    collection <- m Csv..: "collection"
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
    
    
readProfiles :: FilePath -> IO (Map Text SeasonProfile)
readProfiles path = do
  content <- readFile path
  let Right cols = parseSpreadsheet mempty Nothing content
      monthMap (CollectionProfileRow _ month weight) = IntMap.singleton month weight
      grouped = groupAsMap cpCollection monthMap cols
  return $ fmap seasonProfileFromMap grouped


-- * Sku Speed
-- | Row coming from a sku speed file.
data SkuSpeedRow = SkuSpeedRow
  { ssSku :: Text
  , ssWeight :: Double
  }deriving (Show)
instance Csv.FromNamedRecord SkuSpeedRow where
  parseNamedRecord m = SkuSpeedRow  <$> m Csv..: "stock_id"
                                       <*> m Csv..: "eQty"

                  
-- | Load sku speed from a csv
loadSkuSpeed :: FilePath -> IO  [SkuSpeedRow]
loadSkuSpeed filepath = do
  content <- readFile filepath
  case parseSpreadsheet mempty Nothing content of
    Left err -> error $ show err
    Right rows -> return rows

-- | Generate fake transactions corresponding to forecast sales
loadItemForecast ::  Maybe InOutward -> FilePath -> (Map Text ItemInitialInfo) -> Day -> Day -> Handler [(TranKey, TranQP)]
loadItemForecast io forecastDir infoMap start end = do
  settings <- getsYesod appSettings
  catFinder <- categoryFinderCached (appForecastCollectionCategory settings)
  profiles <- liftIO $ readProfiles (forecastDir </> "collection_profiles.csv")
  let profile sku = (catFinder  (FA.StockMasterKey sku) >>= (`lookup` profiles) ) <|> Just flatProfile
      flatProfile = seasonProfile []
  skuFiles <- liftIO $ glob (unpack $ forecastDir </> "*sku_forecast.csv" )
  when (null skuFiles) $ do
     setError "Can't find any sku speed files. Please check with your administrator."

  skuSpeedMap <- liftIO $ mapM loadSkuSpeed skuFiles
  let skuSpeeds = concat skuSpeedMap 
  return $ concatMap (skuSpeedRowToTransInfo infoMap profile start end io) skuSpeeds

skuSpeedRowToTransInfo :: Map Text ItemInitialInfo
                       -> (Text -> Maybe SeasonProfile)
                       -> Day
                       -> Day
                       -> Maybe InOutward
                       -> SkuSpeedRow
                       -> [(TranKey, TranQP)]
skuSpeedRowToTransInfo infoMap profileFor start end iom (SkuSpeedRow sku speed) =
  let io = fromMaybe Outward iom  -- ^ like sales
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
                       Outward -> ST_SALESINVOICE
                    )
                    Nothing Nothing mempty

          qp = mkQPrice io (weight * speed) (fromMaybe 0 $ iiSalesPrice info)
          tqp = tranQP' extra QPSalesForecast qp
      return (key, tqp)
    _ -> []
    


  

   
