module Handler.Items.Forecast.Csv where

import Import
import Items.Types
import Measure as M
import qualified Data.Csv as Csv
import Handler.CsvUtils
import qualified Data.IntMap as IntMap
import Control.Monad.Fail (MonadFail(..))
import Data.Coerce (coerce)

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

readProfiles :: FilePath -> IO (Map Collection SeasonProfile)
readProfiles path = do
  content <- readFile path
  let Right cols = parseSpreadsheetSimple content
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
  case parseSpreadsheetSimple content of
    Left err -> error $ show err
    Right rows -> return rows


-- * Simple Forecast
-- subset of SkuSpeedRow
newtype SkuQtyComment  = SkuQtyComment (Sku, Quantity, Text) deriving Show
instance Csv.FromNamedRecord SkuQtyComment where
   parseNamedRecord m = do
       sku <- m Csv..: "stock_id" <|> m Csv..: "sku"
       qty <- m Csv..: "eQty" <|> m Csv..: "quantity" <|> m Csv..: "qty"
       comment <- m Csv..: "comment" <|> return ""
       return $ SkuQtyComment (Sku sku, Measure qty, comment)
instance Csv.ToNamedRecord SkuQtyComment where
  toNamedRecord (SkuQtyComment (Sku sku, Measure qty, comment)) =
     Csv.namedRecord [ "stock_id" Csv..= sku
                     , "eQty" Csv..= qty
                     , "comment" Csv..= comment
                     ]
instance Csv.DefaultOrdered SkuQtyComment where
  headerOrder _ = Csv.header ["stock_id", "eQty", "comment" ]

readForecast :: FilePath -> IO (Vector (Sku, Quantity, Text))
readForecast filepath = do
  content <- readFile filepath
  case parseSpreadsheetSimpleV @SkuQtyComment content of
       Left err -> error $ show err
       Right v -> return $ coerce v


