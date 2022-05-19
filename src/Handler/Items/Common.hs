module Handler.Items.Common where

import Import
import Items
import Data.Text(splitOn)
import Handler.Items.Category.Cache

-- * Style names conversion 
-- Those function are in handler and not in app
-- because  ultimately they should depend on the configuration file

-- construct the function depending on the category setting
skuToStyleVarH :: Handler (Text -> (Text, Text))
skuToStyleVarH = do
  let f sku = (take 8 sku, drop 9 sku)
  return f



styleVarToSku :: Text -> Text -> Text
styleVarToSku style "" = style
styleVarToSku style var = style <> "-" <> var

-- | Generate a finder Sku or Style -> duty percent
dutyForH :: Handler (Text -> Maybe Double)
dutyForH = do
  styleFn <- (fst.) <$> skuToStyleVarH
  refreshCategoryCache False Nothing
  dutyS <- runDB $ selectList [ItemCategoryCategory ==. "duty"] []
  let styleMap = mapFromList [ (styleFn itemCategoryStockId, duty )
                           | (Entity _ ItemCategory{..}) <- dutyS
                           , Just duty <- [readMay itemCategoryValue]
                           ] :: Map Text Double
  return $ flip lookup styleMap . styleFn



-- ** Sku form info 

iiSku :: ItemInfo a -> Text
iiSku (ItemInfo style var _ ) = styleVarToSku style var

-- | Split a variation name to variations
-- ex: A/B -> [A,B]
variationToVars :: Text -> [Text]
variationToVars = splitOn "/"

-- | Inverse of variationToVars
varsToVariation :: [Text] -> Text
varsToVariation = intercalate "/" 



-- ** Date 
-- Year of the given date
year :: Day -> Integer
year day = y where (y,_,_) = toGregorian day


-- Year as if next year was starting tomorrow
slidingYear :: Day -> Day -> Integer
slidingYear today day = let
  (y,m,d) = toGregorian day
  (y0,m0,d0) = toGregorian today
  before = if (m,d) < (m0,d0) then 0 else 1
  in y - y0 + before

-- | showing sliding year give a string
-- which is not sort frienly (as negative number comes before 0)
slidingYearShow :: Day -> Day -> String
slidingYearShow today day =  let
  y = year today
  s = slidingYear today day
  y' = y + s
  in show (y'-1) <> "-" <> show y'

