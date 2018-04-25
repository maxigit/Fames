module Handler.Items.Common where

import Import
import Items
import Data.Text(splitOn)
import qualified Data.Map as Map 
import Data.Time.Calendar
import qualified FA as FA

-- * Style names conversion
-- Those function are in handler and not in app
-- because  ultimately they should depend on the configuration file

skuToStyleVar :: Text -> (Text, Text)
skuToStyleVar sku = (style, var) where
  style = take 8 sku
  var = drop 9 sku

styleVarToSku :: Text -> Text -> Text
styleVarToSku style "" = style
styleVarToSku style var = style <> "-" <> var
  

-- ** Sku form info

iiSku (ItemInfo style var _ ) = styleVarToSku style var

-- | Split a variation name to variations
-- ex: A/B -> [A,B]
variationToVars :: Text -> [Text]
variationToVars var = splitOn "/" var

-- | Inverse of variationToVars
varsToVariation :: [Text] -> Text
varsToVariation vars = intercalate "/" vars



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

