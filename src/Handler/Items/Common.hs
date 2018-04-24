module Handler.Items.Common where

import Import
import Items
import Data.Text(splitOn)
import qualified Data.Map as Map 
import Text.Regex.TDFA ((=~))

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

-- * Category
-- | Return a function finding the category given a style
categoryFinder :: Handler (Text -> Maybe Text)
categoryFinder = do
  catRules <- appCategoryRules <$> getsYesod appSettings
  let rules = concatMap (Map.toList) catRules
      finder s = asum [ Just result
                      | (regex, result) <- rules
                      , unpack s =~ regex
                      ]
  return finder
