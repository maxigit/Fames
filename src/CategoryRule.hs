module CategoryRule where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types(Parser)

data PriceRanger = PriceRanger (Maybe Double) (Maybe Double) String deriving Show
data RegexSub = RegexSub { rsRegex, rsReplace :: String } deriving Show
-- | 
data CategoryRule
  = SkuTransformer RegexSub
  | CategoryConjunction [CategoryRule] RegexSub -- regex on the result of all concatenation split by [an number]
  -- if only one elements, doesn't 
  | CategoryDisjunction [CategoryRule] -- match first categories
  | SalesPriceRanger PriceRanger
  -- | FACategory RegexSub
  deriving Show

regexSub regex replace = RegexSub (regex ++ ".*") replace
instance FromJSON CategoryRule where
  parseJSON v = parseJSON' "" v 

parseJSON' key0 v = let
    parseSkuString t = case break (=='/') (unpack t) of
      (regex, '/':replace) -> return $ SkuTransformer (regexSub regex replace)
      (regex, "") -> return $ SkuTransformer (regexSub regex (unpack key0))
      _ -> mzero
    parseObject o = do
      let disM = [ parsePair key value
            | (key, value) <- mapToList o
            ]
      dis <- sequence disM
      return $ CategoryDisjunction dis 

    parseDisjunction key as = do
      traceShowM ("Dis", key, as)
      rules <- mapM (parseJSON' key ) (toList as)
      return $ CategoryDisjunction rules
    -- parse pair either a disjunction, we throw the name away
    -- or a sku
    parsePair :: Text -> Value -> Parser CategoryRule
    parsePair key v = withText "replace" (\t -> return $ SkuTransformer (regexSub (unpack t) (unpack key))) v
                          <|> withArray "sublist" (parseDisjunction key) v
                          <|> withObject "matcher" (parseMatcher key) v
                          <|> parseJSON' key v

    parseMatcher :: Text -> Object -> Parser CategoryRule
    parseMatcher key o = do
      traceShowM ("MATCHER", key0, key, o)
      source <- fmap unpackT <$> o .:? "source"
      case source of
        Just "sku" -> SkuTransformer <$> (RegexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key))
        Just "sales_price"  -> SalesPriceRanger <$> (PriceRanger <$> (o .:? "from") <*> (o .:? "to") <*> pure (unpack key))
        _ -> parseObject o
                                             
    in withText "sub string" parseSkuString v
       <|> withArray "rule list" (parseDisjunction key0) v
       <|> withObject "rule object" (parseMatcher key0) v

  
unpackT :: Text -> String
unpackT = unpack
instance ToJSON CategoryRule where
  toJSON v = toJSON ([] :: [Int])
  




