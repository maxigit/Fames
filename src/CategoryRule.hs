module CategoryRule where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import Data.Aeson

data PriceRanger = PriceRanger (Maybe Double) (Maybe Double) Text -- 
data RegexSub = RegexSub { rsRegex, rsReplace :: String } deriving Show
-- | 
data CategoryRule
  = SkuTransformer RegexSub
  | CategoryConjunction [CategoryRule] RegexSub -- regex on the result of all concatenation split by [an number]
  -- if only one elements, doesn't 
  | CategoryDisjunction [CategoryRule] -- match first categories
  -- | FACategory RegexSub
  deriving Show

regexSub regex replace = RegexSub (regex ++ ".*") replace
instance FromJSON CategoryRule where
  parseJSON v = let
    expected = "CategoryRule"
    parseSkuString t = case break (=='/') (unpack t) of
      (regex, '/':replace) -> return $ SkuTransformer (regexSub regex replace)
      _ -> mzero
    parseObject o = do
      let disM = [ parsePair key value
            | (key, value) <- mapToList o
            ]
      dis <- sequence disM
      return $ CategoryDisjunction dis 

    parseDisjunction as = do
      rules <- mapM parseJSON (toList as)
      return $ CategoryDisjunction rules
    -- parse pair either a disjunction, we throw the name away
    -- or a sku
    parsePair key v = withText "replace" (\t -> return $ SkuTransformer (regexSub (unpack key) (unpack t))) v
                          <|> withArray "sublist" parseDisjunction v
                                             
    in withText expected parseSkuString v
       <|> withArray expected parseDisjunction v
       <|> withObject expected parseObject v

  
instance ToJSON CategoryRule where
  toJSON v = toJSON ([] :: [Int])
  




