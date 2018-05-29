module CategoryRule where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Types(Parser, typeMismatch)
import Text.Regex.TDFA ((=~))
import qualified Text.Regex as Rg

data PriceRanger = PriceRanger (Maybe Double) (Maybe Double) String deriving Show
data RegexSub = RegexSub { rsRegex, rsReplace :: String } deriving Show
-- | 
data CategoryRule
  = SkuTransformer RegexSub
  -- | CategoryConjunction [CategoryRule] RegexSub -- regex on the result of all concatenation split by [an number]
  -- if only one elements, doesn't 
  | CategoryDisjunction [CategoryRule] -- match first categories
  | SalesPriceRanger PriceRanger
  | SourceTransformer String CategoryRule
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

      let parseRegex = (SkuTransformer <$> (RegexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key)))
                       <|> (parseDisjunction key =<< o .: "rules")
        

      source <- fmap unpackT <$> o .:? "source"
      case source of
        Just "sku" -> parseRegex 
        Just "sales_price"  -> SalesPriceRanger <$> (PriceRanger <$> (o .:? "from") <*> (o .:? "to") <*> pure (unpack key))
        -- Just other -> typeMismatch ("source " ++ other ++ " invalid" ) (Object o)
        -- Just s -> SourceTransformer s . SkuTransformer <$> (RegexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key))
        Just s -> SourceTransformer s <$> parseRegex  
        Nothing -> parseObject o
                                             
    in withText "sub string" parseSkuString v
       <|> withArray "rule list" (parseDisjunction key0) v
       <|> withObject "rule object" (parseMatcher key0) v

  
unpackT :: Text -> String
unpackT = unpack
instance ToJSON CategoryRule where
  toJSON v = toJSON ([] :: [Int])
  



-- | Computes the value of all category in the category map.
-- We can't compute the value of each category individually because
-- a category can depend on another.
computeCategories :: [(String, CategoryRule)] -> Map String String -> String -> Map String String
computeCategories rules inputMap source = let
  x = foldl' go inputMap  rules
  go iMap (category, rule) = maybe mempty (Map.singleton category) (computeCategory source iMap rule) <> iMap


  in x

computeCategory :: String -> Map String String -> CategoryRule -> Maybe String
computeCategory source inputMap rule = case rule of
      SkuTransformer rsub -> subRegex rsub source
      SalesPriceRanger ranger -> checkPriceRange 15  ranger
      SourceTransformer source subrule -> computeCategory (expandSource inputMap source) inputMap subrule
      CategoryDisjunction rules -> asum $ map (computeCategory source inputMap) rules
      -- CategoryConjunction rules' rsub -> do -- Maybe
      --   subs <- mapM apply rules'
      --   let grouped = case subs of
      --         [sub] -> sub
      --         _ -> join [ "{" ++ show i ++ "}" ++ sub
      --                   | (i, sub) <- zip [1..] subs
      --                   ]
      --   subRegex rsub (grouped)

subRegex :: RegexSub -> String -> Maybe String
subRegex (RegexSub regexS replace) s = let
  regex =Rg.mkRegex regexS
  in if isJust $ Rg.matchRegex regex s
     then Just . pack $ Rg.subRegex regex s replace
     else Nothing


  
checkPriceRange :: Double -> PriceRanger -> Maybe String
checkPriceRange price (PriceRanger from to target) =
  if maybe True (<=price) from && maybe True (>price) to 
  then Just target
  else Nothing
  
  
-- | set the current text source to the target expanded with category
expandSource :: Map String String -> String -> String
expandSource inputMap format = let
  expanded = foldl' sub format (Map.toList inputMap)
  sub s (category, value) = let regex = Rg.mkRegex $ "\\$" ++ category
                            in Rg.subRegex regex s value
  in expanded
  

  



  

  



  

  
