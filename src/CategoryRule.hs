module CategoryRule where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Types(Parser, typeMismatch)
import Text.Regex.TDFA ((=~))
import qualified Text.Regex as Rg

data PriceRanger = PriceRanger (Maybe Double) (Maybe Double) String deriving Show
data RegexSub = RegexSub { rsRegex :: Rg.Regex, rsOriginal, rsReplace :: String }
instance Show RegexSub where
  show (RegexSub _ regex replace) = "regexSub " ++ show regex ++ " " ++ replace
-- | Category rule. The phantom type is there to indicate on which the category shoulp apply
data CategoryRule a
  = SkuTransformer RegexSub
  -- | CategoryConjunction [CategoryRule] RegexSub -- regex on the result of all concatenation split by [an number]
  -- if only one elements, doesn't 
  | CategoryDisjunction [(CategoryRule a)] -- match first categories
  | SalesPriceRanger PriceRanger
  | SourceTransformer String (CategoryRule a)
  | CategoryCondition (CategoryRule a) (CategoryRule a) -- uses second category  only if the first one matches
  -- | FACategory RegexSub
  deriving Show

data CustomerCat
data ItemCat
data OrderCat
data DeliveryCat

type ItemCategoryRule = CategoryRule ItemCat
type CustomerCategoryRule = CategoryRule CustomerCat
type OrderCategoryRule = CategoryRule OrderCat
type DeliveryCategoryRule = CategoryRule DeliveryCat

data RuleInput = RuleInput
  { categoryMap :: Map String String
  , salesPrice :: Maybe Double
  }
regexSub regex replace = RegexSub (Rg.mkRegex $ regex ++ ".*") regex replace
instance FromJSON (CategoryRule a) where
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
      -- traceShowM ("Dis", key, as)
      rules <- mapM (parseJSON' key ) (toList as)
      return $ CategoryDisjunction rules
    -- parse pair either a disjunction, we throw the name away
    -- or a sku
    -- parsePair :: Text -> Value -> Parser (CategoryRule a)
    parsePair key v = withText "replace" (\t -> return $ SkuTransformer (regexSub (unpack t) (unpack key))) v
                          <|> withArray "sublist" (parseDisjunction key) v
                          <|> withObject "matcher" (parseMatcher key) v
                          <|> parseJSON' key v

    -- parseMatcher :: Text -> Object -> Parser (CategoryRule a)


    parseMatcher key o = do
      -- traceShowM ("MATCHER", key0, key, o)

      let parseRegex = (SkuTransformer <$> (regexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key)))
                       <|> (parseDisjunction key =<< o .: "rules")
        

      source <- fmap unpackT <$> o .:? "source"
      case source of
        Just "sku" -> parseRegex 
        Just "sales_price"  -> SalesPriceRanger <$> (PriceRanger <$> (o .:? "from") <*> (o .:? "to") <*> pure (unpack key))
        -- Just other -> typeMismatch ("source " ++ other ++ " invalid" ) (Object o)
        -- Just s -> SourceTransformer s . SkuTransformer <$> (RegexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key))
        --
        Just s -> SourceTransformer s <$> parseRegex  
        Nothing -> parseObject o
                                             
    -- check if contains if and then
    parseCondition key o = do
      condition <- o .: "if"
      rule <- o .: "then"
      return $ CategoryCondition condition rule
      

      
    in withText "sub string" parseSkuString v
       <|> withArray "rule list" (parseDisjunction key0) v
       <|> withObject "rule object - condition" (parseCondition key0) v
       <|> withObject "rule object" (parseMatcher key0) v

unpackT :: Text -> String
unpackT = unpack
instance ToJSON (CategoryRule a) where
  toJSON (SkuTransformer (RegexSub _ origin replace)) = object [ pack replace .= origin ]  -- origin <> "/" <> replace
  toJSON (CategoryDisjunction rules) = toJSON  rules
  toJSON (SalesPriceRanger (PriceRanger fromM toM target)) = object [pack target .= paramJ] where
    paramJ = object ["source" .= ("sales_price" :: Text), "from" .= fromM, "to" .= toM]
  toJSON (SourceTransformer source rule) = object ["source" .= source, "rules" .= rule ]
  toJSON (CategoryCondition condition rule) = object ["if" .= condition, "then" .= rule]
  



-- | Computes the value of all category in the category map.
-- We can't compute the value of each category individually because
-- a category can depend on another.
computeCategories :: Map String Rg.Regex -> [(String, (CategoryRule a))] -> RuleInput -> String -> Map String String
computeCategories catRegexCache rules input source = let
  x = foldl' go input  rules
  go i (category, rule) = i { categoryMap = maybe mempty
                                                  (Map.singleton category)
                                                  (computeCategory catRegexCache source i rule)
                                            <> categoryMap i
                            }


  in categoryMap x

computeCategory :: Map String Rg.Regex -> String -> RuleInput -> (CategoryRule a) -> Maybe String
computeCategory catRegexCache source input rule = case rule of
      SkuTransformer rsub -> subRegex rsub source
      SalesPriceRanger ranger ->  salesPrice input >>= flip checkPriceRange ranger
      SourceTransformer source subrule -> computeCategory catRegexCache (expandSource catRegexCache (categoryMap input) source) input subrule
      CategoryDisjunction rules -> asum $ map (computeCategory catRegexCache source input) rules
      CategoryCondition condition rule ->  case computeCategory catRegexCache source input condition of
                                                Nothing -> Nothing
                                                Just _ -> computeCategory catRegexCache source input rule
        

subRegex :: RegexSub -> String -> Maybe String
subRegex (RegexSub regex _ replace) s = let
  in if isJust $ Rg.matchRegex regex s
     then Just . pack $ Rg.subRegex regex s replace
     else Nothing


  
checkPriceRange :: Double -> PriceRanger -> Maybe String
checkPriceRange price (PriceRanger from to target) =
  if maybe True (<=price) from && maybe True (>price) to 
  then Just target
  else Nothing
  
  
-- | set the current text source to the target expanded with category
expandSource :: Map String Rg.Regex ->  Map String String -> String -> String
expandSource regexCache inputMap format = let
  expanded = foldl' sub format (Map.toList inputMap)
  sub s (category, value) = let regex = fromMaybe (mkCategoryRegex category) (lookup category regexCache)  -- normally everything shoulb be in the cache
                            in Rg.subRegex regex s value
  in expanded
  
-- | Regex as match in source tranform
-- "$cat1 - $cat2"" will search and replace for the value
-- of category cat1 and cat2
mkCategoryRegex category = Rg.mkRegex $ "(\\$" ++ category ++ "\\>)|(\\$\\{" ++ category ++ "\\})"

  



  

  



  

  
