module CategoryRule where

import ClassyPrelude.Yesod hiding(replace)

import qualified Data.Map as Map
import Data.Aeson

import qualified Text.Regex as Rg
import qualified Text.Regex.Base as Rg
import qualified Text.Regex.TDFA.Common as Rg

data PriceRanger = PriceRanger (Maybe Double) (Maybe Double) String deriving Show
data RegexSub = RegexSub { rsRegex :: Rg.Regex, rsOriginal, rsReplace :: String }
instance Show RegexSub where
  show (RegexSub _ regex replace) = "regexSub " ++ show regex ++ " " ++ replace
-- | Category rule0. The phantom type is there to indicate on which the category shoulp apply
data CategoryRule a
  = SkuTransformer RegexSub
  -- | CategoryConjunction [CategoryRule] RegexSub -- regex on the result of all concatenation split by [an number]
  -- if only one elements, doesn't 
  | CategoryDisjunction [(CategoryRule a)] -- match first categories
  | SalesPriceRanger PriceRanger
  | SourceTransformer String (CategoryRule a)
  | CategoryCondition (CategoryRule a) (CategoryRule a) -- uses second category  only if the first one matches
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
regexSub :: String -> String -> RegexSub
regexSub regex replace =
  RegexSub ( Rg.makeRegexOpts
            Rg.CompOption{..}
            Rg.defaultExecOpt
           $ regex ++ ".*"
           ) regex replace where
  caseSensitive = True
  multiline = False
  rightAssoc = True -- default
  newSyntax = True
  lastStarGreedy = True -- Faster
instance FromJSON (CategoryRule a) where
  parseJSON v = parseJSON' "" v 

parseJSON' :: Text -> Value -> _Parser (CategoryRule a)
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
    parsePair key value = withText "replace" (\t -> return $ SkuTransformer (regexSub (unpack t) (unpack key))) value
                          <|> withArray "sublist" (parseDisjunction key) value
                          <|> withObject "matcher" (parseMatcher key) value
                          <|> parseJSON' key value

    -- parseMatcher :: Text -> Object -> Parser (CategoryRule a)


    parseMatcher key o = do
      -- traceShowM ("MATCHER", key0, key, o)

      let parseRegex = (SkuTransformer <$> (regexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key)))
                       <|> (parseDisjunction key =<< o .: "rules")
        

      source0 <- fmap unpackT <$> o .:? "source"
      case source0 of
        Just "sku" -> parseRegex 
        Just "sales_price"  -> SalesPriceRanger <$> (PriceRanger <$> (o .:? "from") <*> (o .:? "to") <*> pure (unpack key))
        -- Just other -> typeMismatch ("source " ++ other ++ " invalid" ) (Object o)
        -- Just s -> SourceTransformer s . SkuTransformer <$> (RegexSub <$> (unpackT <$> o .: "match")  <*> pure (unpack key))
        --
        Just s -> SourceTransformer s <$> parseRegex  
        Nothing -> parseObject o
                                             
    -- check if contains if and then
    parseCondition _key o = do
      condition <- o .: "if"
      rule0 <- o .: "then"
      return $ CategoryCondition condition rule0
      

      
    in withText "sub string" parseSkuString v
       <|> withArray "rule list" (parseDisjunction key0) v
       <|> withObject "rule object - condition" (parseCondition key0) v
       <|> withObject ("rule object" <> show v) (parseMatcher key0) v


unpackT :: Text -> String
unpackT = unpack
instance ToJSON (CategoryRule a) where
  toJSON (SkuTransformer (RegexSub _ origin replace)) =
    if '/' `elem` origin  || '(' `notElem` origin 
    --  ^                         ^
    --  |                         |
    --  |                         +---   heuristic if search & replace involded prefer / notation 
    --  +-----------------------------  / would  interfer with / notation
    then object [ pack replace .= origin ]  -- origin <> "/" <> replace
    else   toJSON $ origin <> "/" <> replace
  toJSON (CategoryDisjunction rules) = let
    -- group by replace string if possible toJSON  rules
    go (SkuTransformer (RegexSub _ origin replace)) = Right (replace, origin)
    go rule = Left rule
    same (Right (a,_)) (Right (b,_)) = a == b
    same _ _ = False
    groups = groupBy same $ map go rules
    groupToJSON g = case partitionEithers g of
                      ([], [(replace,origin)]) -> [toJSON . SkuTransformer $ regexSub origin replace]
                      ([], r'origins@((replace,_):_)) -> [object [ pack replace .=  map snd r'origins]]
                      (others,_) -> map toJSON others
    in  toJSON (concatMap groupToJSON groups :: [Value])

  toJSON (SalesPriceRanger (PriceRanger fromM toM target)) = object [pack target .= paramJ] where
    paramJ = object ["source" .= ("sales_price" :: Text), "from" .= fromM, "to" .= toM]
  -- toJSON (SourceTransformer source0 (SkuTransformer(RegexSub _ origin _))) = object ["source" .= source0, "match" .= origin  ]
  toJSON (SourceTransformer source0 rule0) = object ["source" .= source0, "rules" .= rule0 ]
  toJSON (CategoryCondition condition rule0) = object ["if" .= fromCondition condition, "then" .= rule0] where
    fromCondition (SourceTransformer source0 (SkuTransformer(RegexSub _ origin _))) = object ["source" .= source0, "match" .= origin  ]
    fromCondition (CategoryDisjunction [rule]) = fromCondition rule
    fromCondition rule = toJSON rule
  




-- | Computes the value of all category in the category map.
-- We can't compute the value of each category individually because
-- a category can depend on another.
computeCategories :: Map String Rg.Regex -> [(String, (CategoryRule a))] -> RuleInput -> String -> Map String String
computeCategories catRegexCache rules input source0 = let
  x = foldl' go input  rules
  go i (category, rule0) = i { categoryMap = maybe mempty
                                                  (Map.singleton category)
                                                  (computeCategory catRegexCache source0 i rule0)
                                            <> categoryMap i
                            }


  in categoryMap x

computeCategory :: Map String Rg.Regex -> String -> RuleInput -> (CategoryRule a) -> Maybe String
computeCategory catRegexCache source0 input rule = case rule of
      SkuTransformer rsub -> subRegex rsub source0
      SalesPriceRanger ranger ->  salesPrice input >>= flip checkPriceRange ranger
      SourceTransformer source subrule -> computeCategory catRegexCache (expandSource catRegexCache (categoryMap input) source) input subrule
      CategoryDisjunction rules -> asum $ map (computeCategory catRegexCache source0 input) rules
      CategoryCondition condition rule0 ->  case computeCategory catRegexCache source0 input condition of
                                                Nothing -> Nothing
                                                Just _ -> computeCategory catRegexCache source0 input rule0
        

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
  
  
-- | set the current text source0 to the target expanded with category
expandSource :: Map String Rg.Regex ->  Map String String -> String -> String
expandSource regexCache inputMap format = let
  expanded = foldl' sub format (Map.toList inputMap)
  sub s (category, value) = let regex = fromMaybe (mkCategoryRegex category) (lookup category regexCache)  -- normally everything shoulb be in the cache
                            in Rg.subRegex regex s value
  in expanded
  
-- | Regex as match in source0 tranform
-- "$cat1 - $cat2"" will search and replace for the value
-- of category cat1 and cat2
mkCategoryRegex :: String -> Rg.Regex
mkCategoryRegex category = Rg.mkRegex $ "(\\$" ++ category ++ "\\>)|(\\$\\{" ++ category ++ "\\})"

  



  

  



  

  
