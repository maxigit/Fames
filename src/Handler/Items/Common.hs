module Handler.Items.Common where

import Import
import Items
import Data.Text(splitOn)
import qualified FA
import Handler.Items.Category.Cache

-- * Style names conversion 
-- Those function are in handler and not in app
-- because  ultimately they should depend on the configuration file

-- construct the function depending on the category setting
skuToStyleVarH :: Handler (Sku -> (Style, Var))
skuToStyleVarH = do
  skip <- appSkipStyleCategory <$> getsYesod appSettings
  [styleFn, varFn] <- if skip 
                      then return [const Nothing , const Nothing ] 
                      else mapM categoryFinderCached ["style", "colour"]
  catRulesMap <- mconcat <$> appCategoryRules <$> getsYesod appSettings
  -- check style and colour categories exists
  let style = "style"
      var = "colour"
      ruleInput  = RuleInput mempty Nothing
      computeCat rule def sku = maybe def pack $ computeCategory mempty (unpack sku) ruleInput rule
  styleFromRule <- 
    case lookup style catRulesMap of
      Nothing -> setWarning "Style category not set. Please contact your Administrator."  >> return id
      Just rule -> return $ \sku -> computeCat rule sku sku
  colourFromRule <-
    case lookup var catRulesMap of
      Nothing -> setWarning "Colour category not set. Please contact your Administrator."                 >> return (const "")
      Just rule -> return $ computeCat rule ""
  return $ \(Sku sku) -> ( Style $ fromMaybe (styleFromRule sku) $ styleFn (FA.StockMasterKey sku)
                   , Var $ fromMaybe (colourFromRule sku) . varFn $ FA.StockMasterKey sku
                   )



styleVarToSku :: Style -> Var -> Sku
styleVarToSku (Style style) (Var "") = Sku style
styleVarToSku (Style style) (Var var) = Sku $ style <> "-" <> var

-- | Generate a finder Sku or Style -> duty percent
dutyForH :: Handler (Sku -> Maybe Double)
dutyForH = do
  styleFn <- (fst.) <$> skuToStyleVarH
  refreshCategoryCache False Nothing
  dutyS <- runDB $ selectList [ItemCategoryCategory ==. "duty"] []
  let styleMap = mapFromList [ (styleFn (Sku itemCategoryStockId), duty )
                             | (Entity _ ItemCategory{..}) <- dutyS
                             , Just duty <- [readMay itemCategoryValue]
                             ] :: Map Style Double
  return $ flip lookup styleMap . styleFn



-- ** Sku form info 

iiSku :: ItemInfo a -> Sku
iiSku (ItemInfo style var _ ) = styleVarToSku style var

-- | Split a variation name to variations
-- ex: A/B -> [A,B]
variationToVars :: Var -> [Var]
variationToVars = map Var . splitOn "/" . unVar

-- | Inverse of variationToVars
varsToVariation :: [Var] -> Var
varsToVariation = Var . intercalate "/" . map unVar



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

-- * Filtering
data StockFilter = StockFilter { sfSku :: Maybe FilterExpression
                               , sfCategory :: Maybe (Text, FilterExpression)
                               --                    ^^^^^  ^^^^^^^^^^^^^^^^
                               --                      |           |
                               --                      |           +--- value filter 
                               --                      |
                               --                      +---------------  category
                               }

stockFilterToSql :: StockFilter -> (Maybe Text, Maybe Text, [PersistValue ])
--                                  ^^^^              ^^^^  ^^^^^^^^^^^^^
--                                    |                 |        |
--                                    |                 |        +-- parameters
--                                    |                 +----------- where clause
--                                    +----------------------------- join
--                                  
stockFilterToSql = stockFilterToSqlWithColumn "stock_id"
stockFilterToSqlWithColumn :: Text -> StockFilter -> (Maybe Text, Maybe Text, [PersistValue ])
stockFilterToSqlWithColumn stockColumn StockFilter{..} = ( join
                                   , whereM
                                   , concat params
                                   ) where
   join = case sfCategory of
            Just _ -> Just " JOIN fames_item_category_cache AS filtered_category USING (stock_id) "
            Nothing -> Nothing
   (wheres, params) = unzip
                    $ catMaybes [ flip fmap sfSku (filterEKeyword  stockColumn)
                               , flip fmap  sfCategory \(cat, e) -> let (sql, params) = filterEKeyword "filtered_category.value" e
                                                                   in ("filtered_category.category = ? AND " <> sql
                                                                      , toPersistValue cat : params
                                                                      )
                               ]
   whereM = case wheres of 
                [] -> Nothing 
                _ -> Just $ intercalate " AND " $ map (\t -> " ( " <> t <> " ) " ) wheres
mkStockFilter :: Maybe FilterExpression -> Maybe Text -> Maybe FilterExpression -> StockFilter
mkStockFilter skum catm catFilterM = StockFilter skum ((,) <$> catm <*> catFilterM)
