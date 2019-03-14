-- | Computes categories according to rules and input information
module Handler.Items.Category.Cache 
( categoryFinderCached
, customerCategoryFinderCached
, loadDebtorsMasterRuleInfos
, loadStockMasterRuleInfos
, loadItemDeliveryForSku
, applyCategoryRules
, refreshCategoryCache
, refreshCategoryFor
, refreshCustomerCategoryCache
, refreshOrderCategoryCache
, refreshNewOrderCategoryCache 
, StockMasterRuleInfo(..)
) where

import  Import
import FA as FA hiding (unUserKey)
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..), RawSql(..))
import qualified Data.Map as LMap
import qualified Data.Map.Strict as Map
import qualified Data.List as Data.List
import Data.Maybe(fromJust)
import Data.Align(align)
import Data.These
import Lens.Micro.Extras (preview)
import Text.Printf(printf) 
import Data.Time (diffDays, addGregorianMonthsClip)
-- * Types
-- ** Items
data StockMasterRuleInfo = StockMasterRuleInfo
  { smStockId :: !(Key FA.StockMaster)
  , smDescription :: !String
  , smLongDescription :: !String
  , smUnit :: !String
  , smMbFlag :: !String
  , smTaxTypeName :: !(Maybe String)
  , smCategoryDescription :: !(Maybe String)
  , smDimension1 :: !(Maybe String)
  , smDimension2 :: !(Maybe String)
  , smSalesAccount :: !String
  , smCogsAccount :: !String
  , smInventoryAccount :: !String
  , smAdjustmentAccount :: !String
  , smSalesPrice :: !(Maybe Double)
  , smDeliveries :: [FA.StockMove]
  } deriving Show

stockMasterRuleInfoToTuple StockMasterRuleInfo{..} =
  ( smStockId
  , (Single smDescription, Single smLongDescription, Single smUnit, Single smMbFlag)
  , (Single smTaxTypeName, Single smCategoryDescription, Single smDimension1, Single smDimension2)
  , (Single smSalesAccount, Single smCogsAccount, Single smInventoryAccount, Single smAdjustmentAccount)
  , Single smSalesPrice
  )
stockMasterRuleInfoFromTuple smDeliveries ( smStockId
                             , (Single smDescription, Single smLongDescription, Single smUnit, Single smMbFlag)
                             , (Single smTaxTypeName, Single smCategoryDescription, Single smDimension1, Single smDimension2)
                             , (Single smSalesAccount, Single smCogsAccount, Single smInventoryAccount, Single smAdjustmentAccount)
                             , Single smSalesPrice
                             ) = StockMasterRuleInfo{..}

instance RawSql StockMasterRuleInfo where
  rawSqlCols f = rawSqlCols f . stockMasterRuleInfoToTuple
  rawSqlColCountReason = rawSqlColCountReason . stockMasterRuleInfoToTuple
  rawSqlProcessRow = stockMasterRuleInfoFromTuple [] <$$> rawSqlProcessRow
  

-- ** Customers
type DebtorsMasterRuleInfo = (Key DebtorsMaster
                           , (Single String, Single String, Single String, Single String) -- debtor infos
                           , (Single (Maybe String), Single (Maybe String)) -- dimensions
                           , (Single (Maybe Day), Single (Maybe String)) -- first order
                           , (Single (Maybe Day), Single (Maybe String)) -- last order
                           )
-- * Items
-- | Return a function finding the category given a style
-- The current implementation is based on TDFA regex
-- which are pretty so we cache it into a big map
categoryFinderCached :: Handler (Text -> FA.StockMasterId -> Maybe Text)
categoryFinderCached = cache0 False cacheForEver "category-finder" $ do
  reverseKey <- getsYesod appSettings <&> appReverseCategoryKey
  refreshCategoryCache False Nothing
  -- we reverse the stock_id to speed up string comparison
  -- as most items share a common prefix, it might be faster to compare them from right to left 
  let (order_key, valueFinder ) = if reverseKey
                                  then ("REVERSE(stock_id)", \sku -> Map.lookup (reverse sku))
                                  else ("stock_id", \sku -> Map.lookup sku)
  categories <- categoriesH
  catMaps <- forM categories $ \category -> do

         let sql =  "SELECT " <> order_key <> " AS order_key, value "
                 <> "FROM fames_item_category_cache "
                 <> "WHERE category = ?"
                 <> "ORDER by order_key"
         key'values <- runDB $ rawSql sql [PersistText category]
         let skuMap = Map.fromAscList [ (key , value )
                                      | (Single key, Single value) <- key'values
                                      ]
         return (category, skuMap)
  let cat'skuMap = Map.fromList catMaps
      finder category (FA.StockMasterKey sku) = do
        keyValueMap <- Map.lookup category cat'skuMap
        valueFinder sku keyValueMap

  return $ cat'skuMap `seq` finder
-- ** Category computation
refreshCategoryFor :: (Maybe Text) -> Maybe FilterExpression -> Handler ()
refreshCategoryFor textm stockFilterM = do
  stockFilter <- case stockFilterM of
    Just sf -> return sf
    Nothing -> LikeFilter <$> appFAStockLikeFilter . appSettings <$> getYesod
  rulesMaps <- appCategoryRules <$> getsYesod appSettings
  deliveryRule <- appDeliveryCategoryRule <$> getsYesod appSettings
  stockMasters <- loadStockMasterRuleInfos stockFilter 
  let rules = map (first unpack) $ concatMap mapToList rulesMaps

  runDB $ do
    let criteria = map (ItemCategoryCategory ==.) (maybeToList textm)
    -- Warning, we delete everything before having computed anything
    -- might be better to delete for a given sku in the form loop
    deleteWhere (criteria <> filterE id ItemCategoryStockId (Just stockFilter))
    forM_ stockMasters $ \stockMaster0 -> do
      deliveries <- loadItemDeliveryForSku (smStockId stockMaster0)
      let stockMaster = stockMaster0 { smDeliveries = deliveries}
    -- (loadItemDeliveryForSku . smStockId) 
      -- if we are only computing one category
      -- load the other from the db instead of computing them
      categories <- case textm of
            Nothing -> return $ categoriesFor deliveryRule rules stockMaster
            Just cat ->  do
              let  rulem = lookup (unpack cat) rules
              case rulem of
                Nothing -> return []
                Just rule -> computeOneCategory cat deliveryRule rule  stockMaster
      mapM_ insert_ categories


  
-- | Compute the given category using existing value for other categories
computeOneCategory :: Text -> (Maybe CategoryRule) -> CategoryRule -> StockMasterRuleInfo -> SqlHandler [ItemCategory]
computeOneCategory cat deliveryRule rule ruleInfo@StockMasterRuleInfo{..} = do
  deliveryRule <- appDeliveryCategoryRule <$> getsYesod appSettings
  catFinder <- lift categoryFinderCached
  categories <- lift categoriesH
  let otherCategories = filter (/= cat) categories
      rulesMap = mapFromList $ [ (unpack c, unpack value)
                             | c <- otherCategories
                             , value <-  maybeToList (catFinder c smStockId)
                             ]
      cat'values = applyCategoryRules rulesMap deliveryRule [(unpack cat, rule)] ruleInfo
      cats = unpack cat
  return [ ItemCategory (FA.unStockMasterKey $ smStockId) (pack key) (pack value)
        | (key, value) <- mapToList  (snd cat'values)
        , key == cats
        ]



-- Populates the item category cache table if needed
refreshCategoryCache :: Bool -> Maybe Text -> Handler ()
refreshCategoryCache force textm = do
  -- check if the datbase is empty
  -- clear it if needed
  let criteria = map (ItemCategoryCategory ==.) (maybeToList textm)
  c <- runDB $ do
    when force (deleteWhere criteria)
    count criteria
  when (c == 0) (refreshCategoryFor textm Nothing)
  
-- ** DB
-- *** Standard rules
loadStockMasterRuleInfos :: FilterExpression -> Handler [StockMasterRuleInfo]
loadStockMasterRuleInfos stockFilter = do
  base <- basePriceList
  let sql = " "
            <> "select sm.stock_id "

            <> "     , sm.description "
            <> "     , sm.long_description "
            <> "     , sm.units "
            <> "     , sm.mb_flag "

            <> "     , tt.name "
            <> "     , cat.description as category "
            <> "     , dim1.name as dim1 "
            <> "     , dim2.name As dim2 "

            <> "     , sm.sales_account "
            <> "     , sm.cogs_account "
            <> "     , sm.inventory_account "
            <> "     , sm.adjustment_account "

            <> "     , sales.price "
            <> "from 0_stock_master as sm "
            <> "left join 0_tax_types  AS tt on (sm.tax_type_id = tt.id) "
            <> "left join 0_dimensions as dim1 on (sm.dimension_id = dim1.id) "
            <> "left join 0_dimensions as dim2 on (sm.dimension2_id = dim2.id) "
            <> "left join 0_stock_category as cat on (sm.category_id = cat.category_id) "
            <> "left join 0_prices as sales on (sm.stock_id = sales.stock_id AND sales.sales_type_id =?) "
            <> "where sm.stock_id "<> keyw <> "?"
      (keyw, p) = filterEKeyword stockFilter

  runDB $ rawSql sql [toPersistValue base, PersistText p]


-- We use string to be compatible with regex substitution
-- applyCategoryRules :: [(String, CategoryRule)]
--                    -> StockMasterRuleInfo -> Map String String
-- applyCategoryRules
--   :: [(String, CategoryRule)]
--      -> StockMasterRuleInfo
--      -> (Key StockMaster, Map String String)
applyCategoryRules :: [(String, String)]
                   -> Maybe CategoryRule -- ^ Delivery rules
                   -> [(String, CategoryRule)] -- ^ Categories rules
                   -> StockMasterRuleInfo
                   -> (Key StockMaster, Map String String)
applyCategoryRules extraInputs deliveryRule rules =
  let inputKeys = ["sku"
              ,"description"
              ,"longDescription"
              ,"unit"
              ,"mbFlag"
              ,"taxType"
              ,"category"
              ,"dimension1"
              ,"dimension2"
              ,"salesAccount"
              ,"cogsAccount"
              ,"inventoryAccount"
              ,"adjustmentAccount"
              ] <> deliveryKeys -- inputMap key, outside of the lambda so it can be optimised and calculated only once
      (deliveryKeys, mkDeliveryCategories) = mkItemDeliveryInput deliveryRule
      catRegexCache =  mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys  <> map fst extraInputs <>  map fst rules))
  in \StockMasterRuleInfo{..}
     -> let
              sku = unStockMasterKey smStockId
              ruleInput = RuleInput ( mapFromList 
                                    ( ("sku", unpack sku) :
                                      ("description", smDescription) :
                                      ("longDescription", smLongDescription) :
                                      ("unit", smUnit) :
                                      ("mbFlag", smMbFlag) :
                                      (("taxType",) <$> smTaxTypeName) ?:
                                      (("category",) <$> smCategoryDescription) ?:
                                      (("dimension1",) <$> smDimension1) ?:
                                      (("dimension2",) <$> smDimension2) ?:
                                      ("salesAccount", smSalesAccount) :
                                      ("cogsAccount", smCogsAccount) :
                                      ("inventoryAccount", smInventoryAccount) :
                                      ("adjustmentAccount", smAdjustmentAccount) :
                                    extraInputs <> mkDeliveryCategories smDeliveries
                                    )
                                  )
                                  smSalesPrice
        in (smStockId, computeCategories catRegexCache rules ruleInput (unpack sku))


categoriesFor :: Maybe CategoryRule -> [(String, CategoryRule)] -> StockMasterRuleInfo   -> [ItemCategory]
categoriesFor deliveryRule rules = let
  applyCached =  applyCategoryRules [] deliveryRule rules
  in \info -> let
      (sku, categories ) = applyCached info

      in [ ItemCategory (FA.unStockMasterKey sku) (pack key) (pack value)
        | (key, value) <- mapToList categories
        ]



-- ***  Track delivery
-- Try to guess the provenance of each item in stock
-- by comparing the quantity on hand in each container
-- and what's been deliveredhiding((<&>))
loadItemDeliveryForSku :: Key StockMaster -> SqlHandler [StockMove]
loadItemDeliveryForSku (FA.StockMasterKey sku) = do
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings
-- Load transactions "creating"  new item, ie purchase order delivery  and postive adjustments
  moves <- selectList [FA.StockMoveType <-. (map fromEnum [ST_SUPPRECEIVE, ST_INVADJUST]),  FA.StockMoveStockId ==. sku ]
             [Asc FA.StockMoveStockId, Asc FA.StockMoveLocCode, Asc FA.StockMoveTranDate]
  
  -- load qoh in each container
  qohs <- selectList [FA.DenormQohStockId ==. Just sku] [Asc FA.DenormQohLocCode]

  let locQohMap = Map.fromAscList $ map ((fromJust . FA.denormQohLocCode &&& fromJust . FA.denormQohQuantity) . entityVal) qohs
      locTransMap = groupAscAsMap  FA.stockMoveLocCode (:[])  (map entityVal moves)
      locs = align locTransMap locQohMap

      defLocM = lookup defaultLocation locs
      otherLoc = deleteMap defaultLocation locs 

      -- transaction "used" From other container (ie not lef t) have
      -- probably been transfered to the default location
      used = concatMap fst $ map (partitionDeliveriesFIFO) (Map.elems otherLoc)
      defThese = These (sortOn FA.stockMoveTranDate $ fromMaybe [] (defLocM >>= preview here) <> used)
                       (fromMaybe 0 (defLocM >>= preview there))
      (_, remainers) = partitionDeliveriesFIFO defThese

  return remainers


-- | Partitions deliveries in chronological order so that the "after" moves
-- correspond to the given quantity , ie what's left
-- for example if we have 3 deliverys A:10 B:5 C:5 and 8 item left
-- this will be split into A:10 B:2 (used) and B:2 C:5 (left)
-- This gives us the composition of the current stock given the qoh assuming
-- items are use FIFO
partitionDeliveriesFIFO :: These [FA.StockMove] Double -> ([FA.StockMove], [FA.StockMove])
partitionDeliveriesFIFO (This moves) = (moves, []) -- nothing left
partitionDeliveriesFIFO (That qho) = ([], []) -- should raise an error ?
partitionDeliveriesFIFO (These moves 0) = partitionDeliveriesFIFO (This moves)
partitionDeliveriesFIFO (These [] qoh) = ([], [])
partitionDeliveriesFIFO (These moves qoh) = let
  -- normally we could create a running balance from the end
  -- but it might be faster to not create a reverse list
  quantities = map FA.stockMoveQty moves
  total = sum quantities 
  usedQty = total - qoh
  running = zip (Data.List.scanl1 (+) quantities) moves
  (used',leftover') = case span ((<= usedQty) . fst) running of
    (used, l@((run, current):leftover)) ->
      let toUse = usedQty - (run - FA.stockMoveQty current)
      in if toUse == 0
         then (used, l)
         else ( used ++ [ (run, current {stockMoveQty = toUse})]
              , (run, current {stockMoveQty = FA.stockMoveQty current - toUse}):leftover
              )
    r@(used, []) ->  r
  in -- traceShow ("QS", quantities, "Running", map fst running) 
  -- $ traceShow ("SPLIT", map (FA.stockMoveQty . snd) used', map (FA.stockMoveQty . snd) leftover')  $
  (map snd used', map snd leftover')
                                            
                                       

-- | Creates an input map (category:value) to be given as preset category
-- needed to compute item batch category
mkItemDeliveryInput :: Maybe CategoryRule -> ([String], [FA.StockMove] -> [(String, String)])
mkItemDeliveryInput ruleM = (inputKeys, fn) where
  inputKeys = ["trans_no", "location", "date", "reference", "type", "person"]
  catRegexCache =  mapFromList (map (liftA2 (,) id mkCategoryRegex) inputKeys)
  fn moves = let
      values = case ruleM of
        Nothing -> map source moves
        Just rule -> mapMaybe (\move -> computeCategory catRegexCache (source move) (ruleInput move) rule)  moves
      ruleInput FA.StockMove{..} = RuleInput (Map.fromList
        [ ("trans_no", show stockMoveTransNo)
        , ("location", unpack stockMoveLocCode)
        , ("date", show stockMoveTranDate)
        , ("reference", unpack stockMoveReference)
        , ("type", show stockMoveType)
        , ("person", maybe "" show stockMovePersonId)
        ]) Nothing
      source FA.StockMove{..} = show stockMoveTranDate
                  <> " " <> showTransType (toEnum stockMoveType) -- full text 
                  <> " " <> show stockMoveType -- number for easier "selection"
                  <> "#" <>  show stockMoveTransNo
                  <> " [" <> unpack stockMoveLocCode
                  <> "] @" <> maybe "" show stockMovePersonId
      nubbed = Data.List.nub . Data.List.sort $ filter (not . null) values
      in [("fifo-deliveries", Data.List.intercalate " | " nubbed)]

   

-- * Customers
-- | Return a function finding the customerCategory given a style
-- The current implementation is based on TDFA regex
-- which are pretty so we cache it into a big map
customerCategoryFinderCached :: Handler (Text -> FA.DebtorsMasterId -> Maybe Text)
customerCategoryFinderCached = cache0 False cacheForEver "customerCategory-finder" $ do
  refreshCustomerCategoryCache False
  customerCategories <- runDB $ selectList [] [Asc CustomerCategoryCustomerId, Asc CustomerCategoryCategory]
  let debtor'catMap = Map.fromDistinctAscList [((customerCategoryCustomerId , customerCategoryCategory ), customerCategoryValue )
                           | (Entity _ CustomerCategory{..}) <- customerCategories
                           ]
      finder customerCategory (FA.DebtorsMasterKey debtor) = Map.lookup (debtor, customerCategory) debtor'catMap

  debtor'catMap `seq` return finder

-- ** Computations
refreshCustomerCategoryFor :: Handler ()
refreshCustomerCategoryFor = do
  rulesMaps <- appCustomerCategoryRules <$> getsYesod appSettings
  debtorsmasters <- loadDebtorsMasterRuleInfos
  let customerCategories = concatMap (customerCategoriesFor rules) debtorsmasters
      rules = map (first unpack) $ concatMap mapToList rulesMaps
  runDB $ do
    deleteWhere ([] :: [Filter CustomerCategory])
    -- insertMany_ customerCategories
    mapM_ insert_ customerCategories

-- Populates the customer customerCategory cache table if needed
refreshCustomerCategoryCache :: Bool -> Handler ()
refreshCustomerCategoryCache force = do
  -- check if the datbase is empty
  -- clear it if needed
  c <- runDB $ do
    when force (deleteWhere ([] ::[Filter CustomerCategory]))
    count ([] ::[Filter CustomerCategory]) 
  when (c == 0) (refreshCustomerCategoryFor) 
  
loadDebtorsMasterRuleInfos :: Handler [DebtorsMasterRuleInfo]
loadDebtorsMasterRuleInfos = do
  let sql = " "
            <> "select dm.debtor_no "
            <> "     , dm.name"
            <> "     , dm.notes"
            <> "     , dm.tax_id"
            <> "     , dm.curr_code"
            <> "     , dim1.name as dim1 "
            <> "     , dim2.name As dim2 "
            <> "     , ord.first_ord_date "
            <> "     , ord.first_ord_ref "
            <> "     , ord.last_ord_date "
            <> "     , ord.last_ord_ref "
            <> " from 0_debtors_master as dm "
            <> " left join 0_dimensions as dim1 on (dm.dimension_id = dim1.id) "
            <> " left join 0_dimensions as dim2 on (dm.dimension2_id = dim2.id) "
            -- <> " left join 0_prices as sales on (sales.sales_type_id = dm.sales_type) "
            <> " left join (" <> orders <> ") ord on(dm.debtor_no = ord.debtor_no) "
      orders = " "
              <> " select debtor_no, MIN(ord_date) AS first_ord_date, MAX(ord_date) AS last_ord_date"
              <> " , SUBSTRING_INDEX(GROUP_CONCAT(reference order by ord_date), ',', 1) AS first_ord_ref"
              <> " , SUBSTRING_INDEX(GROUP_CONCAT(reference order by ord_date desc), ',', 1) AS last_ord_ref"
              <> " from 0_sales_orders"
              <> " group by debtor_no"
      decode (debtorId , (Single name, Single note, Single taxCode, Single currency)
              , dims, firstOrder, lastOrder
              ) = (debtorId , (Single . unpack . decodeHtmlEntities $ pack  name
                              , Single . unpack . decodeHtmlEntities $ pack note, Single taxCode, Single currency)
              , dims, firstOrder, lastOrder
              )

  infos <- runDB $ rawSql sql []
  return $ map decode infos


-- We use string to be compatible with regex substitution
-- applyCategoryRules :: [(String, CustomerCategoryRule)]
--                    -> DebtorsMasterRuleInfo -> Map String String
-- applyCategoryRules
--   :: [(String, CustomerCategoryRule)]
--      -> DebtorsMasterRuleInfo
--      -> (Key DebtorsMaster, Map String String)
applyCustomerCategoryRules rules =
  let inputKeys = ["debtor_id"
              ,"name"
              ,"note"
              ,"tax_code"
              ,"currency"
              ,"dimension1"
              ,"dimension2"
              ] -- inputMap key, outside of the lambda so it can be optimised and calculated only once
      catRegexCache =  mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys  <> map fst rules))
  in \(debtorId
                         , (Single name, Single note, Single taxCode, Single currency)
                         , (Single dimension1, Single dimension2 )
                         , (Single firstOrderDate, Single firstOrderRef)
                         , (Single lastOrderDate, Single lastOrderRef)
                         )
     -> let
              debtor = unDebtorsMasterKey debtorId
              ruleInput = RuleInput ( mapFromList 
                                    ( ("id", show debtor ) :
                                      ("name", unpack name) :
                                      ("note", note) :
                                      ("tax_code", taxCode) :
                                      ("currency", currency) :
                                      (("dimension1",) <$> dimension1) ?:
                                      (("dimension2",) <$> dimension2) ?:
                                      ((("first_order_date",) . show) <$> firstOrderDate) ?:
                                      (("first_order_ref",) <$> firstOrderRef) ?:
                                      ((("last_order_date",) . show) <$> lastOrderDate) ?:
                                      (("last_order_ref",) <$> lastOrderRef) ?:
                                    []
                                    )
                                  )
                                  (Just $ fromIntegral debtor)
        in (debtorId, computeCategories catRegexCache rules ruleInput (unpack name))


customerCategoriesFor :: [(String, CategoryRule)] -> DebtorsMasterRuleInfo   -> [CustomerCategory]
customerCategoriesFor rules = let
  applyCached =  applyCustomerCategoryRules rules
  in \info -> let
      (debtor, customerCategories ) = applyCached info 

      in [ CustomerCategory (FA.unDebtorsMasterKey debtor) (pack key) (pack value)
        | (key, value) <- mapToList customerCategories
        ]


-- * Order Category caches
-- | Clears all order categories and computes the n first if given
refreshOrderCategoryCache :: Maybe Int -> Handler ()
refreshOrderCategoryCache nM = do
  runDB $ deleteWhere ([] :: [Filter OrderCategory])
  refreshNewOrderCategoryCache nM

-- | Computes Order category and cache them
refreshNewOrderCategoryCache :: Maybe Int -> Handler ()
refreshNewOrderCategoryCache nM = runDB $ do
  rulesMap <- appOrderCategoryRules <$> getsYesod appSettings
  -- find last order with a category
  [(Single lastOrderM)] <- rawSql "SELECT max(order_id) FROM fames_order_category_cache" []
  orders <- selectList ( (FA.SalesOrderTransType ==. fromEnum ST_SALESORDER)
                       : maybe [] (return . (FA.SalesOrderOrderNo >.)) lastOrderM
                       ) (Asc FA.SalesOrderOrderNo : (maybe [] (return . LimitTo) nM))
  let orderCategories = concatMap (orderCategoriesFor rules) orders
      rules = map (first unpack) $ concatMap mapToList rulesMap
  insertMany_ orderCategories
  
orderCategoriesFor :: [(String, CategoryRule)] -> Entity FA.SalesOrder -> [OrderCategory]
orderCategoriesFor rules = let
  -- trick to for applyCached to be computed only once, so that the regular expressions
  -- are only computed once
  applyCached = applyOrderCategoryRules rules
  in \orderE -> let
       (orderId, orderCategories) = applyCached orderE
       in [OrderCategory (orderId) (pack key) (pack value)
          | (key, value) <- mapToList orderCategories
          ]

applyOrderCategoryRules rules =
  let inputKeys = ["date"
                  , "delivery-date"
                  , "delivery-delay"
                  , "reference"
                  , "customer-ref"
                  , "price_list"
                  , "shipping-cost"
                  , "shipping-via"
                  -- , "deliveryAddress"
                  , "amount"
                  , "amount-PPD"
                  , "PPD-days"
                  , "comment"

                  ]
      catRegexCache = mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys <> map fst rules))
  in \(Entity orderId FA.SalesOrder{..}) -> let
        ruleInput = RuleInput (mapFromList riList) (Just $ salesOrderTotal)
        riList = ("date", show salesOrderOrdDate) :
                 ("delivery-date", show salesOrderDeliveryDate) :
                 ("delivery-delay", printf "%03d" $ diffDays salesOrderDeliveryDate salesOrderOrdDate) :
                 ("reference", unpack salesOrderReference) :
                 ("customer-ref", unpack salesOrderCustomerRef) :
                 ("price_list", show salesOrderOrderType ) :
                 ("shipping-cost", printf "%09.2f"  salesOrderFreightCost) : -- padding with 0 to 6 figures 
                 ("shipping-via", show salesOrderShipVia) :
                 ("amount", printf "%09.2f"  salesOrderTotal) : -- padding with 0 to 6 figures 
                 ((("amount-PPD",) . printf "%09.2f") <$>  salesOrderTotalPpd) ?: -- padding with 0 to 6 figures 
                 ("PPD-days",  show  salesOrderPpdDays) : -- padding with 0 to 6 figures 
                 ((("comment",) . unpack) <$> salesOrderComments) ?:
                 []
        in (salesOrderOrderNo, computeCategories catRegexCache rules ruleInput (unpack salesOrderReference))
