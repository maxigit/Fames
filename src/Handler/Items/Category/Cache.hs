-- | Computes categories according to rules and input information
module Handler.Items.Category.Cache 
( categoryFinderCached
, categoryFinderCachedFor
, categoryFinderCachedSlow
, categoryCacheKey
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

import  Import hiding(leftover, force)
import FA as FA hiding (unUserKey)
import Database.Persist.MySQL(rawSql, Single(..), RawSql(..))
import qualified Data.Map as LMap
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as Data.List
import Data.Maybe(fromJust)
import Data.Align(align)
import Lens.Micro.Extras (preview)
import Lens.Micro
import Data.These.Lens
import Text.Printf(printf) 
import Data.Time (diffDays)
import qualified GHC.Exts as List
{-# NOINLINE  categoryFinderCached #-}
{-# NOINLINE  categoryFinderCachedFor #-}
{-# NOINLINE  categoryFinderCachedSlow #-}
{-# NOINLINE  categoryCacheKey #-}
{-# NOINLINE  customerCategoryFinderCached #-}
{-# NOINLINE  loadDebtorsMasterRuleInfos #-}
{-# NOINLINE  loadStockMasterRuleInfos #-}
{-# NOINLINE  loadItemDeliveryForSku #-}
{-# NOINLINE  applyCategoryRules #-}
{-# NOINLINE  refreshCategoryCache #-}
{-# NOINLINE  refreshCategoryFor #-}
{-# NOINLINE  refreshCustomerCategoryCache #-}
{-# NOINLINE  refreshOrderCategoryCache #-}
{-# NOINLINE  refreshNewOrderCategoryCache  #-}
-- * Types 
-- ** Items 
data StockMasterRuleInfo = StockMasterRuleInfo
  { smStockId :: !(Key FA.StockMaster)
  , smDescription :: !String
  , smLongDescription :: !String
  , smUnit :: !String
  , smMbFlag :: !String
  , smInactive :: !Bool
  , smTaxTypeName :: !(Maybe String)
  , smCategoryDescription :: !(Maybe String)
  , smDimension1 :: !(Maybe String)
  , smDimension2 :: !(Maybe String)
  , smSalesAccount :: !String
  , smCogsAccount :: !String
  , smInventoryAccount :: !String
  , smAdjustmentAccount :: !String
  , smSalesPrice :: !(Maybe Double)
  , smDeliveries :: [StockMove'Batch]
  } deriving Show

stockMasterRuleInfoToTuple StockMasterRuleInfo{..} =
  ( smStockId
  , (Single smDescription, Single smLongDescription, Single smUnit, Single smMbFlag, Single smInactive)
  , (Single smTaxTypeName, Single smCategoryDescription, Single smDimension1, Single smDimension2)
  , (Single smSalesAccount, Single smCogsAccount, Single smInventoryAccount, Single smAdjustmentAccount)
  , Single smSalesPrice
  )
stockMasterRuleInfoFromTuple smDeliveries ( smStockId
                             , (Single smDescription, Single smLongDescription, Single smUnit, Single smMbFlag, Single smInactive)
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
-- which are pretty slow so we cache it into a big map
-- However it seems to be really slow so we shouldn't call it.
categoryFinderCachedSlow :: Handler (Text -> FA.StockMasterId -> Maybe Text)
categoryFinderCachedSlow = do
  categories <- categoriesH
  categoryFinderCachedFor categories

categoryFinderCachedFor :: [Text] -> Handler (Text -> FA.StockMasterId -> Maybe Text)
categoryFinderCachedFor categories = do
  finders <- mapM categoryFinderCached categories 
  let cat'Finders :: Map Text (FA.StockMasterId -> Maybe Text)
      cat'Finders = Map.fromList $ zip categories finders
      finder :: Text -> FA.StockMasterId -> Maybe Text
      finder category stockId = Map.lookup category cat'Finders >>=  ($ stockId)
  return $ finder

categoryFinderCached :: Text -> Handler (FA.StockMasterId -> Maybe Text)
categoryFinderCached category =  cache0 False cacheForEver (categoryCacheKey category) $ do
  refreshCategoryCache False (Just category)
  -- we reverse the stock_id to speed up string comparison
  -- as most items share a common prefix, it might be faster to compare them from right to left 
  let sql =  "SELECT stock_id AS order_key, value "
          <> "FROM fames_item_category_cache "
          <> "WHERE category = ?"
  key'values <- runDB $ rawSql sql [PersistText category]
  -- Don't use fromAscList
  let skuMap :: HMap.HashMap Text Text
      skuMap = List.fromList $ [ (key , value )
                               | (Single key, Single value) <- key'values
                               ]
  let finder (FA.StockMasterKey sku) = HMap.lookup sku skuMap
  return $ skuMap `seq` finder

categoryCacheKey :: Text -> (String, Text)
categoryCacheKey = ("category-finder",)
-- ** Category computation 
refreshCategoryFor :: (Maybe Text) -> Maybe FilterExpression -> Handler ()
refreshCategoryFor textm stockFilterM = do
  stockFilter <- case stockFilterM of
    Just sf -> return sf
    Nothing -> LikeFilter <$> appFAStockLikeFilter . appSettings <$> getYesod
  rulesMaps <- appCategoryRules <$> getsYesod appSettings
  deliveryRules <- appDeliveryCategoryRules <$> getsYesod appSettings
  stockMasters <- loadStockMasterRuleInfos stockFilter 
  let rules = map (first unpack) $ concatMap mapToList rulesMaps

  runDB $ do
    let criteria = map (ItemCategoryCategory ==.) (maybeToList textm)
    -- Warning, we delete everything before having computed anything
    -- might be better to delete for a given sku in the form loop
    deleteWhere (criteria <> filterE id ItemCategoryStockId stockFilterM)
    forM_ stockMasters $ \stockMaster0 -> do
      deliveries <- loadItemDeliveryForSku (smStockId stockMaster0)
      let stockMaster = stockMaster0 { smDeliveries = deliveries}
    -- (loadItemDeliveryForSku . smStockId) 
      -- if we are only computing one category
      -- load the other from the db instead of computing them
      categories <- case textm of
            Nothing -> return $ categoriesFor deliveryRules rules stockMaster
            Just cat ->  do
              let  rulem = lookup (unpack cat) rules
              case rulem of
                Nothing -> return []
                Just rule -> do
                  r <- computeOneCategory cat deliveryRules rule  stockMaster
                  return r
      insertMany_ categories


  
-- | Compute the given category using existing value for other categories
computeOneCategory :: Text -> [Map Text DeliveryCategoryRule] -> ItemCategoryRule -> StockMasterRuleInfo -> SqlHandler [ItemCategory]
computeOneCategory cat __deliveryRules rule ruleInfo@StockMasterRuleInfo{..} = do
  deliveryRules <- appDeliveryCategoryRules <$> getsYesod appSettings
  allCategories <- lift $ categoriesH
  -- only keep categories which are defined
  -- and ignore the "source" one which are computed
  let categories = filter (`elem` allCategories) $ map pack $ ruleDependencies rule
  catFinder <- lift $ categoryFinderCachedFor categories
  let otherCategories = filter (/= cat) categories
      rulesMap = mapFromList $ [ (unpack c, unpack value)
                             | c <- otherCategories
                             , value <-  maybeToList (catFinder c smStockId)
                             ]
      cat'values = applyCategoryRules rulesMap deliveryRules [(unpack cat, rule)] ruleInfo
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
  when (c == 0 ) (refreshCategoryFor textm Nothing)
  
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
            <> "     , sm.inactive "

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
            <> "where sm.stock_id "<> keyw
      (keyw, p) = filterEKeyword stockFilter

  runDB $ rawSql sql (toPersistValue base : p)


-- We use string to be compatible with regex substitution
-- applyCategoryRules :: [(String, (CategoryRule a))]
--                    -> StockMasterRuleInfo -> Map String String
-- applyCategoryRules
--   :: [(String, (CategoryRule a))]
--      -> StockMasterRuleInfo
--      -> (Key StockMaster, Map String String)
applyCategoryRules :: [(String, String)]
                   -> [Map Text DeliveryCategoryRule] --  ^ Delivery rules
                   -> [(String, ItemCategoryRule)] --  ^ Categories rules
                   -> StockMasterRuleInfo
                   -> (Key StockMaster, Map String String)
applyCategoryRules extraInputs deliveryRules rules =
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
      (deliveryKeys, mkDeliveryCategories) = mkItemDeliveryInput deliveryRules
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
                                      ("active", if smInactive then "Inactive" else "Active") :
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


categoriesFor :: [Map Text DeliveryCategoryRule] -> [(String, ItemCategoryRule)] -> StockMasterRuleInfo   -> [ItemCategory]
categoriesFor deliveryRules rules = let
  applyCached =  applyCategoryRules [] deliveryRules rules
  in \info -> let
      (sku, categories ) = applyCached info

      in [ ItemCategory (FA.unStockMasterKey sku) (pack key) (pack value)
        | (key, value) <- mapToList categories
        ]



-- ***  Track delivery 
type StockMove'Batch = (FA.StockMove, Maybe Batch')
newtype Batch' = Batch' { unBatch' :: Text } 
  deriving (Show)
-- Try to guess the provenance of each item in stock
-- by comparing the quantity on hand in each container
-- and what's been delivered.
-- We cheat a bit and replace the reference with the Purchase order ref
-- if possible
--
loadItemDeliveryForSku :: Key StockMaster -> SqlHandler [StockMove'Batch]
loadItemDeliveryForSku (FA.StockMasterKey sku) = do
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings
-- Load transactions "creating"  new item, ie purchase order delivery  and postive adjustments
  let moveSql = "SELECT ??, IF(po.requisition_no='', concat('#', po.order_no) , po.requisition_no) "
              <> ", batch "
              <> "FROM 0_stock_moves " 
              <> "LEFT JOIN 0_grn_batch AS b ON (b.id = trans_no and type =?) "
              <> "LEFT JOIN 0_purch_orders AS po ON (b.purch_order_no = po.order_no) "
              <> "LEFT JOIN 0_grn_items AS gi ON (gi.grn_batch_id = b.id AND gi.item_code = stock_id) "
              <> "LEFT JOIN 0_supp_invoice_items as ii ON (ii.grn_item_id = gi.id ) "
              <> "LEFT JOIN fames_transaction_map tm ON (fa_trans_type = ii.supp_trans_type AND fa_trans_no = ii.supp_trans_no AND event_type = ? ) "
              <> "LEFT JOIN fames_packinglist ON (event_no = packinglist_id) "
              <> "WHERE type IN (?,?) AND 0_stock_moves.stock_id = ? AND qty >0 "
              <> "ORDER BY 0_stock_moves.stock_id, loc_code, tran_date DESC "
  
  move'pos'batch <- rawSql moveSql [ toPersistValue (fromEnum ST_SUPPRECEIVE)
                          , toPersistValue PackingListInvoiceE
                          , toPersistValue (fromEnum ST_SUPPRECEIVE)
                          , toPersistValue (fromEnum ST_INVADJUST)
                          , toPersistValue sku
                          ]
  let moves = map (\(Entity _ move, Single poRefM, batchm) -> 
                    ( case poRefM of
                         Nothing -> move
                         Just ref -> move { stockMoveReference = "PO=" <> ref } -- stockMover reference should be null 
                    , fmap (Batch' . unSingle)  batchm
                    )
                  ) move'pos'batch
  -- [FA.StockMoveType <-. (map fromEnum [ST_SUPPRECEIVE, ST_INVADJUST]),  FA.StockMoveStockId ==. sku ]
  --           [Asc FA.StockMoveStockId, Asc FA.StockMoveLocCode, Asc FA.StockMoveTranDate]
  
  -- load qoh in each container
  qohs <- selectList [FA.DenormQohStockId ==. Just sku] [Asc FA.DenormQohLocCode]

  let locQohMap = Map.fromList $ map ((fromJust . FA.denormQohLocCode &&& fromJust . FA.denormQohQuantity) . entityVal) qohs
      locTransMap = groupAscAsMap  (FA.stockMoveLocCode . fst) (:[])  moves
      locs = align locTransMap locQohMap

      defLocM = lookup defaultLocation locs
      otherLoc = deleteMap defaultLocation locs 

      -- transaction "used" From other container (ie not lef t) have
      -- probably been transfered to the default location
      used = concatMap fst $ map (partitionDeliveriesFIFO) (Map.elems otherLoc)
      defThese = These (sortOn (FA.stockMoveTranDate . fst)  $ fromMaybe [] (defLocM >>= preview here) <> used)
                       (fromMaybe 0 (defLocM >>= preview there))
      (_, remainers) = partitionDeliveriesFIFO defThese

  -- traceShowM ("used", used, "QOH", locQohMap)
  return remainers


-- | Partitions deliveries in chronological order so that the "after" moves
-- correspond to the given quantity , ie what's left
-- for example if we have 3 deliverys A:10 B:5 C:5 and 8 item left
-- this will be split into A:10 B:2 (used) and B:2 C:5 (left)
-- This gives us the composition of the current stock given the qoh assuming
-- items are use FIFO
partitionDeliveriesFIFO :: These [StockMove'Batch] Double -> ([StockMove'Batch], [StockMove'Batch])
partitionDeliveriesFIFO (This moves) = (moves, []) -- nothing left
partitionDeliveriesFIFO (That _) = ([], []) -- should raise an error ?
partitionDeliveriesFIFO (These moves 0) = partitionDeliveriesFIFO (This moves)
partitionDeliveriesFIFO (These [] _) = ([], [])
partitionDeliveriesFIFO (These moves' qoh) = let
  moves = sortOn (FA.stockMoveTranDate . fst) moves'
  quantities = map (FA.stockMoveQty . fst)  moves
  -- normally we could create a running balance from the end
  -- but it might be faster to not create a reverse list
  total = sum quantities 
  setQty = sets (\f s -> s { stockMoveQty = f (stockMoveQty s ) })
  usedQty = total - qoh
  running = zip (Data.List.scanl1 (+) quantities) moves
  (used',leftover') = case span ((<= usedQty) . fst) running of
    (used, l@((run, current):leftover)) ->
      let toUse = usedQty - (run - (FA.stockMoveQty . fst) current)
      in if toUse == 0
         then (used, l)
          else ( used ++ [ (run, current & _1 . setQty    .~ toUse)]
              -- , (run, current {stockMoveQty = FA.stockMoveQty current - toUse}):leftover
              , (run, current & _1 . setQty -~ toUse):leftover
              )
    r ->  r
  in
     -- traceShow ("QS", quantities, "Running", map fst running) 
     --   $ traceShow ("SPLIT", map (FA.stockMoveQty . snd) used', map (FA.stockMoveQty . snd) leftover')  $
    ( map snd used', map snd leftover')
                                            
                                       

-- | Creates an input map (category:value) to be given as preset category
-- needed to compute item batch category
mkItemDeliveryInput :: [Map Text DeliveryCategoryRule] -> ([String], [StockMove'Batch] -> [(String, String)])
mkItemDeliveryInput ruleM = (inputKeys, fn) where
  inputKeys = ["trans_no", "location", "date", "reference", "type", "person", "stockId", "pl-batch"]
  fn move'batchs = let
      value'qohs = case extractMainDeliveryRule ruleM of
        Nothing -> map (liftA2 (,) source (stockMoveQty . fst)) move'batchs
        Just (ruleName, rules) -> let
          catRegexCache =  mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys <> map fst rules))
          in mapMaybe (\move -> lookup ruleName $ computeCategories catRegexCache rules (ruleInput move) (source move)
                                <&> (,stockMoveQty $ fst move)
                              )  move'batchs
      ruleInput (FA.StockMove{..}, batchm)  = RuleInput (Map.fromList
        [ ("trans_no", show stockMoveTransNo)
        , ("location", unpack stockMoveLocCode)
        , ("date", show stockMoveTranDate)
        , ("reference", unpack stockMoveReference)
        , ("type", show stockMoveType)
        , ("full_type", showTransType (toEnum stockMoveType))
        , ("person", maybe "" show stockMovePersonId)
        , ("quantity", show (floor stockMoveQty))
        , ("stockId", unpack stockMoveStockId)
        , ("pl-batch", maybe "" (unpack . unBatch') batchm)
        ]) Nothing
      source (FA.StockMove{..},_) = show stockMoveTranDate
                  <> " " <> showTransType (toEnum stockMoveType) -- full text 
                  <> " " <> show stockMoveType -- number for easier "selection"
                  <> "#" <>  show stockMoveTransNo
                  <> " location=" <> unpack stockMoveLocCode
                  <> " @" <> maybe "" show stockMovePersonId
                  <> " ref=" <> unpack stockMoveReference 
      -- group batch by value and sum quantities
      valueQohMap = LMap.fromListWith (+) $ filter (not . null . fst) value'qohs
      withQoh = map (\(v,q) -> v ++ " qoh=" ++ show (floor q)) (LMap.toList valueQohMap)
      withQoh' = map (\(v,q) -> v ++ " +" ++ show (floor q)) (LMap.toList valueQohMap)
      mkCategory catName vs = (catName, Data.List.intercalate " | " vs)
      in mkCategory "fifo-deliveries" (LMap.keys valueQohMap) :
         mkCategory "fifo-deliveries-with-qoh" withQoh :
         mkCategory "fifo-deliveries-with-short-qoh" withQoh' :
         mkCategory "fifo-deliveries-by-qoh" (map fst $ sortOn (Down . snd) $ LMap.toList valueQohMap) :
         []

-- | Flatten delivery rules and find the name of the last one, which is the one we need as an output.
-- For obvious reasons, all not used by the output needs to be computed before the output category itself.
-- therefore the output has to be the last one
extractMainDeliveryRule :: [Map Text DeliveryCategoryRule] -> Maybe (String, [(String, DeliveryCategoryRule)])
extractMainDeliveryRule maps = do -- Maybe
  let name'rules = first unpack <$> concatMap mapToList maps
  (ruleName, _ ) <- lastMay name'rules
  Just (ruleName, name'rules) 
  


   

-- * Customers 
-- | Return a function finding the customerCategory given a style
-- The current implementation is based on TDFA regex
-- which are pretty so we cache it into a big map
customerCategoryFinderCached :: Handler (Text -> FA.DebtorsMasterId -> Maybe Text)
customerCategoryFinderCached = cache0 False cacheForEver "customerCategory-finder" $ do
  refreshCustomerCategoryCache False
  customerCategories <- runDB $ selectList [] [Asc CustomerCategoryCustomerId, Asc CustomerCategoryCategory]
  -- don't use fromAscLIs
  let debtor'catMap = Map.fromList [((customerCategoryCustomerId , customerCategoryCategory ), customerCategoryValue )
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
applyCustomerCategoryRules :: [(String, CustomerCategoryRule)]
                   -> DebtorsMasterRuleInfo
                   -> (Key DebtorsMaster, Map String String)
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


customerCategoriesFor :: [(String, CustomerCategoryRule)] -> DebtorsMasterRuleInfo   -> [CustomerCategory]
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
  
orderCategoriesFor :: [(String, (OrderCategoryRule))] -> Entity FA.SalesOrder -> [OrderCategory]
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
  in \(Entity __orderId FA.SalesOrder{..}) -> let
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
