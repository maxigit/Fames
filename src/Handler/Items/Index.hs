{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Index where

import Import hiding(replace)
import Handler.Table
import Yesod.Form.Bootstrap3
import FA
import qualified DC as DC
import Items
import qualified Data.Map as Map
import Data.Monoid(Endo(..), appEndo)
import Data.Text(toTitle, replace, splitOn)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import Database.Persist.MySQL hiding(replace)
import qualified Data.IntMap.Strict as IntMap
import Text.Printf (printf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Util.Cache
import Control.Monad(zipWithM_)
import Handler.Util
import Control.Monad.Reader(mapReaderT)

-- * Types
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data IndexParam = IndexParam
  { ipStyles :: Maybe FilterExpression
  , ipVariationsF :: Maybe FilterExpression
  , ipVariationGroup :: Maybe Text -- ^ Alternative to variations
  , ipShowInactive :: Bool
  , ipShowExtra :: Bool
  , ipBases :: Map Text Text -- ^ style -> selected base
  , ipChecked :: [Text] -- ^ styles to act upon
  , ipColumns :: [Text] -- ^ columns to act upon
  , ipMode :: ItemViewMode
  , ipClearCache :: Bool -- ^ Clear cache
  } deriving (Eq, Show, Read)

ipVariations :: IndexParam -> Either FilterExpression (Maybe Text)
ipVariations param = case (ipVariationsF param, ipVariationGroup param) of
        (Just var, Nothing) -> Left var
        (_, group_) -> Right group_

data IndexColumn = GLColumn Text
            | PriceColumn Int
            | PurchaseColumn Int
            | CheckColumn
            | RadioColumn
            | StockIdColumn
            | StatusColumn
            | FAStatusColumn Text
            | WebStatusColumn Text
            | WebPriceColumn Int
            deriving Show

-- | Preloaded data to be used by the handler
data IndexCache = IndexCache
  { icPriceLists :: [Entity SalesType]
  , icPriceListNames :: IntMap Text
  , icSupplierNames :: IntMap Text
  , icWebPriceList :: [Int] -- price list ids
  }

data Button = CreateMissingBtn
            | ActivateBtn
            | DeactivateBtn
            | DeleteBtn
            deriving (Read, Show)
data ButtonStatus = BtnActive | BtnInactive Text | BtnHidden deriving Show
-- * Handlers
getItemsIndexR :: Maybe ItemViewMode -> Handler TypedContent
getItemsIndexR mode = do
  renderIndex (paramDef mode) ok200

postItemsIndexR :: Maybe ItemViewMode -> Handler TypedContent
postItemsIndexR mode = do
  action <- lookupPostParam "button"
  (param,_,_) <- getPostIndexParam (paramDef mode)
  case action >>= readMay of
    Just CreateMissingBtn -> createMissing param
    Just ActivateBtn      -> activate param
    Just DeactivateBtn    -> deactivate param
    Just DeleteBtn        -> deleteItems param
    _ -> return ()
  renderIndex param ok200

-- | User authorized to see purchase prices or not ?
purchaseAuth :: Handler Bool
purchaseAuth = do
  settings <- appSettings <$> getYesod
  mu <- maybeAuth
  let role = roleFor (appRoleFor settings) (userIdent . entityVal <$> mu)
  return $ null (filterPermissions ReadRequest (setFromList ["purchase/prices"]) role)
    
-- * Utils
-- ** Constants
-- | Default cached delay. 1 day. Can be refreshed using the refresh cache button if needed.
cacheDelay :: CacheDelay
cacheDelay = cacheDay 1
-- ** Params and Forms
paramDef :: Maybe ItemViewMode -> IndexParam
paramDef mode = IndexParam Nothing Nothing Nothing False True
                           mempty empty empty
                           (fromMaybe ItemGLView mode)
                           False
-- indexForm :: (MonadHandler m,
--               RenderMessage (HandlerSite m) FormMessage)
--           => [Text]
--           -> IndexParam
--           -> Markup
--           -> MForm m (FormResult IndexParam, WidgetT (HandlerSite m) IO ())
indexForm groups param = renderBootstrap3 BootstrapBasicForm form
  where form = IndexParam
          <$> (aopt filterEField "styles" (Just $ ipStyles param))
          <*> (aopt filterEField "variations" (Just $ ipVariationsF param))
          <*> (aopt (selectFieldList groups') "variation group" (Just $ ipVariationGroup param))
          <*> (areq boolField "Show Inactive" (Just $ ipShowInactive param))
          <*> (areq boolField "Show Extra" (Just $ ipShowExtra param))
          <*> pure (ipBases param)
          <*> pure (ipChecked param)
          <*> pure (ipColumns param)
          <*> pure (ipMode param)
          <*> pure False
        groups' =  map (\g -> (g,g)) groups

-- | Fill the parameters which are not in the form but have to be extracted
-- from the request parameters
fillTableParams :: IndexParam -> Handler IndexParam
fillTableParams params0 = do
  (params,_) <- runRequestBody
  let checked = mapMaybe (stripPrefix "check-" . fst)  params
      bases = Map.fromList $ mapMaybe (\(k,v) -> stripPrefix "base-" k <&> (\b -> (b, v))
                                     ) params
      columns = mapMaybe (stripPrefix "col-check-" . fst) params
      clearCache = lookup "button" params == Just "refresh"
  return $ params0 {ipChecked = checked, ipBases=bases, ipColumns=columns, ipClearCache = clearCache}
   

-- getPostIndexParam :: IndexParam -> Handler (IndexParam, _
getPostIndexParam :: IndexParam -> HandlerT App IO (IndexParam, WidgetT App IO (), Enctype)
getPostIndexParam param0 = do
  varGroup <- appVariationGroups <$> getsYesod appSettings
  ((resp, form), encType) <- runFormPost (indexForm (Map.keys varGroup) param0)
  let param1 = case resp of
        FormMissing -> param0
        FormSuccess par -> par
        FormFailure _ ->  param0
  param <- fillTableParams param1
  return (param, form, encType)

-- | Generates the filter which loads the variations corresponding to the given
-- parameters.
-- styleFilter.
styleFilter :: IsString a => IndexParam -> Either a [Filter StockMaster]
styleFilter param = 
  let styleF = ipStyles param
  in case styleF of
      Nothing -> Left "Please enter a styles filter expression (SQL like expression or regexp starting with '/'')"
      Just _ -> Right (filterE StockMasterKey FA.StockMasterId styleF
                            ++ if (ipShowInactive param)
                               then []
                               else [FA.StockMasterInactive ==. False]
                      )

-- | Generates function to filter sku based on items which have been checked (checkbox)
checkFilter :: IndexParam -> Text -> Bool
checkFilter param sku = 
  let set = setFromList (ipChecked param) :: Set Text
      toKeep sku = case ipChecked param of
        [] -> True
        cs -> sku `member` set
  in toKeep sku

-- ** Preloaded Cache
fillIndexCache :: Handler IndexCache
fillIndexCache = cache0 False (cacheHour 1) "index/static"  $ runDB $ do
  salesTypes <- selectList [] [] -- [Entity SalesType]
  let priceListNames = mapFromList [ (k, salesTypeSalesType t)
                                   | (Entity (SalesTypeKey k) t) <- salesTypes
                                   ]
  supplierNames <- do
        entities <- selectList [] []
        return $ mapFromList [(k, supplierSuppName s) | (Entity (SupplierKey k) s) <- entities]

  rows <- rawSql "SHOW TABLES LIKE 'dcx_field_data_field_price%'" []
  let webPriceList = [ pId :: Int
                  | (Single table) <- rows
                  , Just pId <- return $ readMay =<< stripPrefix "dcx_field_data_field_price_pl_" (table :: Text)

                  ]

  return $ IndexCache salesTypes priceListNames supplierNames webPriceList
  

  
-- ** StyleAdjustment
getAdjustBase :: Handler (IndexCache -> ItemInfo (ItemMasterAndPrices Identity) -> Text -> ItemInfo (ItemMasterAndPrices Identity))
getAdjustBase = do
  settings <- appSettings <$> getYesod
  basePl <- basePriceList
  let varMap = appVariations settings
      go cache item0@(ItemInfo style _ master ) var = let
        stock = impMaster master
        salesPrices = impSalesPrices master
        purchasePrices = impPurchasePrices master
        theoreticalPrices = computeTheoreticalPricesF basePl -- (icBasePriceList cache)
                                                    (icPriceLists cache)
                                                    <$> salesPrices 
        adj = adjustDescription varMap (iiVariation item0) var
        sku = styleVarToSku style var
        in item0  { iiInfo = master
                    { impMaster = (\s -> s {smfDescription = smfDescription s <&> adj}) <$> stock
                    , impSalesPrices = (fmap (\p -> p { pfStockId = Identity sku })
                                       ) <$> salesPrices
                    , impPurchasePrices = (fmap (\p -> p { pdfStockId = Identity sku })
                                       ) <$> purchasePrices
                    -- web prices should be base not on the web prices
                    -- but on the price list
                    , impWebPrices = Just $ (fromMaybe mempty theoreticalPrices)
                                          <>  (fromMaybe mempty (impWebPrices master))
                    }
                  }
  when (null varMap ) $ do
       setWarning "No variations have been defined. Pleasec contact your adminstrator."
  return go

-- Replace all occurrences of a variation name in the description
-- for example "Red" "Black T-Shirt" => "Red T-Shirt"
adjustDescription :: Map Text Text -> Text -> Text -> Text -> Text
adjustDescription varMap var0 var desc =
  case (lookupVars varMap var0, lookupVars varMap var) of
    ([],_) -> desc
    (_, []) -> desc
    (vnames0, vnames) -> let
      endos = [ (Endo $ replace (f vnames0) (f vnames))
              | f0 <- [toTitle, toUpper, toLower ]
              , let f vs = varsToVariation (map f0 vs)
              ]
      in appEndo (mconcat endos) desc
-- * Load DB 
-- ** StockMaster info
loadVariations :: IndexCache -> IndexParam
               -> Handler [ (ItemInfo (ItemMasterAndPrices Identity) -- base
                            , [ ( VariationStatus
                                , ItemInfo (ItemMasterAndPrices ((,) [Text]))
                                )
                              ] -- all variations, including base
                            )
                          ]
loadVariations cache param = do
  -- pre cache variations parts
  let forceCache = ipClearCache param
  delayeds <- mapM (\(p,a) -> preCache0 forceCache
                                        (cacheDelay) (p
                                           , param { ipMode = ItemPriceView
                                                  , ipChecked = []
                                                  , ipBases = mempty
                                                  }
                                           ) (runDB $ a param))
           [ ("prices", loadSalesPrices)
           , ("purchase", loadPurchasePrices)
           , ("fa status", loadStatus)
           , ("web status", loadWebStatus)
           , ("web prices", loadWebPrices cache)
           ]
  -- mapM_ startDelayed delayeds
  let [ delayedSalesPrices , delayedPurchasePrices
        , delayedStatus
        , delayedWebStatus
        , delayedWebPrices 
        ] = delayeds

                        
  let varF = ipVariations param
      bases =  ipBases param
  adjustBase <- getAdjustBase
  varGroupMap <- appVariationGroups <$> appSettings <$> getYesod
  
  styles <- case styleFilter param of
    Left err -> do
              setWarning err
              return []
    Right styleF -> let select = selectList styleF [Asc FA.StockMasterId]
                    in cache0 forceCache (cacheDelay) ("load styles", ipStyles param) (runDB select)
  variations <- case varF of
    (Right Nothing) -> return (Left $ map  entityKey styles)
    (Left filter_)  -> let select = (Left . map entityKey) <$> runDB (selectList -- selectKeysList bug. fixed but not in current LTS
                                    (filterE StockMasterKey FA.StockMasterId (Just filter_)
                                     <> [FA.StockMasterInactive ==. False ]
                                    )
                                          [Asc FA.StockMasterId])
                       in cache0 forceCache (cacheDelay) (filter_, "load variations") select
    (Right (Just group_)) -> return $ Right (Map.findWithDefault [] group_ varGroupMap)
  infoSources <- mapM getDelayed (case ipMode param of
    ItemGLView -> []
    ItemPriceView -> [delayedSalesPrices]
    ItemPurchaseView -> [delayedPurchasePrices]
    ItemAllView -> [delayedSalesPrices, delayedPurchasePrices]
    ItemWebStatusView -> [delayedSalesPrices, delayedStatus, delayedWebStatus, delayedWebPrices]
    )

  let itemStyles = mergeInfoSources ( map stockItemMasterToItem styles
                                    : infoSources
                                    )
      itemVars =  case variations of
        Left keys -> map (snd . skuToStyleVar . unStockMasterKey) keys
        Right vars -> vars
      itemGroups = joinStyleVariations (skuToStyleVar <$> bases)
                                        (adjustBase cache) computeDiff
                                        itemStyles itemVars
      filterExtra = if ipShowExtra param
                    then id
                    else (List.filter ((/= VarExtra) . fst))

  let result =  map (\(base, vars)
                  -> (base, filterExtra vars)
                ) itemGroups
  mapM_ startDelayed delayeds
  return result

    
-- ** Sales prices
-- | Load sales prices 
    -- loadSalesPrices :: IndexParam -> Handler [ItemInfo (ItemMasterAndPrices Identity)]
loadSalesPrices :: IndexParam -> SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadSalesPrices param = do
  case (ipStyles param) of
     Just styleF -> do
       let sql = "SELECT ?? FROM 0_prices JOIN 0_stock_master USING(stock_id)"
            <> " WHERE curr_abrev = 'GBP' AND " <> stockF <> inactive
            <> " ORDER BY stock_id"
           (fKeyword, p) = filterEKeyword styleF
           stockF = "stock_id " <> fKeyword <> "?"
           inactive =  if ipShowInactive param
                       then ""
                       else " AND inactive = 0"
       do
           prices <- rawSql sql [PersistText p]

           let group_ = groupBy ((==) `on `priceStockId) (map entityVal prices)
               maps = map (\priceGroup@(one:_) -> let
                              pricesF = mapFromList [ ( priceSalesTypeId p
                                                      , runIdentity $ aPriceToPriceF p
                                                      )
                                                    | p <- priceGroup
                                                    ]
                              (style, var) = skuToStyleVar (priceStockId one)
                              master = mempty { impSalesPrices = Just pricesF }
                              in ItemInfo style var master
                          ) group_

           return maps
          

            
     _ -> return []

-- | get selected price list ids
priceListsToKeep :: IndexCache -> IndexParam -> Handler [Key SalesType]
priceListsToKeep cache params = do
  let plIds =  map (entityKey) (icPriceLists cache)
  return $ filter (\(SalesTypeKey i) -> (priceColumnCheckId i) `elem` ipColumns params) plIds
  
-- ** Purchase prices
-- | Load purchase prices
-- loadPurchasePrices :: IndexParam -> Handler [ ItemInfo (Map Text Double) ]
loadPurchasePrices :: IndexParam -> SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadPurchasePrices param = do
  case (ipStyles param) of
     Just styleF -> do
       let sql = "SELECT ?? FROM 0_purch_data JOIN 0_stock_master USING(stock_id)"
            <> " WHERE " <> stockF <> inactive
            <> " ORDER BY stock_id"
           (fKeyword, p) = filterEKeyword styleF
           stockF = "stock_id " <> fKeyword <> "?"
           inactive =  if ipShowInactive param
                       then ""
                       else " AND inactive = 0"
       do
           prices <- rawSql sql [PersistText p]

           let group_ = groupBy ((==) `on `purchDataStockId) (map entityVal prices)
               maps = map (\priceGroup@(one:_) -> let
                              pricesF = mapFromList [ ( purchDataSupplierId p
                                                      , runIdentity $ aPurchDataToPurchDataF p
                                                      )
                                                    | p <- priceGroup
                                                    ]
                              (style, var) = skuToStyleVar (purchDataStockId one)
                              master = mempty { impPurchasePrices = Just pricesF }
                              in ItemInfo style var master
                          ) group_

           return maps
          

            
     _ -> return []
  
suppliersToKeep :: IndexCache -> IndexParam -> Handler [Key Supplier]
suppliersToKeep cache params = do
  let suppIds = keys (icSupplierNames cache)
  return $ (map SupplierKey) (filter (\i -> (supplierColumnCheckId i) `elem` ipColumns params) (suppIds))

-- ** FA Status
-- | Load item status, needed to know if an item can be deactivated or deleted safely
-- This includes if items have been even ran, still in stock, on demand (sales) or on order (purchase)
loadStatus :: IndexParam ->  SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadStatus param = do 
  case (ipStyles param) of
    Just styleF -> do
      let sql = "SELECT stock_id, COALESCE(qoh, 0), COALESCE(all_qoh, 0)"
              <> " , COALESCE(on_demand, 0), COALESCE(all_on_demand, 0) "
              <> " , COALESCE(on_order,0) "
              <> " , ordered, demanded "
              <> " FROM 0_stock_master  "
              <> " LEFT  JOIN (SELECT stock_id, SUM(qty*stock_weight) as qoh, SUM(qty) as all_qoh"
              <> "       FROM 0_stock_moves JOIN 0_locations USING(loc_code) "
              <> "       GROUP BY stock_id"
              <> "      ) qoh USING(stock_id)"
              <> " LEFT JOIN (SELECT stk_code stock_id, SUM((quantity-qty_sent)*order_weight) as on_demand "
              <> "                                    , SUM((quantity-qty_sent)) as all_on_demand "
              <> "                                    , 1 AS demanded "
              <> "       FROM 0_sales_order_details "
              <> "       JOIN 0_sales_orders USING (order_no, trans_type)"
              <> "       JOIN 0_locations ON(from_stk_loc = loc_code) "
              <> "       WHERE trans_type = 30 "
              <> "              AND expiry_date > (NOW())" -- filter expired order
              <> "       GROUP BY stk_code"
              <> "      ) demand USING (stock_id)"
              <> " LEFT JOIN (SELECT item_code AS stock_id "
              <> "                   , SUM(quantity_ordered-quantity_received) on_order"
              <> "                   , 1 AS ordered "
              <> "            FROM 0_purch_order_details "
              <> "            GROUP BY stock_id"
              <> "           ) on_order USING (stock_id)"
              <> " WHERE stock_id " <> fKeyword <> "?"
              <> inactive
          (fKeyword, p) = filterEKeyword styleF
          inactive =  if ipShowInactive param
                       then ""
                       else " AND inactive = 0"
      rows <- rawSql sql [PersistText p]
      return [ ItemInfo style var master
             | (Single sku, Single qoh, Single allQoh
               , Single onDemand, Single allOnDemand, Single onOrder
               , Single ordered, Single demanded
               ) <- rows
             , let (style, var) = skuToStyleVar sku
             , let used = (demanded <|> ordered) == Just True
             , let status = ItemStatusF (pure qoh) (pure allQoh)
                                       (pure onDemand) (pure allOnDemand)
                                       (pure onOrder)
                                       (pure used)
             , let master = mempty { impFAStatus = Just status}
             ]
    _ -> return []

-- ** Web Status
    
-- | Load Web Status, if item exists, have a product display, activated and the prices
loadWebStatus :: IndexParam -> SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadWebStatus param = do
  case (ipStyles param) of
    Just styleF -> do
      let sql = "SELECT sku, product.status, dcx_node.title"
             <> " FROM dcx_commerce_product AS product "
             <> " LEFT JOIN dcx_field_data_field_product ON (product_id = field_product_product_id"
             <> "                                                     AND  entity_type = 'node')"
             <> " LEFT JOIN dcx_node ON (entity_id = nid) "
             <> " WHERE sku " <> fKeyword <> " ?"
          (fKeyword, p) = filterEKeyword styleF
      rows <- rawSql sql [PersistText p]
      return [ItemInfo style var master
             | (Single sku, Single active, Single display) <- rows
             , let (style, var) = skuToStyleVar sku
             , let webStatus = ItemWebStatusF (pure display)  (pure active)
             , let master = mempty { impWebStatus = Just webStatus}
             ]
    
         
    _ -> return []

-- ** Web Prices
loadWebPrices :: IndexCache -> IndexParam -> SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadWebPrices cache param = do
  case (ipStyles param) of
    Just styleF ->  do
       -- individual prices For each sku
       sku'prices <- mapM (loadWebPriceFor (filterEKeyword styleF)) (icWebPriceList cache)
       let style'var'pricesMap  :: Map Text (ItemPriceF Identity)
           style'var'pricesMap  = Map.fromListWith (<>) (concat sku'prices)
       return [ ItemInfo style var (mempty {impWebPrices = Just prices})
              | (sku, prices) <- Map.toList style'var'pricesMap
              , let (style, var) = skuToStyleVar sku
              ]

    _ -> return []


-- webPriceList :: Int -> Text
webPriceList pId = pack $ printf "pl_%0.2d" pId
loadWebPriceFor ::  _ -> Int -> SqlHandler [(Text, ItemPriceF Identity )]
loadWebPriceFor (fKeyword, p) pId = do 
  let sql =  " SELECT sku, field_price_" <> webPriceList pId <> "_amount"
          <> " FROM dcx_commerce_product AS product "
          <> " JOIN dcx_field_data_field_price_" <> webPriceList pId <> " AS price"
          <> "      ON (price.entity_id = product_id AND type = 'product')"
          <> " WHERE sku " <> fKeyword <> " ?"
  rows <- rawSql sql [PersistText p]
  return [ (sku, webPrices)
         | (Single sku, Single price) <- rows
         , let webPrices = ItemPriceF (mapFromList [(fromIntegral pId, Identity (price /100))])
         ]
  
loadVariationsToKeep :: IndexCache -> IndexParam
               -> Handler [ (ItemInfo (ItemMasterAndPrices Identity) -- base
                            , [ ( VariationStatus
                                , ItemInfo (ItemMasterAndPrices ((,) [Text]))
                                )
                              ] -- all variations, including base
                            )
                          ]
loadVariationsToKeep cache params = do
  itemGroups <- loadVariations cache params
  let checkSku = checkFilter params 
      toKeep (_,info) = checkSku sku where sku = styleVarToSku (iiStyle info) (iiVariation info)
  return $  (filter toKeep) <$$> itemGroups

-- * Misc
-- ** Style names conversion

skuToStyleVar :: Text -> (Text, Text)
skuToStyleVar sku = (style, var) where
  style = take 8 sku
  var = drop 9 sku

styleVarToSku :: Text -> Text -> Text
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

-- ** Type conversions
stockItemMasterToItem :: (Entity FA.StockMaster) -> ItemInfo (ItemMasterAndPrices Identity)
stockItemMasterToItem (Entity key val) = ItemInfo  style var master where
            sku = unStockMasterKey key
            (style, var) = skuToStyleVar sku
            master = mempty { impMaster = Just (runIdentity (aStockMasterToStockMasterF val)) }

stockMasterToItemCode :: Entity StockMaster -> ItemCode
stockMasterToItemCode (Entity stockIdKey StockMaster{..}) = let
  sku = unStockMasterKey stockIdKey
  itemCodeItemCode = sku
  itemCodeStockId = sku
  itemCodeDescription=stockMasterDescription
  itemCodeCategoryId = stockMasterCategoryId
  itemCodeQuantity = 1
  itemCodeIsForeign = False
  itemCodeInactive = stockMasterInactive
  in ItemCode{..}

-- ** Helpers
-- | Lookup for list of variation code
lookupVars :: Map Text Text -> Text -> [Text]
lookupVars varMap = mapMaybe (flip Map.lookup varMap) . variationToVars


-- ** Table Infos
-- | List or columns for a given mode
columnsFor :: IndexCache -> ItemViewMode -> [ItemInfo (ItemMasterAndPrices f)] -> [IndexColumn]
columnsFor _ ItemGLView _ = map GLColumn cols where
  cols = [ "categoryId"
         , "taxTypeId"
         , "description"
         , "longDescription"
         , "units"
         , "mbFlag"
         , "salesAccount"
         , "cogsAccount"
         , "inventoryAccount"
         , "adjustmentAccount"
         , "assemblyAccount"
         , "dimensionId"
         , "dimension2Id"
           -- , "actualCost"
           -- , "lastCost"
           -- , "materialCost"
           -- , "labourCost"
           -- , "overheadCost"
         , "inactive"
         , "noSale"
         , "editable"
         ] :: [Text]
columnsFor _ ItemPriceView infos = map PriceColumn cols where
  cols = salesPricesColumns $ map  iiInfo  infos
columnsFor _ ItemPurchaseView infos = map PurchaseColumn cols where
  cols = purchasePricesColumns $ map  iiInfo  infos
columnsFor cache ItemWebStatusView _ =
  map FAStatusColumn fas
  <> map WebStatusColumn webs
  <> map WebPriceColumn webPrices
  where
  fas = [ "Quantity On Hand"
        , "Quantity On Hand (all)"
        , "On Demand"
        , "On Demand (all)"
        , "On Order"
        , "Status"
        ]
  webs = ["Web Status", "Product Display"]
  -- union of FA prices list AND web prices
  -- so we also show missing prices list
  webPrices = List.nub . sort $ (keys (icPriceListNames cache)) <> (icWebPriceList cache)

columnsFor _ ItemAllView _ = []


itemsTable :: IndexCache -> IndexParam ->  Handler Widget
itemsTable cache param = do
  let checkedItems = if null (ipChecked param) then Nothing else Just (ipChecked param)
  renderUrl <- getUrlRenderParams
  itemGroups <- loadVariations cache param

  let allItems = [ items | (_, varStatus) <- itemGroups  , (_, items) <- varStatus]
  -- don't display purchase prices if the user is not authorized
  when (ipMode param == ItemPurchaseView) $ do
    settings <- appSettings <$> getYesod
    mu <- maybeAuth
    let role = roleFor (appRoleFor settings) (userIdent . entityVal <$> mu)
    when (not . null $ filterPermissions ReadRequest (setFromList ["purchase"]) role) $ do
      permissionDenied "Can't acces Purchase prices"






  let columns = [CheckColumn, RadioColumn, StockIdColumn, StatusColumn] ++ columnsFor cache (ipMode param) allItems

  -- Church encoding ?
  let itemToF :: ItemInfo (ItemMasterAndPrices Identity)
              -> (VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text])))
              -> (IndexColumn -> Maybe (Html, [Text]) -- Html + classes per column
                , [Text]) -- classes for row

      itemToF item0 (status , ItemInfo style var master) =
        let sku =  styleVarToSku style var
            checked = maybe True (sku `elem`) checkedItems
            missingLabel = case status of
                                  VarMissing -> [shamlet| <span.label.label-warning data-label=missing> Missing |]
                                  VarExtra -> [shamlet| <span.label.label-info data-label=extra> Extra |]
                                  VarOk -> [shamlet||]
            val col = case col of
                        CheckColumn ->
                          Just ([], [shamlet|<input type=checkbox name="check-#{sku}" :checked:checked>|])
                        RadioColumn ->
                          let rchecked = var == iiVariation item0
                          in if status == VarMissing
                            then Just ([], missingLabel)
                            else Just ([], [shamlet|<input type=radio name="base-#{style}" value="#{sku}"
                                                :rchecked:checked
                                            >|] >> missingLabel)
                        StockIdColumn               ->
                          let route = ItemsR $ ItemsHistoryR sku
                          in Just ([], [hamlet|<a href=@{route} target="_blank">#{sku}|] renderUrl )
                        StatusColumn ->
                          let label = case differs of
                                                True -> [shamlet| <span.label.label-danger data-label=diff
                                                                  data-toggle="tooltip" title="The item differs from its selected base or expected state."
                                                                  > Diff |]
                                                _    -> [shamlet||]
                          in Just ([], [hamlet|#{label}|] renderUrl )
              
                        GLColumn name ->  columnForSMI name =<< impMaster master
                        PriceColumn i -> columnForPrices i =<< impSalesPrices master 
                        PurchaseColumn i -> columnForPurchData i =<< impPurchasePrices master 
                        FAStatusColumn name -> columnForFAStatus name =<< impFAStatus master
                        WebStatusColumn name -> columnForWebStatus name (impWebStatus master)
                        WebPriceColumn i -> columnForWebPrice i =<< impWebPrices master

            differs = or diffs where
              diffs = [ "text-danger" `elem `kls
                      | col <- columns
                      , let kls = maybe [] fst (val col)
                      ]
            classes :: [Text]
            classes = ("style-" <> iiStyle item0)
                      : (if differs then ["differs"] else ["no-diff"])
                      <> (if checked then [] else ["unchecked"])
                      <> case smfInactive <$> impMaster master of
                          Just (_, True) -> ["fa-inactive"] -- , "text-muted"]
                          _ -> []
                      -- ++ case status of
                      --     VarOk -> []
                      --     VarMissing -> ["danger"]
                      --     VarExtra -> ["info"]
                    ++ if var == iiVariation item0
                        then ["base"]
                        else ["variation"]

        in (\col -> fmap (\(fieldClasses, v)
                    -> (v, ("stock-master-"<> columnClass col):fieldClasses)
                      ) (val col)
          , classes
          )


      -- We keep row grouped so we can change the style of the first one of every group.
      rowGroup = map (\(base, vars) -> map (itemToF base) vars) itemGroups
      styleFirst ((fn, klasses):rs) = (fn, "style-start":klasses):rs
      styleFirst [] = error "Shouldn't happend"

      styleGroup klass rs = [(fn, klass:klasses) | (fn,klasses) <- rs]

  columnToTitle <- getColumnToTitle cache param
        

  return $ displayTable columns columnToTitle
                        (concat  (zipWith styleGroup (List.cycle ["group-2","group-3"])
                                    .map styleFirst $ rowGroup))
                      

-- *** columns
columnForSMI :: Text -> (StockMasterF ((,) [Text])) -> Maybe ([Text], Html)
columnForSMI col stock =
  case col of 
    "categoryId"             -> Just (toHtml . tshow <$> smfCategoryId stock )
    "taxTypeId"              -> Just $ toHtml <$> smfTaxTypeId stock 
    "description"            -> Just $ toHtml <$>  smfDescription stock 
    "longDescription"        -> Just $ toHtml <$> smfLongDescription stock 
    "units"                  -> Just $ toHtml <$>  smfUnits stock 
    "mbFlag"                 -> Just $ toHtml <$>  smfMbFlag stock 
    "salesAccount"           -> Just $ badgify "saccount" <$>  smfSalesAccount stock 
    "cogsAccount"            -> Just $ badgify "caccount" <$>  smfCogsAccount stock 
    "inventoryAccount"       -> Just $ badgify "siaccount" <$>  smfInventoryAccount stock 
    "adjustmentAccount"      -> Just $ badgify "aaccount" <$>  smfAdjustmentAccount stock 
    "assemblyAccount"        -> Just $ badgify "asaccount" <$>  smfAssemblyAccount stock 
    "dimensionId"            -> Just $ badgify "dim1" . tshowM  <$> smfDimensionId stock 
    "dimension2Id"           -> Just $ badgify "dim2" . tshowM <$> smfDimension2Id stock 
    "actualCost"             -> Just $ toHtml <$> smfActualCost stock 
    "lastCost"               -> Just $ toHtml <$> smfLastCost stock 
    "materialCost"           -> Just $ toHtml <$> smfMaterialCost stock 
    "labourCost"             -> Just $ toHtml <$> smfLabourCost stock 
    "overheadCost"           -> Just $ toHtml <$> smfOverheadCost stock 
    "inactive"               -> Just $ badgify "active" . toInactive <$> smfInactive stock 
    "noSale"                 -> Just $ toHtml <$> smfNoSale stock 
    "editable"               -> Just $ toHtml <$> smfEditable stock 
    _ -> Nothing
  where
    badgify :: Text -> Text -> Html
    badgify label str = [shamlet|<span data-label=#{label}-#{str}>#{str}|]
    toInactive False = "Active"
    toInactive True = "Inactive"

columnForPrices :: Int -> (IntMap (PriceF ((,) [Text]))) -> Maybe ([Text], Html)
columnForPrices colInt prices = do -- Maybe
  value <- IntMap.lookup colInt prices
  return $ toHtml . tshow <$> pfPrice value where

columnForPurchData :: Int -> (IntMap (PurchDataF ((,) [Text]))) -> Maybe ([Text], Html)
columnForPurchData colInt purchData = do -- Maybe
  value <- IntMap.lookup colInt purchData
  return $ toHtml . tshow <$> pdfPrice value where

columnForFAStatus :: Text -> (ItemStatusF ((,) [Text])) -> Maybe ([Text], Html)
columnForFAStatus col iStatus@ItemStatusF{..} =
  case col of
    "Quantity On Hand" -> Just (toHtml . tshow <$> isfQoh)
    "Quantity On Hand (all)" -> Just (toHtml . tshow <$> isfAllQoh)
    "On Demand" -> Just (toHtml . tshow <$> isfOnDemand)
    "On Demand (all)" -> Just (toHtml . tshow <$> isfAllOnDemand)
    "On Order" -> Just (toHtml . tshow <$> isfOnOrder)
    "Status" -> Just (statusBadge <$> faRunningStatus iStatus )
    _ -> Nothing
  where statusBadge st = case st of
          FARunning ->  [shamlet|<span.label.label-success data-label=running
                              data-toggle="tooltip" title="#{running}"
                                >Running|]
          FAAsleep ->  [shamlet|<span.label.label-warning data-label=asleep
                              data-toggle="tooltip" title="#{asleep}"
                               >Asleep|]
          FADead ->  [shamlet|<span.label.label-danger data-label=dead
                              data-toggle="tooltip" title="#{dead}"
                              >Dead|]
          FAGhost ->  [shamlet|<span.label.label-info data-label=ghost
                              data-toggle="tooltip" title="#{ghost}"
                              >Ghost|]
        running = "The item is used, in stock, on demand or and order." :: Text
        asleep = "Not used but still present in cancelled or expired order/location. Probably needs tidying up before being deleted." :: Text
        dead = "Not used anymore but can't be deleted because it has been used in previous transaction." :: Text
        ghost = "The item exists in the system but is not used whatsoever. It could be safely deleted" :: Text


columnForWebStatus :: Text -> Maybe (ItemWebStatusF ((,) [Text])) -> Maybe ([Text], Html)
columnForWebStatus col wStatusM =
  let w  = (iwfActive `traverse` wStatusM)
  in case col of
    "Web Status" -> let
      in  Just (showActive <$>  w)
    "Product Display" ->
      case sequence w of
        Just (_, True) ->  Just $ maybe ([], [shamlet|<span.label.label-danger data-label="unlinked"
                                                      data-toggle="tooltip" title="The variation exists but is not linked to a product display."
                                                     >Unlinked|])
                                      (fmap toHtml)
                                      (sequence . iwfProductDisplay =<<  wStatusM) 
        Just (_, False) ->  Just $ maybe ([], [shamlet|<span.label.label-warning data-label="unliked-a">Inactive|])
                                      (fmap toHtml)
                                      (sequence . iwfProductDisplay =<<  wStatusM) 
        Nothing -> Nothing
    _ -> Nothing
  where showActive (Just True) = [shamlet|<span data-label="web-enabled"
                                          data-toggle="tooltip" title="The variation exists and will show on the product display page (if any)."
                                          >Enabled|]
        showActive (Just False) = [shamlet|<span data-label="web-disabled"
                                          data-toggle="tooltip" title="The variation exists but is hidden. It won't show on the product display page."
                                           >Disabled|]
        showActive Nothing = [shamlet|<span data-●●●●●●label="web-missing"
                                          data-toggle="tooltip" title="The variation doesn't exist. Can be created using <Create Missings> if needed."
                                      >Missing|]
  -- where showActive (Just True) = "Enabled"
  --       showActive (Just False) = "Disabled"
  --       showActive Nothing = "Missing"
 
columnForWebPrice :: Int -> ItemPriceF ((,) [Text]) -> Maybe ([Text], Html)
columnForWebPrice colInt (ItemPriceF priceMap) = do
  value <- IntMap.lookup colInt priceMap
  return $  toHtml . (\x -> x :: String) . printf "%.2f"  <$> value
  
-- * Rendering
renderButton :: IndexParam -> Text -> Button -> Html
renderButton param bclass button = case buttonStatus param button of
  BtnActive ->  [shamlet|
               <button.btn class="btn-#{bclass}" type="submit" name="button" value="#{tshow button}">#{buttonName button}
               |]
  BtnInactive tooltip ->  [shamlet|
                 <a href="#" data-toggle="tooltip" title="#{tooltip}">
                    <div.btn.disabled class="btn-#{bclass} type="submit" name="button" value="#{tshow button}">#{buttonName button}
               |]
  BtnHidden ->  [shamlet||]

buttonStatus :: IndexParam -> Button -> ButtonStatus
buttonStatus param CreateMissingBtn = case (ipMode param, ipShowInactive param) of
  (ItemGLView, False) -> BtnInactive "Please show inactive item before creating items. This is to avoid trying to create disabled items."
  _ -> BtnActive
 
buttonStatus param ActivateBtn = case ipMode param of
  ItemPriceView -> BtnHidden
  ItemPurchaseView -> BtnHidden
  _ -> BtnActive
buttonStatus param DeactivateBtn = case ipMode param of
  ItemPriceView -> BtnHidden
  ItemPurchaseView -> BtnHidden
  _ -> BtnActive
  
buttonStatus param DeleteBtn = BtnActive

buttonName :: Button -> Text
buttonName CreateMissingBtn = "Create Missings"
buttonName ActivateBtn = "Activate"
buttonName DeactivateBtn = "Deactivate"
buttonName DeleteBtn = "Delete"

  


renderIndex :: IndexParam -> Status -> Handler TypedContent
renderIndex param0 status = do
  (param, form, encType) <- getPostIndexParam param0
  cache <- fillIndexCache
  ix <- itemsTable cache param
  purchAuth <- purchaseAuth
  if (ipMode param == ItemPurchaseView && not purchAuth)
    then permissionDenied "Contact your system administrator"
    else return ()
  let css = [cassius|
#items-index
  tr.unchecked
    font-weight: normal
  tr
    font-weight: bold
  th
    writing-mode: sideways-lr
  .clickable
    cursor: pointer
  tr.base.group-1 
    background: #f2dede
  tr.base.group-2 
    background: #dff0d8
  tr.base.group-3 
    background: #d0edf7
  tr.base.group-4 
    background: #fcf8e3
  .base
    font-weight: 500
  tr.style-start
    border-top: 3px solid black
  tr.group-1
    border-left: solid #d0534f
  tr.group-2
    border-left: solid #93c54b
  tr.group-3
    border-left: solid #29abe0
  tr.group-4
    border-left: solid #f47c3c
  td.text-danger
    font-weight: bold
  td.stock-master-radio span.label-info
    font-size: 60%
  tr.fa-inactive
    font-style: italic
    background: #eee
|]
  let js = [julius|
$('[data-toggle="tooltip"]').tooltip();
|]
  let navs = filter (\n -> n /= ItemAllView
                           && ( n /= ItemPurchaseView || purchAuth -- don't display tab if not authorized
                              )
                    ) [minBound..maxBound] :: [ItemViewMode]
      mode = ipMode param
      navClass nav = if mode == nav then "active" else "" :: Html
  let main = [whamlet|
      ^{ix}
      <div.well>
        #{renderButton param "primary" CreateMissingBtn}
        #{renderButton param "primary" ActivateBtn} 
        #{renderButton param "warning" DeactivateBtn}
        #{renderButton param "danger" DeleteBtn}
        |]
  let widget = [whamlet|
<div #items-index>
  <form #items-form role=form method=post action=@{ItemsR (ItemsIndexR (Just mode))} enctype=#{encType}>
    <div.well>
      ^{form}
      <button type="submit" name="button" value="search" class="btn btn-default">Search
      <button type="submit" name="button" value="refresh" class="btn btn-info">Refresh Cache
    <ul.nav.nav-tabs>
      $forall nav <- navs
        <li class="#{navClass nav}">
          <a.view-mode href="#" data-url="@{ItemsR (ItemsIndexR (Just nav))}">#{drop 4 $ tshow nav}
    <div#items-table>
      ^{main}
|]
      fay = $(fayFile "ItemsIndex")
  selectRep $ do
    provideRep  $ do
      html <- sendResponseStatus status =<< defaultLayout (widget >> fay >> toWidget css >> toWidget js)
      return (html :: Html)
    provideRep $ do -- Ajax. return table
      table <- widgetToPageContent main
      html <- withUrlRenderer (pageBody table)
      returnJson (renderHtml html)
      
        
-- ** css classes
  
priceColumnCheckId i = tshow i
supplierColumnCheckId i = tshow i

getColumnToTitle :: IndexCache -> IndexParam -> Handler (IndexColumn -> (Html, [Text]))
getColumnToTitle cache param = do
  runDB $ do
    -- TODO only loads map if needed
    let priceListNames = icPriceListNames cache
        supplierNames = icSupplierNames cache
    let toh (Left r) = r
        toh (Right s) = (toHtml s, [])
        go column = let
          title = case column of
            CheckColumn -> Left ("", ["checkall"])
            RadioColumn -> Right ""
            StockIdColumn -> Right "Stock Id"
            StatusColumn -> Right "Status"
            GLColumn gl -> Right gl
            PriceColumn i -> case lookup i priceListNames of
                Nothing -> Right ""
                Just name -> let
                  checked = priceColumnCheckId i `elem` (ipColumns param)
                  in Left ([shamlet|#{name}<input type="checkbox" name="col-check-#{priceColumnCheckId i}" :checked:checked>|]
                          , ["price"]
                          )
            PurchaseColumn i -> case lookup i supplierNames of
                Nothing -> Right ""
                Just name -> let
                  checked = supplierColumnCheckId i `elem` (ipColumns param)
                  in Left ([shamlet|#{name}<input type="checkbox" name="col-check-#{supplierColumnCheckId i}" :checked:checked>|]
                          , ["purch_price"]
                          )
            FAStatusColumn t -> Right t
            WebStatusColumn t -> Right t
            WebPriceColumn i -> -- check if the prices list exists in FA or Web
              case lookup i priceListNames of
                Nothing -> Left (toHtml $ "#" <> tshow i, ["text-danger"]) -- only web
                Just name -> if i `List.elem` icWebPriceList cache
                             then Right name
                             else Left (toHtml name, ["text-danger"])
          in toh title
    return go

columnClass :: IndexColumn -> Text
columnClass col = filter (/= ' ') (tshow col)
  
-- * Actions
-- ** Missings
-- *** Actions
createMissing :: IndexParam -> Handler ()
createMissing params = do
   resp <- (case ipMode params of
              ItemWebStatusView -> createDCMissings
              _ -> createGLMissings
           ) params

   clearAppCache
   return resp

deleteItems :: IndexParam -> Handler ()
deleteItems params = do
  resp <- ( case ipMode params of
              ItemWebStatusView -> deleteDC params
              ItemPriceView -> deleteSalesPrices params
              ItemPurchaseView -> deletePurchasePrices params
              _ -> error "Should not happen"
          )
  clearAppCache
  return resp


-- ***  Gl
createGLMissings :: IndexParam -> Handler ()
createGLMissings params = do
  -- load inactive as well to avoid trying to create missing product
  -- If on the GL tab, create prices as well as items
  -- Otherwise, only create the data corresponding to the current tab (eg. sales/purchase prices)
  let newMode = case ipMode params of
                  ItemGLView -> ItemAllView
                  mode -> mode
  cache <- fillIndexCache
  itemGroups <- loadVariations cache (params {ipShowInactive = True, ipMode = newMode})
  let toKeep = checkFilter params

      missings = [ (sku, iiInfo info)
                 | (_, vars) <- itemGroups
                 , (status, info) <- vars
                 , let sku = styleVarToSku (iiStyle info) (iiVariation info)
                 ,  status == VarMissing
                 , toKeep sku
                 ]
  let stockMasters = [ Entity (StockMasterKey sku) var
                     | (sku, info) <- missings
                     , let tvarM = aStockMasterFToStockMaster <$> (impMaster info)
                     , isJust tvarM
                     , let Just (t,var) = tvarM
                     , let _types = t :: [Text] -- to help the compiler
                     ]
      -- prices = do -- []
      --   (_, info) <- missings
      --   case impSalesPrices info of
      --     Nothing -> []
      --     Just mprices -> do
      --       price <- IntMap.elems mprices
      --       let (t,p) = aPriceFToPrice price
      --           _types = t :: [Text]
      --       [p]
      -- for prices we need missing variations but also missing prices for existing variations
      prices = do -- []
         (_, vars) <- itemGroups
         (status, info) <- vars
         let sku = styleVarToSku (iiStyle info) (iiVariation info)
         guard (toKeep sku)
         mprices <- maybeToList (impSalesPrices $ iiInfo info)
         -- only keep prices if they are missing or new
         priceF <- IntMap.elems mprices
         let (t,price) = aPriceFToPrice priceF
             _types = t :: [Text]
         -- keep only new prices
         guard (status == VarMissing || "text-warning"  `elem` t)
         [price]
            -- keep 
        
      -- TODO factorize with salesPrices
      purchData = do -- []
         (_, vars) <- itemGroups
         (status, info) <- vars
         let sku = styleVarToSku (iiStyle info) (iiVariation info)
         guard (toKeep sku)
         mprices <- maybeToList (impPurchasePrices $ iiInfo info)
         -- only keep prices if they are missing or new
         priceF <- IntMap.elems mprices
         let (t,price) = aPurchDataFToPurchData priceF
             _types = t :: [Text]
         -- keep only new prices
         guard (status == VarMissing || "text-warning"  `elem` t)
         [price]

      -- new items also need an items codes
      itemCodes = map stockMasterToItemCode stockMasters
        
  clearAppCache
  runDB $ do
    insertEntityMany stockMasters
    insertMany_ itemCodes
    insertMany_ prices
    insertMany_ purchData

  setSuccess (toHtml $ tshow (maximumEx [length stockMasters, length prices, length purchData]) <> " items succesfully created.")
  return ()

deleteSalesPrices :: IndexParam -> Handler ()
deleteSalesPrices params = do
  cache <- fillIndexCache
  -- timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- loadVariationsToKeep cache (params {ipBases = mempty}) 
  plIds <- priceListsToKeep cache params
  -- delete all prices selected by the user for all given variations
  let deletePairs =
        [ [FA.PriceStockId ==. iiSku info, PriceSalesTypeId ==. unSalesTypeKey plId]
        | plId <- plIds
        , (_, items) <- itemGroups
        , (_, info) <- items
        ]

  runDB $ mapM_ deleteWhere deletePairs

deletePurchasePrices :: IndexParam -> Handler ()
deletePurchasePrices  params = do
  cache <- fillIndexCache
  -- timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- loadVariationsToKeep cache (params {ipBases = mempty}) 
  supplierIds <- suppliersToKeep cache params
  -- delete all prices selected by the user for all given variations
  let deletePairs =
        [ [FA.PurchDataStockId ==. iiSku info, PurchDataSupplierId ==. supplierId]
        | (SupplierKey supplierId) <- supplierIds
        , (_, items) <- itemGroups
        , (_, info) <- items
        ]

  runDB $ mapM_ deleteWhere deletePairs

-- *** Website
createDCMissings :: IndexParam -> Handler ()
createDCMissings params = do
  cache <- fillIndexCache
  basePl <- basePriceList
  timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- loadVariationsToKeep cache (params {ipShowInactive = True, ipBases = mempty}) 
  -- we need to create everything which is missing in the correct order
  let go (baseInfo, group') = runDB $ do
        -- find items with no product information in DC
        -- ie, ItemWebStatusF not present 
        -- also we can't create product which doesn't have a price
        -- let missingProduct group = isJust (_ group)
          let groupE = [ if toKeep
                         then Right (s'i, price)
                         else Left (iiSku info)
                       | s'i@(_, info) <- group'
                       , let mm = iiInfo info
                       , let toKeep = isNothing . join $ snd (iwfProductDisplay `traverse` impWebStatus mm)
                                      -- && not (isNothing (impMaster mm)) -- only to create new product
                       -- filter item with no base price
                       , Just price <- return $ masterPrice basePl mm
                       ]
          let group = rights groupE
          -- mapM (\sku -> lift $ setWarning (toHtml $ "Can't create " ++ sku ++ "as it doesn't exist in FA, or doesn't have a sale price. Please create it first.")
          --      ) (lefts groupE)
             

           
          product'revMap <- createMissingProducts cache group
          createMissingDCLinks cache (iiStyle $ baseInfo) (map fst group) product'revMap
          createMissingWebPrices cache (map fst group) product'revMap
          return ()
  mapM_ go itemGroups
  
deleteDC :: IndexParam -> Handler ()
deleteDC params = do
  cache <- fillIndexCache
  basePl <- basePriceList
  timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- loadVariationsToKeep cache (params {ipShowInactive = True, ipBases = mempty}) 

  let skus = [ iiSku info
             | (_, group) <- itemGroups
             , (_, info) <- group
             ] 

      go pId'revId = do
        -- skip deleteWebPrices
        -- skip deleteDCLinks
        deleteProduct pId'revId

  runDB $ do
        sku'pId'revIds <- loadSkuProductRevisions mempty skus
        let pId'revIds = [ (p,r) | (_, p, r) <- sku'pId'revIds ]
        mapM_ go pId'revIds
  
  
createMissingProducts
  :: IndexCache
  -> [((VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text]))), Double)]
  -> _ (Map Text (DC.CommerceProductTId, DC.CommerceProductRevisionTId))
createMissingProducts cache group = do
  timestamp <- round <$> liftIO getPOSIXTime
  colorMap <- lift loadDCColorMap
  -- find items with no product information in DC
  -- ie, ItemWebStatusF not present 
  -- let missingProduct group = isJust (_ group)
  let infos  = [ (info, price)
              | ((_, info), price) <- group
              , isNothing (impWebStatus (iiInfo info))
              ]
  let skus = map (iiSku.fst) infos
      prices = map snd infos
      colors = [ (fromMaybe (error "missing color") baseId, trimId)
               | (info, _) <- infos
               , let (base, trim ) = case variationToVars $ iiVariation info of
                                              [b] -> (b, Nothing)
                                              (b:trim:_) -> (b, Just trim)
                                              _ -> error "should not happen"
               , let baseId = Map.lookup base colorMap
               , let trimId = flip Map.lookup colorMap =<< trim
               ]
      bases = map fst colors
      trims = map snd colors
  productEntities <- createAndInsertNewProducts timestamp skus
  prod'revKeys <- createAndInsertNewProductRevisions timestamp productEntities
  let prod'revMKeys = Just <$$> prod'revKeys
  createAndInsertProductPrices prices prod'revMKeys
  createAndInsertProductColours bases prod'revMKeys
  createAndInsertProductTrimColours (trims) prod'revMKeys
  createAndInsertProductStockStatus prod'revMKeys
  return $ mapFromList (zip skus prod'revKeys)
        
-- deleteProduct :: (DC.CommerceProductTId, Maybe DC.CommerceProductRevisionTId) -> _ ()
deleteProduct p'r = do
  deleteProductStockStatus p'r
  deleteProductTrimColour p'r
  deleteProductColour p'r
  deleteProductPrice p'r
  deleteCommerceProduct p'r
  return ()


createAndInsertFields'
  :: _
  => [(Int, Maybe Int) -> r]
  -> [(DC.CommerceProductTId, Maybe DC.CommerceProductRevisionTId)]
  -> SqlHandler ()
createAndInsertFields' mks p'rKeys = do
  let p'rIds = [(pId, rId)
               | (DC.CommerceProductTKey pId, rM) <- p'rKeys
               , let rId = DC.unCommerceProductRevisionTKey <$> rM
               ]
  insertMany_ $ zipWith ($) mks p'rIds

createAndInsertRevFields' mks0 p'rKeys = do
  let (mks, p'rIds) = unzip [(mk, (pId, rId))
                            | (mk, (DC.CommerceProductTKey pId, rIdM)) <- zip mks0 p'rKeys
                            , Just (DC.CommerceProductRevisionTKey rId) <- return rIdM
                            ]
  insertMany_ $ zipWith ($) mks p'rIds
  
createAndInsertFields mk = createAndInsertFields' (repeat mk) 
createAndInsertRevFields mk = createAndInsertRevFields' (repeat mk) 

newProductField f (pId, revId) = f
 "commerce_product"
 "product"
 False
 pId
 revId
 "und"
 0

-- **** commerce_product
-- | Create and insert a new product in the corresponding table
createAndInsertNewProducts :: Int -> [Text] -> SqlHandler [Entity DC.CommerceProductT]
createAndInsertNewProducts timestamp skus =  do
      let products = map (newProduct timestamp) skus
      pIds <- insertMany products
      return $ zipWith Entity pIds products

-- | Create and insert a new product revision. Update the product table
-- accordingly
createAndInsertNewProductRevisions
  :: Int -> [Entity DC.CommerceProductT]
  -> SqlHandler [(DC.CommerceProductTId, DC.CommerceProductRevisionTId)]
createAndInsertNewProductRevisions timestamp pEntities = do
  let revs = map newProductRev  pEntities
  revIds <- insertMany revs
  let product'revIds = zip (map entityKey pEntities) revIds
  mapM_ updateProductWithRevision product'revIds
  return product'revIds

updateProductWithRevision
  :: (DC.CommerceProductTId, DC.CommerceProductRevisionTId)
  -> SqlHandler ()
updateProductWithRevision (pId, DC.CommerceProductRevisionTKey revId) = do
  update pId [DC.CommerceProductTRevisionId =. Just (revId)]
     

newProduct :: Int -> Text -> DC.CommerceProductT

newProduct timestamp sku = DC.CommerceProductT{..} where
  commerceProductTRevisionId = Nothing
  commerceProductTSku = sku
  commerceProductTTitle = sku
  commerceProductTType = "product"
  commerceProductTLanguage = ""
  commerceProductTUid = 1
  commerceProductTStatus = True -- Active
  commerceProductTCreated = timestamp
  commerceProductTChanged = timestamp
  commerceProductTData = Nothing -- "b:0;" -- Empty PHP encoding
        
newProductRev :: Entity DC.CommerceProductT -> DC.CommerceProductRevisionT
newProductRev (Entity pId DC.CommerceProductT{..}) = DC.CommerceProductRevisionT{..} where
  commerceProductRevisionTProductId = DC.unCommerceProductTKey pId
  commerceProductRevisionTSku = commerceProductTSku
  commerceProductRevisionTTitle = commerceProductTTitle
  commerceProductRevisionTRevisionUid = commerceProductTUid
  commerceProductRevisionTStatus = commerceProductTStatus
  commerceProductRevisionTLog = "Created by Fames"
  commerceProductRevisionTRevisionTimestamp = commerceProductTChanged
  commerceProductRevisionTData = commerceProductTData
  
deleteCommerceProduct (pId, mrevId) = do
  delete pId
  forM mrevId (\revId ->

                 delete revId
              )
-- **** field_*_field_price : main price. Should be Retail price
createAndInsertProductPrices :: [Double]
                             -> [(DC.CommerceProductTId, Maybe DC.CommerceProductRevisionTId)]
                             -> _
createAndInsertProductPrices prices p'rKeys = do
  let go mk = [ newProductPrice price mk
              | price <- prices
              ]
  let go' mk = [ newProductPrice price mk
              | price <- prices
              ]
  createAndInsertFields'  (go DC.FieldDataCommercePriceT)  p'rKeys
  createAndInsertRevFields'  (go' DC.FieldRevisionCommercePriceT)  p'rKeys
    

newProductPrice price mkPrice pr = newProductField mkPrice pr
 (round $ price * 100)
 "GBP"
 (Just  "a:1:{s:10:\"components\";a:0:{}}")

deleteProductPrice (pId,mrevId) = do
  deleteWhere  [ DC.FieldDataCommercePriceTEntityId ==. DC.unCommerceProductTKey pId
               , DC.FieldDataCommercePriceTEntityType ==. "commerce_product"
               ]
  forM mrevId (\revId -> 
                deleteWhere  [ DC.FieldRevisionCommercePriceTEntityId ==. DC.unCommerceProductRevisionTKey revId
                             , DC.FieldRevisionCommercePriceTEntityType ==. "commerce_product"
                             ]
             )
-- **** field*_field_colour
loadDCColorMap :: Handler (Map Text Int)
loadDCColorMap = cache0 False cacheForEver "dc-color-map" $ do
  let sql = "SELECT field_colour_code_value, entity_id "
          <>  "FROM dcx_field_data_field_colour_code "
          <>  "WHERE bundle = 'colours' "
          <>  "AND deleted = 0 "
  rows <- runDB $ rawSql sql []
  return $ mapFromList [ ( col, eid ) | ( Single col, Single eid ) <- rows ]

createAndInsertProductColours colours p'rKeys = do
  let go mk = [ newProductColour col mk
              | col <- colours
              ]
  let go' mk = [ newProductColour col mk
              | col <- colours
              ]
  createAndInsertFields' (go DC.FieldDataFieldColourT) p'rKeys
  createAndInsertRevFields' (go' DC.FieldRevisionFieldColourT) p'rKeys

deleteProductColour (pId,mrevId) = do
  deleteWhere  [ DC.FieldDataFieldColourTEntityId ==. DC.unCommerceProductTKey pId
               , DC.FieldDataFieldColourTEntityType ==. "commerce_product"
               ]
  forM mrevId (\revId -> 
                deleteWhere  [ DC.FieldRevisionFieldColourTEntityId ==. DC.unCommerceProductRevisionTKey revId
                             , DC.FieldRevisionFieldColourTEntityType ==. "commerce_product"
                             ]
             )

createAndInsertProductTrimColours colourMs p'rKeys0 = do
  -- we need to skip colours which are not present
  let (colours, p'rKeys) = unzip [(col, p'r)
                                 | (Just col, p'r) <- zip colourMs p'rKeys0
                                 ]
  let go mk = [ newProductColour col mk
              | col <- colours
              ]
  let go' mk = [ newProductColour col mk
              | col <- colours
              ]
  createAndInsertFields' (go DC.FieldDataFieldTrimColourT) p'rKeys
  createAndInsertRevFields' (go' DC.FieldRevisionFieldTrimColourT) p'rKeys

deleteProductTrimColour (pId,mrevId) = do
  deleteWhere  [ DC.FieldDataFieldTrimColourTEntityId ==. DC.unCommerceProductTKey pId
               , DC.FieldDataFieldTrimColourTEntityType ==. "commerce_product"
               ]
  forM mrevId (\revId -> 
                deleteWhere  [ DC.FieldRevisionFieldTrimColourTEntityId ==. DC.unCommerceProductRevisionTKey revId
                             , DC.FieldRevisionFieldTrimColourTEntityType ==. "commerce_product"
                             ]
             )

newProductColour colId mkColour pId'revId =
  newProductField mkColour pId'revId (Just colId)

-- **** field_*_field_stock_status 
createAndInsertProductStockStatus p'rKeys = do
  createAndInsertFields (newProductStockStatus (Just 70) DC.FieldDataFieldStockStatusT) p'rKeys
  createAndInsertRevFields (newProductStockStatus (Just 70) DC.FieldRevisionFieldStockStatusT) p'rKeys

newProductStockStatus status  mkStatus pId'revId =
  newProductField mkStatus pId'revId status
                      --       newFieldProduct productDisplayId productId delta = DC.FieldDataFieldProductT{..} where
          --        fieldDataFieldProductTEntityType = "node"
--        fieldDataFieldProductTBundle = "product_display"
--        fieldDataFieldProductTDeleted = False
--        fieldDataFieldProductTEntityId = productDisplayId
--        fieldDataFieldProductTRevisionId = Nothing
--        fieldDataFieldProductTLanguage = "und"
--        fieldDataFieldProductTDelta = delta
--        fieldDataFieldProductTFieldProductProductId = productId

--   runDB $ do
--     ids <- insertMany $ trace "new product" (map newProduct missings)
--     return ()
--     -- insertMany_ (map newProductRev missings)
  
deleteProductStockStatus (pId,mrevId) = do
  deleteWhere  [ DC.FieldDataFieldStockStatusTEntityId ==. DC.unCommerceProductTKey pId
               , DC.FieldDataFieldStockStatusTEntityType ==. "commerce_product"
               ]
  forM mrevId (\revId -> 
                deleteWhere  [ DC.FieldRevisionFieldStockStatusTEntityId ==. DC.unCommerceProductRevisionTKey revId
                             , DC.FieldRevisionFieldStockStatusTEntityType ==. "commerce_product"
                             ]
             )
  
-- **** field_*_field_product : link to product display
-- | Create product display link for all variations of a given style
createMissingDCLinks
  :: IndexCache
  -> Text -- ^ Style name, common to all variations
  -> [(VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text])))] -- ^ variations
  -> Map Text (DC.CommerceProductTId, DC.CommerceProductRevisionTId)
  -> SqlHandler ()
createMissingDCLinks cache style group p'rMap = do
  -- only keep item which doesn't have a produc display
  let skus = [ iiSku info
             | (status, info) <-  group
             , let mm = iiInfo info
             , isNothing $ join $ snd (iwfProductDisplay `traverse` impWebStatus mm) 
             ]
  -- the sku, pId , revId needs to be loaded from the database
  -- the one passed as argument are only there as cache but 
  (displayId, displayRev) <- loadProductDisplayInfo style
  lastDelta <- loadLastProductDelta displayId
  sku'p'rs0 <- loadSkuProductRevisions p'rMap skus
  let sku'p'rs = zip [lastDelta + 1   ..] sku'p'rs0
  createAndInsertDCLinks displayId displayRev sku'p'rs
  return ()
  

loadSkuProductRevisions
  :: Map Text (DC.CommerceProductTId, DC.CommerceProductRevisionTId)
  -> [Text]
  -> SqlHandler [(Text, DC.CommerceProductTId, Maybe DC.CommerceProductRevisionTId)]
loadSkuProductRevisions p'rMap skus  = do
  let go sku = do
        case Map.lookup sku p'rMap of
          Nothing -> do -- Find it from the database
            entities <- selectList [DC.CommerceProductTSku ==. sku] [LimitTo 1]
            return $ case entities of
                          [Entity pId product] -> let rev = DC.commerceProductTRevisionId product
                                                  in Just ( sku
                                                          , pId
                                                          , DC.CommerceProductRevisionTKey <$> rev )
                          _ -> Nothing
          Just (pId, revId) -> return $ Just (sku, pId, Just revId)

  resultMaybes <- mapM go skus
  return (catMaybes resultMaybes)

loadProductDisplayInfo
  :: Text -> SqlHandler (DC.NodeTId, Maybe DC.NodeRevisionTId)
loadProductDisplayInfo style = do
  pdKeys <- selectList [DC.NodeTTitle ==. style, DC.NodeTType ==. "product_display" ] []
  case pdKeys of
    [Entity nodeKey node] -> let nodeId = nodeKey
                                 revId = DC.NodeRevisionTKey <$> DC.nodeTVid node
                             in return (nodeId, revId)
    [] -> error $ "Product display not found for style :" ++ unpack style
    _ -> error $ "Too many nodes for style :" ++ unpack style
  
loadLastProductDelta
  :: DC.NodeTId
  -> SqlHandler Int
loadLastProductDelta displayId = do
      [(Single lastDelta)] <- rawSql ("SELECT MAX(delta) "
                           <> "FROM dcx_field_data_field_product "
                           <> "WHERE entity_id = ?"
                           )
                           [toPersistValue displayId]
      return (fromMaybe 0 lastDelta)

createAndInsertDCLinks
  :: DC.NodeTId
  -> Maybe DC.NodeRevisionTId
  -> [(Int, (Text, DC.CommerceProductTId, Maybe DC.CommerceProductRevisionTId))]
  -> SqlHandler ()
createAndInsertDCLinks displayId displayRev d's'p'r = do
  let go mk = [ newProductDCLink displayId displayRev mk d 
              | (d, _) <- d's'p'r
              ]
  let go' mk = [ newProductDCLink' displayId displayRev' mk d 
              | (d, _) <- d's'p'r
              , Just displayRev' <- return displayRev
              ]
      p'rS = [(p,r) | (_, (_,p,r)) <- d's'p'r]
  createAndInsertFields' (go DC.FieldDataFieldProductT) p'rS
  createAndInsertRevFields' (go' DC.FieldRevisionFieldProductT) p'rS
  
  
-- newProductDCLink
--   ::  DC.NodeTId
--   -> (Maybe DC.NodeRevisionTId)
--   -> (_)
--   -> Int
--   -> (Int, _ )
--   -> b
newProductDCLink displayId displayRev mk delta (pId, revId) = mk
  "node"
  "product_display"
  False
  (DC.unNodeTKey displayId)
  (DC.unNodeRevisionTKey <$> displayRev)
  "und"
  delta
  (Just pId)

newProductDCLink' displayId displayRev mk delta (pId, revId) = mk
  "node"
  "product_display"
  False
  (DC.unNodeTKey displayId)
  (DC.unNodeRevisionTKey displayRev)
  "und"
  delta
  (Just pId)
-- | Create missing link between product and product display.
-- createMissingDCLinksOldForStyle :: _ -> _ -> Handler ()
-- createMissingDCLinksOldForStyle toKeep (base, items) = runDB $ do
--   let missings = [ sku
--                  | (status, info) <-  items
--                  , let sku = styleVarToSku (iiStyle info) (iiVariation info)
--                  , toKeep sku
--                  , let mm = iiInfo info
--                  , isNothing $ join $ snd (iwfProductDisplay `traverse` impWebStatus mm) 
--                  ]
--   -- Get product display id
--   pdKeys <- selectKeysList [DC.NodeTTitle ==. iiStyle base] []
--   case pdKeys of
--     [] -> setError (toHtml $ "Product display " ++ (iiStyle base) ++ " missing from node table") >> return ()
--     [DC.NodeTKey pdId] -> do
--       let newFieldProduct productId delta = DC.FieldDataFieldProductT{..} where
--             fieldDataFieldProductTEntityType = "node"
--             fieldDataFieldProductTBundle = "product_display"
--             fieldDataFieldProductTDeleted = False
--             fieldDataFieldProductTEntityId = pdId
--             fieldDataFieldProductTRevisionId = Nothing
--             fieldDataFieldProductTLanguage = "und"
--             fieldDataFieldProductTDelta = delta
--             fieldDataFieldProductTFieldProductProductId = productId
--       pIds' <- forM missings $ \sku -> do
--         entities <- selectKeysList [DC.CommerceProductTSku ==. sku] []
--         case entities of
--           [DC.CommerceProductTKey pId] -> return (Just pId)
--           _ -> return Nothing
--       let pIds = map Just $ catMaybes pIds'
--       last_delta <- rawSql "SELECT MAX(delta) FROM dcx_field_data_field_product where entity_id = ?" [toPersistValue pdId]
--       let delta = case last_delta of
--             [Single (Just d)] -> d + 1
--             _ -> 1
           
            

--       zipWithM_  (\pId delta -> do
--                      insert_ $ trace ("inserting product field" ++ show (pId, delta)) (newFieldProduct pId delta)
--                  ) pIds [delta..]
        


      
    
-- **** Web Prices : different price lists

createMissingWebPrices
  :: IndexCache
  -> [(VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text])))] -- ^ variations
  -> Map Text (DC.CommerceProductTId, DC.CommerceProductRevisionTId)
  -> SqlHandler ()
createMissingWebPrices cache group p'revMap = do
  basePl <- lift basePriceList

  let sku'prices = [ (sku, priceMap)
           | (status, info) <- group
           , Just priceMap <- return $ impSalesPrices (iiInfo info)
           , let sku = iiSku info
           ]
      priceList = trace "got price list from cache " $ icPriceLists cache -- TODO move in global cache
      prices = map (computeTheoreticalPricesP basePl priceList . snd)  sku'prices
  sku'p'rs0 <- loadSkuProductRevisions p'revMap (map fst sku'prices)

  createAndInsertDCPrices prices sku'p'rs0
  
createAndInsertDCPrices priceMaps sku'p'rs = do
  let creators =
        [ createAndInsertDCPriceFor 1 DC.FieldDataFieldPricePl01T (DC.FieldRevisionFieldPricePl01T)
        , createAndInsertDCPriceFor 2 DC.FieldDataFieldPricePl02T DC.FieldRevisionFieldPricePl02T
        , createAndInsertDCPriceFor 3 DC.FieldDataFieldPricePl03T DC.FieldRevisionFieldPricePl03T
        , createAndInsertDCPriceFor 4 DC.FieldDataFieldPricePl04T DC.FieldRevisionFieldPricePl04T
        , createAndInsertDCPriceFor 5 DC.FieldDataFieldPricePl05T DC.FieldRevisionFieldPricePl05T
        , createAndInsertDCPriceFor 6 DC.FieldDataFieldPricePl06T DC.FieldRevisionFieldPricePl06T
        , createAndInsertDCPriceFor 7 DC.FieldDataFieldPricePl07T DC.FieldRevisionFieldPricePl07T
        , createAndInsertDCPriceFor 8 DC.FieldDataFieldPricePl08T DC.FieldRevisionFieldPricePl08T
        , createAndInsertDCPriceFor 9 DC.FieldDataFieldPricePl09T DC.FieldRevisionFieldPricePl09T
        , createAndInsertDCPriceFor 10 DC.FieldDataFieldPricePl10T DC.FieldRevisionFieldPricePl10T
        , createAndInsertDCPriceFor 11 DC.FieldDataFieldPricePl11T DC.FieldRevisionFieldPricePl11T
        , createAndInsertDCPriceFor 12 DC.FieldDataFieldPricePl12T DC.FieldRevisionFieldPricePl12T
        , createAndInsertDCPriceFor 13 DC.FieldDataFieldPricePl13T DC.FieldRevisionFieldPricePl13T
        , createAndInsertDCPriceFor 14 DC.FieldDataFieldPricePl14T DC.FieldRevisionFieldPricePl14T
        ]
      go creator = creator priceMaps sku'p'rs
        
  mapM_ go creators

createAndInsertDCPriceFor index mk mkRev priceMaps sku'p'rs = do
  let (prices, p'rs) = unzip [(price, (p,r))
                                 | (priceMap,  (_, p, r)) <- zip priceMaps sku'p'rs
                                 , Just price <- return $ lookup index priceMap
                                 ]
  createAndInsertFields' (map (newProductFieldPrice mk) prices) p'rs
  createAndInsertRevFields' (map (newProductFieldPrice mkRev) prices) p'rs

    
-- newProductFieldPrice :: Double ->
--   (Text -> Text -> Bool -> Int -> rev -> Text -> Int -> Int -> Maybe Text -> Maybe Text -> a) -> a
newProductFieldPrice mk price p'r = 
  newProductField mk p'r (round $ 100* price) ("GBP") Nothing
  

-- ** Activation
-- | Activates/deactivate and item in FrontAccounting.
changeFAActivation :: Bool -> IndexParam -> Handler ()
changeFAActivation activate param = do
  changeActivation activate param
                   [StockMasterInactive ==. activate]
                   (flip update [StockMasterInactive =. not activate])
  clearAppCache

changeActivation :: Bool -> IndexParam -> _ -> _ -> Handler ()
changeActivation activate param activeFilter updateFn =  runDB $ do
  -- we set ShowInactive to true to not filter anything yet using (StockMasterInactive)
  -- as it will be done before
  entities <- case styleFilter param {ipMode = ItemGLView, ipShowInactive = True } of
    Left err -> error err >> return []
    Right filter_ ->  selectList ( activeFilter ++ filter_) []
  let toKeep = checkFilter param
      keys_ = [ key
             | (Entity key _) <- entities
             , toKeep (unStockMasterKey key)
             ] :: [Key StockMaster]
  mapM_ updateFn keys_

-- | Activated and item in  FrontAccounting or the Website
activate :: IndexParam -> Handler ()
activate param = case (ipMode param ) of
  ItemWebStatusView -> changeWebActivation True param
  _ -> changeFAActivation True param

-- | deactivate and item in  FrontAccounting or the Website
deactivate :: IndexParam -> Handler ()
deactivate param = case (ipMode param ) of
  ItemWebStatusView -> changeWebActivation False param
  _ -> changeFAActivation False param
  
changeWebActivation :: Bool -> IndexParam -> Handler ()
changeWebActivation activate param = do
  muser <- maybeAuth
  timestamp <- round <$> liftIO getPOSIXTime
  let usertext = maybe "" (\u -> " by user:" <> userIdent (entityVal u)) muser
  changeActivation activate param [] $ \key -> do
    let sku = unStockMasterKey key
    --     sql = " UPDATE dcx_commerce_product p "
    --        <> " SET p.status = ?, p.changed = ?"
    --        <> " WHERE p.sku = ? "
    --     pactivate = PersistBool activate
    --     ptimestamp = PersistInt64 timestamp

    -- rawExecute sql [ pactivate, ptimestamp, PersistText sku]

    -- -- let sql_rev = " UPDATE dcx_commerce_product_revision r "
    -- --        <> " JOIN dcx_commerce_product p "
    -- --        <> " USING (product_id, revision_id) "
    -- --        <> " SET r.status = ? "
    -- --        <> "   , r.log = ?, r.revision_timestamp = ? "
    -- --        <> " WHERE p.sku = ? "

    -- -- rawExecute sql_rev [ pactivate
    -- --                , PersistText ("Updated by Fames" <> usertext)
    -- --                , ptimestamp 
    -- --                , PersistText sku
    -- --                ]
    products <- selectList [DC.CommerceProductTSku ==. sku] []
    let updateProduct (Entity pKey product) = do
          let pId = DC.unCommerceProductTKey pKey
          update pKey [ DC.CommerceProductTStatus =. activate
                     , DC.CommerceProductTChanged =. timestamp
                     ]

          -- update revision if any
          forM_ (DC.commerceProductTRevisionId product) $ \revId -> 
                updateWhere [ DC.CommerceProductRevisionTProductId ==. pId 
                            , DC.CommerceProductRevisionTId ==. DC.CommerceProductRevisionTKey revId
                            ]
                            [ DC.CommerceProductRevisionTStatus =. activate
                            , DC.CommerceProductRevisionTLog =. ("Updated by Fames" <> usertext)
                            , DC.CommerceProductRevisionTRevisionTimestamp =. timestamp
                            ]
    mapM_ updateProduct products
  
  
