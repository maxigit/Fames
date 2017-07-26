{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Index where

import Import hiding(replace)
import Handler.Table
import Yesod.Form.Bootstrap3
import FA
import Items
import Text.Blaze (Markup)
import qualified Data.Map as Map
import Data.Monoid(Endo(..), appEndo)
import Data.Text(toTitle, replace, splitOn)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import Database.Persist.MySQL hiding(replace)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
-- * Types
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data FilterExpression = LikeFilter Text  | RegexFilter Text deriving (Eq, Show, Read)
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
            deriving Show

-- * Handlers
getItemsIndexR :: Maybe ItemViewMode -> Handler TypedContent
getItemsIndexR mode = do
  renderIndex (paramDef mode) ok200

postItemsIndexR :: Maybe ItemViewMode -> Handler TypedContent
postItemsIndexR mode = do
  action <- lookupPostParam "button"
  -- traceShowM ("POST",mode, paramDef mode)
  (param,_,_) <- getPostIndexParam (paramDef mode)
  -- traceShowM ("button", action)
  case action of
    Just "create" ->  do
        createMissing param
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
-- ** Filtering Expressions (Like or Regexp)
showFilterExpression :: FilterExpression -> Text
showFilterExpression (LikeFilter t) = t
showFilterExpression (RegexFilter t) = "/" <> t


readFilterExpression :: Text -> FilterExpression
readFilterExpression t = case stripPrefix "/" t of
  Nothing -> LikeFilter t
  Just regex -> RegexFilter regex


instance IsString FilterExpression where
  fromString = readFilterExpression . fromString


filterEField :: (RenderMessage (HandlerSite m) FormMessage,
                  Monad m) =>
                Field m FilterExpression
filterEField = convertField readFilterExpression showFilterExpression textField


filterE :: PersistField a =>
           (Text -> a)
        -> EntityField record a
        -> Maybe FilterExpression
        -> [Filter record]
filterE _ _ Nothing = []
filterE conv field (Just (LikeFilter like)) = 
  [ Filter field
         (Left $ conv like)
         (BackendSpecificFilter "LIKE")
  ]
filterE conv field (Just (RegexFilter regex)) =
  [ Filter field
         (Left $ conv regex)
         (BackendSpecificFilter "RLIKE")
  ]
  
filterEKeyword ::  FilterExpression -> (Text, Text)
filterEKeyword (LikeFilter f) = ("LIKE", f)
filterEKeyword (RegexFilter f) = ("RLIKE", f)
-- ** Params and Forms
paramDef :: Maybe ItemViewMode -> IndexParam
paramDef mode = IndexParam Nothing Nothing Nothing False True
                           mempty empty empty
                           (fromMaybe ItemGLView mode)
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
        groups' =  map (\g -> (g,g)) groups

-- | Fill the parameters which are not in the form but have to be extracted
-- from the request parameters
fillTableParams :: IndexParam -> Handler IndexParam
fillTableParams params0 = do
  (params,_) <- runRequestBody
  let checked = mapMaybe (stripPrefix "check-" . fst)  params
      bases = Map.fromList $ mapMaybe (\(k,v) -> stripPrefix "base-" k <&> (\b -> (b, v))
                                     ) params
  return $ params0 {ipChecked = checked, ipBases=bases}
   

-- getPostIndexParam :: IndexParam -> Handler (IndexParam, _
getPostIndexParam param0 = do
  varGroup <- appVariationGroups <$> getsYesod appSettings
  ((resp, form), encType) <- runFormPost (indexForm (Map.keys varGroup) param0)
  let param1 = case resp of
        FormMissing -> param0
        FormSuccess par -> par
        FormFailure err ->  param0
  param <- fillTableParams param1
  return (param, form, encType)

-- ** StyleAdjustment
getAdjustBase :: Handler (ItemInfo (ItemMasterAndPrices Identity) -> Text -> ItemInfo (ItemMasterAndPrices Identity))
getAdjustBase = do
  settings <- appSettings <$> getYesod 
  let varMap = appVariations settings
      go item0@(ItemInfo style _ master ) var = let
        stock = impMaster master
        salesPrices = impSalesPrices master
        purchasePrices = impPurchasePrices master
        adj = adjustDescription varMap (iiVariation item0) var
        sku = styleVarToSku style var
        in item0  { iiInfo = master
                    { impMaster = (\s -> s {smfDescription = smfDescription s <&> adj}) <$> stock
                    , impSalesPrices = (fmap (\p -> p { pfStockId = Identity sku })
                                       ) <$> salesPrices
                    , impPurchasePrices = (fmap (\p -> p { pdfStockId = Identity sku })
                                       ) <$> purchasePrices
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
      -- replace' a b c = traceShow (a,b,c,d) d where d = replace a b c
      endos = [ (Endo $ replace (f vnames0) (f vnames))
              | f0 <- [toTitle, toUpper, toLower ]
              , let f vs = varsToVariation (map f0 vs)
              ]
      in appEndo (mconcat endos) desc
-- * Load DB 
-- ** StockMaster info
loadVariations :: IndexParam
               -> Handler [ (ItemInfo (ItemMasterAndPrices Identity) -- base
                            , [ ( VariationStatus
                                , ItemInfo (ItemMasterAndPrices ((,) [Text]))
                                )
                              ] -- all variations, including base
                            )
                          ]
loadVariations param = do
  let styleF = ipStyles param
      varF = ipVariations param
      bases =  ipBases param
  adjustBase <- getAdjustBase
  varGroupMap <- appVariationGroups <$> appSettings <$> getYesod
  runDB $ do
    let conv = StockMasterKey
    styles <- case styleF of
      Nothing -> do
                setWarning "Please enter a styles filter expression (SQL like expression or regexp starting with '/'')"
                return []
      Just _ -> selectList (filterE conv FA.StockMasterId styleF
                            ++ if (ipShowInactive param) then [] else [FA.StockMasterInactive ==. False]
                          )
                          [Asc FA.StockMasterId]
    variations <- case varF of
      (Right Nothing) -> return (Left $ map  entityKey styles)
      (Left filter_)  -> (Left . map entityKey) <$> selectList -- selectKeysList bug. fixed but not in current LTS
                                 (filterE conv FA.StockMasterId (Just filter_)
                                 <> [FA.StockMasterInactive ==. False ]
                                 )
                                            [Asc FA.StockMasterId]
      (Right (Just group_)) -> return $ Right (Map.findWithDefault [] group_ varGroupMap)
    salesPrices <- loadSalesPrices param
    purchasePrices <- loadPurchasePrices param
    itemStatus <- loadStatus param

    let itemStyles = mergeInfoSources [ map stockItemMasterToItem styles
                                      , salesPrices
                                      , purchasePrices
                                      , itemStatus
                                      ]
        itemVars =  case variations of
          Left keys -> map (snd . skuToStyleVar . unStockMasterKey) keys
          Right vars -> vars
        itemGroups = joinStyleVariations (skuToStyleVar <$> bases)
                                         adjustBase computeDiff
                                         itemStyles itemVars
        filterExtra = if ipShowExtra param
                      then id
                      else (List.filter ((/= VarExtra) . fst))
    return  $ map (\(base, vars)
                   -> (base, filterExtra vars)
                  ) itemGroups

    
-- ** Sales prices
-- | Load sales prices 
-- loadSalesPrices :: IndexParam -> Handler [ItemInfo (ItemMasterAndPrices Identity)]
loadSalesPrices :: (MonadIO m)
  => IndexParam -> ReaderT SqlBackend m [ItemInfo (ItemMasterAndPrices Identity)]
loadSalesPrices param = do
  case (ipStyles param) of
     Just styleF |  ipMode param `elem` [ItemPriceView, ItemAllView] -> do
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

-- ** Purchase prices
-- | Load purchase prices
-- loadPurchasePrices :: IndexParam -> Handler [ ItemInfo (Map Text Double) ]
loadPurchasePrices :: (MonadIO m)
  => IndexParam -> ReaderT SqlBackend m [ItemInfo (ItemMasterAndPrices Identity)]
loadPurchasePrices param = do
  case (ipStyles param) of
     Just styleF |  ipMode param `elem` [ItemPurchaseView, ItemAllView] -> do
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
           -- traceShowM("PURCH_DATA", sql, prices)

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
  
-- ** Status
-- | Load item status, needed to know if an item can be deactivated or deleted safely
-- This includes if items have been even ran, still in stock, on demand (sales) or on order (purchase)
loadStatus :: (MonadIO m)
           => IndexParam ->  ReaderT SqlBackend m [ItemInfo (ItemMasterAndPrices Identity)]
loadStatus param = do 
  case (ipStyles param) of
    Just styleF | ipMode param `elem` [ItemWebStatusView] -> do
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

-- * Misc
-- ** Style names conversion

skuToStyleVar :: Text -> (Text, Text)
skuToStyleVar sku = (style, var) where
  style = take 8 sku
  var = drop 9 sku

styleVarToSku :: Text -> Text -> Text
styleVarToSku style var = style <> "-" <> var

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
columnsFor :: ItemViewMode -> [ItemInfo (ItemMasterAndPrices f)] -> [IndexColumn]
columnsFor ItemGLView _ = map GLColumn cols where
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
columnsFor ItemPriceView infos = map PriceColumn cols where
  cols = salesPricesColumns $ map  iiInfo  infos
columnsFor ItemPurchaseView infos = map PurchaseColumn cols where
  cols = purchasePricesColumns $ map  iiInfo  infos
columnsFor ItemWebStatusView _ = map FAStatusColumn fas where
  fas = [ "Quantity On Hand"
        , "Quantity On Hand (all)"
        , "On Demand"
        , "On Demand (all)"
        , "On Order"
        , "Status"
        ]

columnsFor ItemAllView _ = []

itemsTable :: IndexParam ->  Handler Widget
itemsTable param = do
  let checkedItems = if null (ipChecked param) then Nothing else Just (ipChecked param)
  renderUrl <- getUrlRenderParams
  itemGroups <- loadVariations param

  let allItems = [ items | (_, varStatus) <- itemGroups  , (_, items) <- varStatus]
  -- don't display purchase prices if the user is not authorized
  when (ipMode param == ItemPurchaseView) $ do
    settings <- appSettings <$> getYesod
    mu <- maybeAuth
    let role = roleFor (appRoleFor settings) (userIdent . entityVal <$> mu)
    when (not . null $ filterPermissions ReadRequest (setFromList ["purchase"]) role) $ do
      permissionDenied "Can't acces Purchase prices"






  let columns = [CheckColumn, RadioColumn, StockIdColumn, StatusColumn] ++ columnsFor (ipMode param) allItems

  -- Church encoding ?
  let itemToF :: ItemInfo (ItemMasterAndPrices Identity)
              -> (VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text])))
              -> (IndexColumn -> Maybe (Html, [Text]) -- Html + classes per column
                , [Text]) -- classes for row

      itemToF item0 (status , ItemInfo style var master) =
        let sku =  styleVarToSku style var
            checked = maybe True (sku `elem`) checkedItems
            missingLabel = case status of
                                  VarMissing -> [shamlet| <span.label.label-warning> Missing |]
                                  VarExtra -> [shamlet| <span.label.label-info> Extra |]
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
                                                True -> [shamlet| <span.label.label-danger> Diff |]
                                                _    -> [shamlet||]
                          in Just ([], [hamlet|#{label}|] renderUrl )
              
                        GLColumn name ->  columnForSMI name =<< impMaster master
                        PriceColumn i -> columnForPrices i =<< impSalesPrices master 
                        PurchaseColumn i -> columnForPurchData i =<< impPurchasePrices master 
                        FAStatusColumn name -> columnForFAStatus name =<< impFAStatus master

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
                          Just (_, True) -> ["text-muted"]
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

  columnToTitle <- getColumnToTitle param
        

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
    "salesAccount"           -> Just $ toHtml <$>  smfSalesAccount stock 
    "cogsAccount"            -> Just $ toHtml <$>  smfCogsAccount stock 
    "inventoryAccount"       -> Just $ toHtml <$>  smfInventoryAccount stock 
    "adjustmentAccount"      -> Just $ toHtml <$>  smfAdjustmentAccount stock 
    "assemblyAccount"        -> Just $ toHtml <$>  smfAssemblyAccount stock 
    "dimensionId"            -> Just $ toHtml . tshowM  <$> smfDimensionId stock 
    "dimension2Id"           -> Just $ toHtml . tshowM <$> smfDimension2Id stock 
    "actualCost"             -> Just $ toHtml <$> smfActualCost stock 
    "lastCost"               -> Just $ toHtml <$> smfLastCost stock 
    "materialCost"           -> Just $ toHtml <$> smfMaterialCost stock 
    "labourCost"             -> Just $ toHtml <$> smfLabourCost stock 
    "overheadCost"           -> Just $ toHtml <$> smfOverheadCost stock 
    "inactive"               -> Just $ toHtml <$> smfInactive stock 
    "noSale"                 -> Just $ toHtml <$> smfNoSale stock 
    "editable"               -> Just $ toHtml <$> smfEditable stock 
    _ -> Nothing

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
          FARunning ->  [shamlet|<span.label.label-success>Running|]
          FAAsleep ->  [shamlet|<span.label.label-warning>Asleep|]
          FADead ->  [shamlet|<span.label.label-danger>Dead|]
          FAGhost ->  [shamlet|<span.label.label-info>Ghost|]


-- * Rendering
renderIndex :: IndexParam -> Status -> Handler TypedContent
renderIndex param0 status = do
  (param, form, encType) <- getPostIndexParam param0
  ix <- itemsTable param
  purchAuth <- purchaseAuth
  if (ipMode param == ItemPurchaseView && not purchAuth)
    then permissionDenied "Contact your system administrator"
    else return ()
  let css = [cassius|
#items-index
  tr.unchecked
    opacity: 0.5
    font-weight: normal
  th
    writing-mode: sideways-lr
  .clickable
    cursor: crosshair
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
|]
  let navs = filter (\n -> n /= ItemAllView
                           && ( n /= ItemPurchaseView || purchAuth -- don't display tab if not authorized
                              )
                    ) [minBound..maxBound] :: [ItemViewMode]
      mode = ipMode param
      navClass nav = if mode == nav then "active" else "" :: Html
  let widget = [whamlet|
<div #items-index>
  <form #items-form role=form method=post action=@{ItemsR (ItemsIndexR (Just mode))} enctype=#{encType}>
    <div.well>
      ^{form}
      <button type="submit" name="button" value="search" class="btn btn-default">Search
    <ul.nav.nav-tabs>
      $forall nav <- navs
        <li class="#{navClass nav}">
          <a.view-mode href="#" data-url="@{ItemsR (ItemsIndexR (Just nav))}">#{drop 4 $ tshow nav}
      <div#items-table>
        ^{ix}
    <div.well>
      $if (ipShowInactive param)
        <button.btn.btn-danger type="submit" name="button" value="create">Create Missings
      $else
        <a href="#" data-toggle="tooltip" title="Please show unactive before creating missing items.">
          <div.btn.btn-danger.disabled type="submit" name="button" value="create">
            Create Missings
|]
      fay = $(fayFile "ItemsIndex")
  selectRep $ do
    provideRep  $ do
      html <- sendResponseStatus status =<< defaultLayout (widget >> fay >> toWidget css)
      return (html :: Html)
    provideRep $ do -- Ajax. return table
      table <- widgetToPageContent ix
      html <- withUrlRenderer (pageBody table)
      returnJson (renderHtml html)
      
  
-- ** css classes
  

getColumnToTitle :: IndexParam -> Handler (IndexColumn -> (Html, [Text]))
getColumnToTitle param = do
  runDB $ do
    -- TODO only loads map if needed
    priceListNames <- do
        entities <- selectList [] [] -- [Entity SalesType]
        return $ mapFromList [(k, salesTypeSalesType t) | (Entity (SalesTypeKey k) t) <- entities]
    supplierNames <- do
        entities <- selectList [] []
        return $ mapFromList [(k, supplierSuppName s) | (Entity (SupplierKey k) s) <- entities]
    let toh (Left r) = r
        toh (Right s) = (toHtml s, [])
        go column = let
          title = case column of
            CheckColumn -> Left ("", ["checkall"])
            RadioColumn -> Right ""
            StockIdColumn -> Right "Stock Id"
            StatusColumn -> Right "Status"
            GLColumn gl -> Right gl
            PriceColumn i -> Right $ findWithDefault "" i (priceListNames :: IntMap Text)
            PurchaseColumn i -> Right $ findWithDefault "" i (supplierNames :: IntMap Text)
            FAStatusColumn t -> Right t
          in toh title
    return go

columnClass :: IndexColumn -> Text
columnClass col = filter (/= ' ') (tshow col)

-- * Actions
-- ** Missings
createMissing :: IndexParam -> Handler ()
createMissing params = do
  -- load inactive as well to avoid trying to create missing product
  -- If on the GL tab, create prices as well as items
  -- Otherwise, only create the data corresponding to the current tab (eg. sales/purchase prices)
  let newMode = case ipMode params of
                  ItemGLView -> ItemAllView
                  mode -> mode
  traceShowM ("CREATE", newMode)
  itemGroups <- loadVariations (params {ipShowInactive = True, ipMode = newMode})
  let toKeep sku = case ipChecked params of
        [] -> True
        cs -> let set = setFromList cs :: Set Text
              in  traceShow ("lookup", sku, set) $ sku `member` set


      missings = [ (sku, iiInfo info)
                 | (_, vars) <- itemGroups
                 , (status, info) <- vars
                 , let sku = styleVarToSku (iiStyle info) (iiVariation info)
                 , traceShow (status, sku) $ status == VarMissing
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
         -- traceShowM ("prices", mprices)
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
         -- traceShowM ("purch", mprices)
         priceF <- IntMap.elems mprices
         let (t,price) = aPurchDataFToPurchData priceF
             _types = t :: [Text]
         -- keep only new prices
         guard (status == VarMissing || "text-warning"  `elem` t)
         [price]

      -- new items also need an items codes
      itemCodes = map stockMasterToItemCode stockMasters
        
  -- traceShowM ("tocreate ", (stockMasters, prices, purchData))
  runDB $ do
    insertEntityMany stockMasters
    insertMany_ itemCodes
    insertMany_ prices
    insertMany_ purchData

  setSuccess (toHtml $ tshow (maximumEx [length stockMasters, length prices, length purchData]) <> " items succesfully created.")
  return ()
