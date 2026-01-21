{-# LANGUAGE OverloadedStrings, ImplicitParams #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Items.Index
( getItemsIndexR
, postItemsIndexR
, loadVariations
, fillIndexCache
, IndexParam(..), indexParam
, ShowInactive(..)
)
where

import Import hiding(replace, product)
import Handler.Table
import Yesod.Form.Bootstrap3
import FA
import Items
import Handler.Items.Common
import Handler.Items.Category.Cache
import qualified Data.Map as Map
import Data.Monoid(Endo(..), appEndo)
import Data.Text(toTitle, replace, strip, splitOn)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import Database.Persist.MySQL hiding(replace)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Util.Cache
import Control.Comonad
import qualified Generics.OneLiner as OL
  
-- * Types 
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data IndexParam = IndexParam
  { ipSKU :: Maybe FilterExpression
  , ipCategory :: Maybe Text
  , ipCategoryFilter :: Maybe FilterExpression
  , ipVariationsF :: Maybe FilterExpression
  , ipVariationGroup :: Maybe Text -- ^ Alternative to variations
  , ipShowInactive :: ShowInactive
  , ipShowExtra :: Bool
  , ipBases :: Map Text Text -- ^ style -> selected base
  , ipChecked :: [Text] -- ^ styles to act upon
  , ipColumns :: [Text] -- ^ columns to act upon
  , ipMode :: ItemViewMode
  , ipClearCache :: Bool -- ^ Clear cache
  , ipGLStatusFilter :: Maybe [GLStatus]
  , ipSalesPriceStatusFilter :: Maybe [PriceStatus]
  , ipPurchasePriceStatusFilter :: Maybe [PriceStatus]
  , ipFAStatusFilter :: Maybe [FARunningStatus]
  , ipCategories :: Maybe [Text]
  , ipBaseVariation:: Maybe Text -- ^ to keep when filtering element, so that missing have a base
  } deriving (Eq, Show)


indexParam :: IndexParam
indexParam = IndexParam{..} where
  ipSKU = Nothing
  ipCategory = Nothing
  ipCategoryFilter = Nothing
  ipVariationsF = Nothing
  ipVariationGroup = Nothing
  ipShowInactive = ShowAll
  ipShowExtra = False
  ipBases = mempty
  ipChecked = []
  ipColumns = []
  ipMode = ItemFAStatusView
  ipClearCache = False
  ipGLStatusFilter = Nothing
  ipSalesPriceStatusFilter = Nothing
  ipPurchasePriceStatusFilter = Nothing
  ipFAStatusFilter = Nothing
  ipCategories = Nothing
  ipBaseVariation= Nothing

data IndexColumn = GLColumn Text
            | PriceColumn Int
            | PurchaseColumn Int
            | CheckColumn
            | RadioColumn
            | StockIdColumn
            | StatusColumn
            | GLStatusColumn
            | SalesPriceStatusColumn
            | PurchasePriceStatusColumn
            | FAStatusColumn FAStatusColumn
            | CategoryColumn Text -- (Text -> Maybe Text)
            deriving Show


data FAStatusColumn
  = QuantityOnHand AllOrAvailable
  | OnDemand AllOrAvailable
  | OnOrder
  | RunningStatus
  deriving (Show, Generic)

-- | In FA : some location are "disabled" location
-- meaning they should be ignored (ie lOST for lost item)
-- or CANCELLED for cancelled order
-- this type describe if we are interested in relevant quantities
-- of everything
data AllOrAvailable = FAAvailable | FAAll deriving (Show, Enum, Bounded, Eq, Ord, Generic)

-- | Preloaded data to be used by the handler
data IndexCache = IndexCache
  { icPriceLists :: [Entity SalesType]
  , icPriceListNames :: IntMap Text
  , icSupplierNames :: IntMap Text
  , icCategoryFinder :: Text -> FA.StockMasterId -> Maybe Text
  , icCategories :: [Text]
  }

data Button = CreateMissingBtn
            | ActivateBtn
            | DeactivateBtn
            | DeleteBtn
            deriving (Read, Show)
data ButtonStatus = BtnActive | BtnInactive Text | BtnHidden deriving Show
-- * Handlers 
-- | Override parameters from Url
-- Allows, link from categories to call this page
overrideParamFromUrl :: IndexParam -> Handler IndexParam
overrideParamFromUrl param@IndexParam{..} = do
  -- could be rewritten using normal Yesod form
  categories <- batchCategoriesH
  categoryIndex <- lookupGetParam "category"
  categoryFilter <- readFilterExpression <$$> lookupGetParam "category-filter"
  sku <- readFilterExpression <$$> lookupGetParam "sku"
  let category = case categoryIndex >>= readMay of
        Nothing -> categoryIndex -- category name
        Just index_ -> listToMaybe $ drop index_ categories
  return $ param { ipSKU = ipSKU <|> sku
                 , ipCategory = ipCategory <|> category
                 , ipCategoryFilter = ipCategoryFilter <|> categoryFilter
                 }

  
  
  
{-# NOINLINE getItemsIndexR #-}
getItemsIndexR :: Maybe ItemViewMode -> Handler TypedContent
getItemsIndexR mode = do
  skuToStyleVar <- skuToStyleVarH
  let ?skuToStyleVar = skuToStyleVar
  param <- overrideParamFromUrl (paramDef mode)
  renderIndex param ok200

{-# NOINLINE postItemsIndexR #-}
postItemsIndexR :: Maybe ItemViewMode -> Handler TypedContent
postItemsIndexR mode = do
  action <- lookupPostParam "button"
  skuToStyleVar <- skuToStyleVarH
  let ?skuToStyleVar = skuToStyleVar
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
  role <- currentRole
  return $ null (filterPermissions ReadRequest (setFromList ["purchase/prices"]) role)
    
-- * Utils 
-- ** Constants 
-- | Default cached delay. 5 minutes. Can be refreshed using the refresh cache button if needed.
-- At the moment there is a bug in the Cache which prevent
-- cache items to be purged before the delay expires.
-- This means that a 1 Day item will stay in memory for one day even if we refresh the cache,
-- so it is better at the moment to have of short life cache.
cacheDelay :: CacheDelay
cacheDelay = cacheMinute 15
-- ** Params and Forms 
paramDef :: Maybe ItemViewMode -> IndexParam
paramDef mode = IndexParam Nothing Nothing Nothing -- SKU category
                           Nothing Nothing -- variation
                           ShowActive True
                           mempty empty empty
                           (fromMaybe ItemGLView mode)
                           False --  ^ clear c
                           Nothing Nothing Nothing Nothing Nothing -- status filter
                           Nothing -- _^ base variation

-- g :: Applicative f => (f (Double -> Bool -> ABC)) -> _ -> f ABC
-- g  = a <*> undefined
-- indexForm :: (MonadHandler m,
--               RenderMessage (HandlerSite m) FormMessage)
--           => [Text]
--           -> IndexParam
--           -> Markup
--           -> MForm m (FormResult IndexParam, WidgetT (HandlerSite m) IO ())
indexForm :: [Text]
          -> _
          -> IndexParam
          -> Html
          -> MForm Handler (FormResult IndexParam, Widget)
indexForm categories groups param extra = do
    (f1, w1 ) <- renderBootstrap3 BootstrapBasicForm form1 extra
    (f2, w2 ) <- renderBootstrap3 BootstrapBasicForm (form2 $ pure (\a b f -> f a b )) ""
    (f2', w2' ) <- renderBootstrap3 BootstrapBasicForm (form2' $ pure (\a b f -> f a b)) ""
    (f3, _unused_w3 ) <- renderBootstrap3 BootstrapBasicForm (form3 $ pure (\a b c d e f -> f a b c d e)) ""
    (f4, w4 ) <- renderBootstrap3 BootstrapBasicForm (form4 $ pure (\a b c d e f -> f a b c d e)) ""
    (f5, w5 ) <- renderBootstrap3 BootstrapBasicForm (form5 $ pure (\a f -> f a)) ""
    let f = f1 <**> f2 <**> f2' <**> f3 <**> f4 <**> f5
        ws = map renderInline [w1 , w2, w2', w4 , w5]
    return (f, mconcat ws )
  where form1 = IndexParam
          <$> (aopt filterEField (bfs' "Sku") (Just $ ipSKU param))
          <*> (aopt (selectFieldList categoryOptions) (bfs' "Category") (Just $ ipCategory param))
          <*> (aopt filterEField (bfs' "Category Filter") (Just $ ipCategoryFilter param))
        form2 f = f
          <*> (aopt filterEField (bfs' "variations") (Just $ ipVariationsF param))
          <*> (aopt (selectFieldList groups') (bfs' "variation group_") (Just $ ipVariationGroup param))
        form2' f = f
          <*> (areq (selectField optionsEnum) (bfs' "Show Inactive") (Just $ ipShowInactive param))
          <*> (areq boolField (bfs' "Show Extra") (Just $ ipShowExtra param))
        form3 f = f
          <*> pure (ipBases param)
          <*> pure (ipChecked param)
          <*> pure (ipColumns param)
          <*> pure (ipMode param)
          <*> pure False
        form4 f = f
          <*> (aopt (mkStatusOptions 2) (bfs' "GL status") (Just $ ipGLStatusFilter param)) 
          <*> (aopt (mkStatusOptions 5) (bfs' "Sales Price status") (Just $ ipSalesPriceStatusFilter param)) 
          <*> (aopt (mkStatusOptions 5) (bfs' "Purchase Price status") (Just $ ipPurchasePriceStatusFilter param)) 
          <*> (aopt (mkStatusOptions 2) (bfs' "Running status") (Just $ ipFAStatusFilter param)) 
          <*> (aopt multiCategories (bfs' "Categories") (Just $ ipCategories param)) 
        form5 f = f
          <*> (aopt textField (bfs' "base candidates") (Just $ ipBaseVariation param))
        -- form = form2 form1
        groups' =  map (\g -> (g,g)) groups
        mkStatusOptions n = multiSelectField $ optionsPairs $ map (fanl (drop n . tshow)) [minBound..maxBound]
        -- rstatus = optionsPairs $ map (fanl (drop 2 . tshow)) [minBound..maxBound]
        -- wstatus = optionsPairs $ map (fanl (drop 3 . tshow)) [minBound..maxBound]
        categoryOptions = [(cat, cat) | cat <-categories ]
        multiCategories = multiSelectField $ optionsPairs categoryOptions
        bfs' t = t --  bfs (t :: Text)
        renderInline w = [whamlet|<div.well>
                                     <div.form-inline>^{w}
                                 |]

-- | Fill the parameters which are not in the form but have to be extracted
-- from the request parameters
fillTableParams :: IndexParam -> Handler IndexParam
fillTableParams params0 = do
  (params,_) <- runRequestBody
  let checked False  = mapMaybe (stripPrefix "check-" . fst)  params
      checked True =[]
      bases False = Map.fromList $ mapMaybe (\(k,v) -> stripPrefix "base-" k <&> (\b -> (b, v))
                                     ) params
      bases True = mempty
      columns = mapMaybe (stripPrefix "col-check-" . fst) params
      clearCache = lookup "button" params == Just "refresh"
      resetVariations = lookup "button" params == Just "clear-variations"
      
  return $ params0 {ipChecked = checked resetVariations, ipBases=bases resetVariations, ipColumns=columns, ipClearCache = clearCache}
   

-- getPostIndexParam :: IndexParam -> Handler (IndexParam, _
getPostIndexParam :: IndexParam -> Handler (IndexParam, Widget, Enctype)
getPostIndexParam param0 = do
  varGroup <- appVariationGroups <$> getsYesod appSettings
  categories <- categoriesH
  ((resp, form), encType) <- runFormPost (indexForm categories (Map.keys varGroup) param0)
  let param1 = case resp of
        FormMissing -> param0
        FormSuccess par -> par
        FormFailure _ ->  param0
  param <- fillTableParams param1
  return (param, form, encType)

-- | Generates the filter which loads the variations corresponding to the given
-- parameters.
-- styleQuery.
styleQuery :: IsString a => IndexParam -> Either a (Text, [PersistValue])
styleQuery IndexParam{..} = 
  let selectClause = " SELECT ?? FROM 0_stock_master "
      (where0, p0) = case ipSKU of
          Nothing -> ("", [])
          Just styleE ->  let (keyw, v) = filterEKeyword styleE
                              where_ = " stock_id " <> keyw
                          in (where_, v)
      (joinClause, where1 , p1) = case (ipCategory, ipCategoryFilter) of
                       (Just category, Just catFilter) -> let joinClause0 = " JOIN fames_item_category_cache AS category USING (stock_id) "
                                                              (catw, catv) = filterEKeyword catFilter
                                                              whereCat = [ "category.category = ? " , " category.value " <> catw ]
                                                          in (joinClause0, whereCat, (toPersistValue category : catv))
                       _ -> ("", [], [])
      makeWhere [] = ""
      makeWhere ws = " WHERE " <> intercalate " AND " ws
      activeWhere = case ipShowInactive of
                      ShowActive -> [" 0_stock_master.inactive = 0 "]
                      ShowInactive -> [" 0_stock_master.inactive = 1 "]
                      ShowAll -> []

  in case (filter (not . null) $ (where0 : where1)) of
    [] -> Left "Please enter a styles or category filter expression (SQL like expression or regexp starting with '/'. You can also enter a text cart or list starting with a delimeter , or |')"
    wheres -> Right  $ ( selectClause ++ joinClause ++ makeWhere (wheres ++ activeWhere)
                      , p0 <> p1 )
-- | Return Persistent filter corresponding to selected checkbox
selectedItemsFilter :: IsString a => IndexParam -> Either a  [ Filter FA.StockMaster ]
selectedItemsFilter param = 
  case ipChecked param of
    [] -> Left "No items selected"
    checkeds -> Right [ FA.StockMasterId <-. map FA.StockMasterKey checkeds ]

-- | Generates function to filter sku based on items which have been checked (checkbox)
checkFilter :: IndexParam -> Text -> Bool
checkFilter param sku0 = 
  let set = setFromList (ipChecked param) :: Set Text
      toKeep sku = case ipChecked param of
        [] -> True
        _non_empty -> sku `member` set
  in toKeep sku0

-- ** Preloaded Cache 
fillIndexCache :: Handler IndexCache
fillIndexCache = fillIndexCache' (Just [])
fillIndexCache' :: Maybe [Text] -> Handler IndexCache
fillIndexCache' categoriesm = do
  categories <- case categoriesm of
                   Nothing -> categoriesH
                   Just cats -> return cats
  catFinder <- categoryFinderCachedFor categories
  mkCache <- cache0 False (cacheDay 1) ("index_/static")  $ do
      salesTypes <- runDB $ selectList [] [] -- [Entity SalesType]
      let priceListNames = mapFromList [ (k, salesTypeSalesType t)
                                      | (Entity (SalesTypeKey k) t) <- salesTypes
                                      ]
      supplierNames <- runDB $ do
            entities <- selectList [] []
            return $ mapFromList [(k, supplierSuppName s) | (Entity (SupplierKey k) s) <- entities]

      -- don't load webprices if the DC database is not configured

      return $ IndexCache salesTypes priceListNames supplierNames
  return $ mkCache catFinder categories
  
  
-- ** StyleAdjustment 
getAdjustBase :: Handler (IndexCache -> ItemInfo (ItemMasterAndPrices Identity) -> Var -> ItemInfo (ItemMasterAndPrices Identity))
getAdjustBase = do
  settings <- appSettings <$> getYesod
  let varMap = appVariations settings
      go _cache item0@(ItemInfo style _ master ) var = let
        stock = impMaster master
        salesPrices = impSalesPrices master
        purchasePrices = impPurchasePrices master
        adj = adjustDescription varMap (iiVariation item0) var
        Sku sku = styleVarToSku style var
        in item0  { iiInfo = master
                    { impMaster = (\s -> s {smfDescription = smfDescription s <&> adj
                                           ,smfLongDescription = smfLongDescription s <&> adj}
                                  ) <$> stock
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
adjustDescription :: Map Var Text -> Var -> Var -> Text -> Text
adjustDescription varMap var0 var desc =
  case (lookupVars varMap var0, lookupVars varMap var) of
    ([],_) -> desc
    (_, []) -> desc
    (vnames0, vnames) -> let
      endos = [ (Endo $ replace (f vnames0) (f vnames))
              | f0 <- [toTitle, toUpper, toLower ]
              , let f vs = unVar $ varsToVariation (map (Var . f0) vs)
              ]
      in appEndo (mconcat endos) desc
-- * Load DB  
-- ** StockMaster info 
-- |  Load all variations matching the criteria
-- regardless or whether they've been checked or not
loadVariations :: (?skuToStyleVar :: Sku -> (Style, Var))
                => IndexCache -> IndexParam
               -> Handler [ (ItemInfo (ItemMasterAndPrices Identity) -- base
                            , [ ( VariationStatus
                                , ItemInfo (ItemMasterAndPrices ((,) [Text]))
                                )
                              ] -- all variations, including base
                            )
                          ]
loadVariations cache param = do
  let forceCache = ipClearCache param
  -- pre cache variations parts
  -- parts of ItemMasterAndPrices Are loaded and cached asynchronouly.
  -- This allow to preload and cache everything, ready to be seen by the user
  -- when activated the corresponding time and at the same time
  -- make data available if needed.
  -- The cache include the paramter in the key, however
  -- some parameter like which items are check on the mode are irrelevant
  -- and therefore overriden.
  -- Base it's also irrelevant even thought it has an impact
  -- on the calculation of the status, it doesn't have
  -- an impact on the loaded data
  delayeds <- mapM (\(p,a) -> (p,) <$> preCache0 forceCache
                                        (cacheDelay) (p
                                           , param { ipMode = ItemPriceView
                                                  , ipChecked = []
                                                  , ipBases = mempty
                                                  }
                                           ) (runDB $ a param))
           [ ("prices" :: Text, loadSalesPrices)
           , ("purchase", loadPurchasePrices)
           , ("fa status", loadStatus)
           ]
  -- mapM_ startDelayed delayeds
  let [ delayedSalesPrices , delayedPurchasePrices
        , delayedStatus
        ] = delayeds
  let -- adjustSources ::
    -- Add sources needed by filter and remove duplicate
      adjustSources :: [(Text, Delayed Handler [ItemInfo (ItemMasterAndPrices Identity)])]
                    ->  [ Delayed Handler [ItemInfo (ItemMasterAndPrices Identity)]]
      adjustSources sources = toList $ Map.fromList
        (
          (if null (ipSalesPriceStatusFilter param) then Nothing else Just delayedSalesPrices) ?:
          (if null (ipPurchasePriceStatusFilter param) then Nothing else Just delayedPurchasePrices) ?:
          (if null (ipFAStatusFilter param) then Nothing else Just delayedStatus) ?:
          sources
        )

  let bases =  Map.mapKeys Style . fmap Sku $ ipBases param
  adjustBase <- getAdjustBase
  
  styles <- case styleQuery param of
    Left err -> do
              Import.setInfo err
              return []
    Right (sql, sqlParams) -> let select = rawSql (sql <> " ORDER BY 0_stock_master.stock_id") sqlParams
                    in cache0 forceCache (cacheDelay) ("load styles", (ipSKU param, ipCategory param, ipCategoryFilter param, ipShowInactive param)) (runDB select)
  infoSources <- mapM getDelayed $ adjustSources (case ipMode param of
    ItemGLView -> []
    ItemPriceView -> [delayedSalesPrices]
    ItemPurchaseView -> [delayedPurchasePrices]
    ItemAllStatusView -> [delayedSalesPrices, delayedPurchasePrices, delayedStatus]
    ItemAllView -> [delayedSalesPrices, delayedPurchasePrices]
    ItemFAStatusView -> [delayedStatus]
    ItemCategoryView -> []
    )

  itemVars <- getVarsFor forceCache param
  let itemStyles = filterActive $ mergeInfoSources ( map stockItemMasterToItem styles
                                                   : infoSources
                                                   ) 
      filterActive = case ipShowInactive param of
                     ShowActive ->   (List.filter (maybe False (not . runIdentity . smfInactive) . (impMaster . iiInfo)))
                     _ -> id
                     -- \^ only keep active variations
                     -- impMaster not present, means the variations hasn't been loaded (ie filtered)
                     -- so we are not showing it
      baseCandidates = map Var $ maybe [] (splitOn "|") (ipBaseVariation param)
      itemGroups = joinStyleVariations (?skuToStyleVar <$> bases)
                                        baseCandidates
                                        (adjustBase cache) computeDiff
                                        itemStyles itemVars
      filterExtra = if ipShowExtra param
                    then id
                    else (List.filter ((/= VarExtra) . fst))

  let result =  mapMaybe (filterFromParam param cache) $ map (\(base, vars)
                  -> (base, filterExtra vars)
                ) itemGroups
  mapM_ (startDelayed . snd) delayeds
  return result
  
-- | Create a function which given a style returns
-- a list of variants. The normal case is cross product
-- with the variations (select * for variationGroup) but is tweaked
-- to add explicitely filtered sku
getVarsFor :: (?skuToStyleVar :: Sku -> (Style, Var)) => Bool -> IndexParam -> Handler (Style -> [Var])
getVarsFor forceCache param =
  case (ipVariationsF param, ipVariationGroup param)  of
       (Just filter_, Nothing)  -> do
                       let select = runDB (selectKeysList
                                                      (filterE StockMasterKey FA.StockMasterId (Just filter_)
                                                       <> [FA.StockMasterInactive ==. False ]
                                                      )
                                             [Asc FA.StockMasterId])
                       keys' <-   cache0 forceCache (cacheDelay) (filter_, "load variations") select
                       return $ const $ map (snd . ?skuToStyleVar . Sku . unStockMasterKey) keys'
       (Nothing, Nothing) -> 
          case ipSKU param of
            Just (InFilter _ skus) ->  let  styleToVars = groupAsMap fst (return . snd) $ map (?skuToStyleVar . Sku) skus
                               in return $ \st -> findWithDefault [] st styleToVars

            _ -> return . const $ [] -- (Left $ map  entityKey styles)
       (_ ,Just group_) -> do
         varGroupMap <- appVariationGroups <$> appSettings <$> getYesod
         return . const $ (Map.findWithDefault [] group_ varGroupMap)

-- | Filters according to FA and DC status but KEEP base variation if needed.
-- This is needed To be able to copy price or other information when creating missing
-- product. 
-- However, we only keep the base variation if there is any valid variations
filterFromParam :: IndexParam -> IndexCache
  ->  (ItemInfo ( ItemMasterAndPrices Identity)
      , [ ( VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text]) )) ]
      )
  -> Maybe (ItemInfo ( ItemMasterAndPrices Identity)
           , [ ( VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text]) )) ]
           )
filterFromParam param@IndexParam{..} cache (base, vars0) = let
  vars = filter (keepVar . snd) vars0
  keepVar i = isBase i || statusOk i
  -- statusOk :: (Applicative f, Comonad f, Eq (StockMasterF f), Eq (PriceF f), Eq (PurchDataF f))
  --   => ItemMasterAndPrices f  -> ItemInfo (ItemMasterAndPrices f) -> Bool
  statusOk i = all (\fn -> fn param i) [ glStatusOk
                                     , salesPriceStatusOk (priceListsToKeep cache param)
                                     , purchasePriceStatusOk (suppliersToKeep cache param)
                                     , faStatusOk
                                     ]
  -- -| variation given as parameter or base one
  isBase info = iiVariation base == iiVariation info
  in case vars of
       []                       -> Nothing
       [b] | not (statusOk (snd b)) -> Nothing -- base incorrect, don't display
       _                        -> Just (base, vars)

faStatusOk :: (Applicative f, Comonad f) => IndexParam -> ItemInfo (ItemMasterAndPrices f) -> Bool
faStatusOk = checkStatuses ipFAStatusFilter (fmap faRunningStatus . impFAStatus . iiInfo)

-- | Check if statuses are given that 
checkStatuses :: (Eq status, Comonad w)
  => (param -> Maybe [status]) -> (item -> Maybe (w status)) -> param -> item -> Bool
checkStatuses paramStatuses itemStatus param item =
  case (paramStatuses param, itemStatus item) of
    (Nothing, _) -> True -- no statuses to check
    (Just [], _) -> True -- no statuses to check
    (Just nonnull, Just statusM)  -> extract statusM `elem` nonnull
    (_, Nothing) -> False -- not item status to check

glStatusOk = checkStatuses ipGLStatusFilter (Just . ([],) .  glStatus . iiInfo)
glStatus :: ItemMasterAndPrices ((,) [Text]) -> GLStatus
glStatus item = case impMaster item of
  Nothing -> GLDiffers
  Just master' -> let
    master = master' { smfActualCost  = ([], 0)
                     , smfLastCost  = ([], 0)
                     , smfMaterialCost  = ([], 0)
                     , smfLabourCost  = ([], 0)
                     , smfOverheadCost  = ([], 0)
                     } -- costs can be different, so we don't check them
    (classes, _) = aStockMasterFToStockMaster master :: ([Text], StockMaster)
    in case (filter (not . null) classes) of
              [] -> GLOk
              ["text-danger"] | fst (smfDescription master) == ["text-danger"] -> GLDescriptionDiffers
              _ -> GLDiffers
    
    
-- salesPriceStatusOk ::
--   [Key SalesType]
--   -> ItemMasterAndPrices ((,) Text)
--   -> IndexParam
--   -> Bool
salesPriceStatusOk priceListIds = checkStatuses ipSalesPriceStatusFilter (Just . ([],) . (salesPricesStatus priceListIds) . iiInfo)
salesPricesStatus :: [Key SalesType] ->  ItemMasterAndPrices ((,) [Text]) -> PriceStatus
salesPricesStatus priceListIds item = maybe PriceMissing (pricesStatus pfPrice unSalesTypeKey priceListIds) (impSalesPrices item)

-- pricesStatus :: [Key SalesType] -> IntMap (PriceF ((,) [Text])) -> PriceStatus
pricesStatus getPrice unkey priceListIds prices0 =  let
    prices = case priceListIds of
      [] -> prices0
      _ -> IntMap.filterWithKey (\k _ -> k `IntSet.member` validKeys) prices0
    validKeys = (IntSet.fromList $ map unkey priceListIds)
    classes = filter (not . null) $ concatMap (fst . getPrice) (toList prices) :: [Text]
    in case ("text-danger" `elem` classes, classes) of
      (True, _ ) -> PriceDiffers
      (_, (_:_)) -> PriceMissing -- or extra which means the base is missing
      _ -> PriceOk

    
purchasePriceStatusOk priceListIds = checkStatuses ipPurchasePriceStatusFilter (Just . ([],) . purchasePricesStatus priceListIds . iiInfo)
purchasePricesStatus :: [Key Supplier] ->  ItemMasterAndPrices ((,) [Text]) -> PriceStatus
purchasePricesStatus supplierIds item = maybe PriceMissing (pricesStatus pdfPrice unSupplierKey supplierIds) (impPurchasePrices item)

-- ** Sales prices 
-- | Load sales prices 
    -- loadSalesPrices :: IndexParam -> Handler [ItemInfo (ItemMasterAndPrices Identity)]
loadSalesPrices :: (?skuToStyleVar :: Sku -> (Style, Var))
                => IndexParam -> SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadSalesPrices param = do
  case (ipSKU param) of
     Just styleF -> do
       let sql = "SELECT ?? FROM 0_prices JOIN 0_stock_master USING(stock_id)"
            <> " WHERE curr_abrev = 'GBP' AND " <> stockF <> inactive
            <> " ORDER BY stock_id"
           (fKeyword, p) = filterEKeyword styleF
           stockF = "stock_id " <> fKeyword
           inactive = case ipShowInactive param of
                        ShowActive -> " AND inactive = 0"
                        ShowInactive -> " AND inactive = 1"
                        ShowAll -> ""
       do
           prices <- rawSql sql p

           let group_ = groupBy ((==) `on `priceStockId) (map entityVal prices)
               maps = map (\priceGroup@(one:_) -> let
                              pricesF = mapFromList [ ( priceSalesTypeId p'
                                                      , runIdentity $ aPriceToPriceF p'
                                                      )
                                                    | p' <- priceGroup
                                                    ]
                              (style, var) = ?skuToStyleVar (Sku $ priceStockId one)
                              master = mempty { impSalesPrices = Just pricesF }
                              in ItemInfo style var master
                          ) group_

           return maps
          

            
     _ -> return []

-- | get selected price list ids
priceListsToKeep :: -- (?skuToStyleVar :: Sku -> (Style, Var))
              IndexCache -> IndexParam -> [Key SalesType]
priceListsToKeep cache params =
  let plIds =  map (entityKey) (icPriceLists cache)
  in filter (\(SalesTypeKey i) -> (priceColumnCheckId i) `elem` ipColumns params) plIds
  
-- ** Purchase prices 
-- | Load purchase prices
-- loadPurchasePrices :: IndexParam -> Handler [ ItemInfo (Map Text Double) ]
loadPurchasePrices :: (?skuToStyleVar :: Sku -> (Style, Var))
              => IndexParam -> SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadPurchasePrices param = do
  case (ipSKU param) of
     Just styleF -> do
       let sql = "SELECT ?? FROM 0_purch_data JOIN 0_stock_master USING(stock_id)"
            <> " WHERE " <> stockF <> inactive
            <> " ORDER BY stock_id"
           (fKeyword, p) = filterEKeyword styleF
           stockF = "stock_id " <> fKeyword
           inactive = case ipShowInactive param of
                        ShowActive -> " AND inactive = 0"
                        ShowInactive -> " AND inactive = 1"
                        ShowAll -> ""
       do
           prices <- rawSql sql p

           let group_ = groupBy ((==) `on `purchDataStockId) (map entityVal prices)
               maps = map (\priceGroup@(one:_) -> let
                              pricesF = mapFromList [ ( purchDataSupplierId p'
                                                      , runIdentity $ aPurchDataToPurchDataF p'
                                                      )
                                                    | p' <- priceGroup
                                                    ]
                              (style, var) = ?skuToStyleVar (Sku $ purchDataStockId one)
                              master = mempty { impPurchasePrices = Just pricesF }
                              in ItemInfo style var master
                          ) group_

           return maps
          

            
     _ -> return []
  
suppliersToKeep :: IndexCache -> IndexParam -> [Key Supplier]
suppliersToKeep cache params = let
  suppIds = keys (icSupplierNames cache)
  in (map SupplierKey) (filter (\i -> (supplierColumnCheckId i) `elem` ipColumns params) (suppIds))

-- ** FA Status 
-- | Load item status, needed to know if an item can be deactivated or deleted safely
-- This includes if items have been even ran, still in stock, on demand (sales) or on order (purchase)
loadStatus :: (?skuToStyleVar :: Sku -> (Style, Var))
              => IndexParam ->  SqlHandler [ItemInfo (ItemMasterAndPrices Identity)]
loadStatus param = do
  rows <- loadFAStatus (ipSKU param) (ipShowInactive param)
  return [ ItemInfo style var master
         | (sku, status) <- rows
         , let (style, var) = ?skuToStyleVar $ Sku sku
         , let master = mempty { impFAStatus = Just status}
         ]


loadVariationsToKeep :: (?skuToStyleVar :: Sku -> (Style, Var))
                => IndexCache -> IndexParam
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
      toKeep (_,info) = checkSku sku where Sku sku = styleVarToSku (iiStyle info) (iiVariation info)
  return . filter (not . null . snd  ) -- filter Group with no variations left
         $ (filter toKeep) <$$> itemGroups -- 

-- * Misc 
-- ** Type conversions 
stockItemMasterToItem :: (?skuToStyleVar :: Sku -> (Style, Var))
              => (Entity FA.StockMaster) -> ItemInfo (ItemMasterAndPrices Identity)
stockItemMasterToItem (Entity key val) = ItemInfo  style var master where
            sku = Sku $ unStockMasterKey key
            (style, var) = ?skuToStyleVar sku
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
lookupVars :: Map Var Text -> Var -> [Text]
lookupVars varMap = mapMaybe (flip Map.lookup varMap) . variationToVars


-- ** Table Infos 
-- | List or columns for a given mode
columnsFor :: IndexCache -> ItemViewMode -> [ItemInfo (ItemMasterAndPrices f)] -> [IndexColumn]
columnsFor _ ItemGLView _ = GLStatusColumn : map GLColumn cols where
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
columnsFor _ ItemPriceView infos = SalesPriceStatusColumn : map PriceColumn cols where
  cols = salesPricesColumns $ map  iiInfo  infos
columnsFor _ ItemPurchaseView infos = PurchasePriceStatusColumn :map PurchaseColumn cols where
  cols = purchasePricesColumns $ map  iiInfo  infos
columnsFor _ ItemFAStatusView _ = map FAStatusColumn fas where
  fas = OL.create @Bounded [minBound,maxBound] 
columnsFor _ ItemAllStatusView _ =  [StatusColumn, GLStatusColumn, SalesPriceStatusColumn, PurchasePriceStatusColumn, FAStatusColumn RunningStatus]
columnsFor _ ItemAllView _ = []
columnsFor cache ItemCategoryView _ = map CategoryColumn (icCategories cache)


itemsTable :: (?skuToStyleVar :: Sku -> (Style, Var))
                => IndexCache -> IndexParam ->  Handler Widget
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






  let columns = [CheckColumn, RadioColumn, StockIdColumn] ++ columnsFor cache (ipMode param) allItems

  -- Church encoding ?
  let itemToF :: ItemInfo (ItemMasterAndPrices Identity)
              -> (VariationStatus, ItemInfo (ItemMasterAndPrices ((,) [Text])))
              -> (IndexColumn -> Maybe (Html, [Text]) -- Html + classes per column
                , [Text]) -- classes for row

      itemToF item0 (status , ItemInfo style var master) =
        let Sku sku =  styleVarToSku style var
            checked = maybe True (sku `elem`) checkedItems
            missingLabel = case status of
                                  VarMissing -> [shamlet| <span.label.label-warning data-label=missing> Missing |]
                                  VarExtra -> [shamlet| <span.label.label-info data-label=extra> Extra |]
                                  VarOk -> [shamlet||]
            -- base = iiInfo $ computeDiff item0 item0
            val col = case col of
                        CheckColumn ->
                          Just ([], [shamlet|<input type=checkbox name="check-#{sku}" :checked:checked>|])
                        RadioColumn ->
                          let rchecked = var == iiVariation item0
                          in if status == VarMissing
                            then Just ([], missingLabel)
                            else Just ([], [shamlet|<input type=radio name="base-#{unStyle style}" value="#{sku}"
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
                        FAStatusColumn col' -> columnForFAStatus col' =<< impFAStatus master
                        CategoryColumn catName -> columnForCategory catName <$> icCategoryFinder cache catName (FA.StockMasterKey sku)
                        GLStatusColumn -> case glStatus master of
                          GLOk ->  Just ([], [shamlet|<span.label.label-success data-label=glok>Ok|])
                          GLDiffers ->  Just ([], [shamlet|<span.label.label-danger data-label=gldiffers>diff|])
                          GLDescriptionDiffers ->  Just ([], [shamlet|<span.label.label-warning data-label=gldescription>description|])
                        SalesPriceStatusColumn -> case salesPricesStatus (priceListsToKeep cache param ) master of
                          PriceOk ->  Just ([], [shamlet|<span.label.label-success data-label=sales-prices-ok>Ok|])
                          PriceMissing ->  Just ([], [shamlet|<span.label.label-warning data-label=sales-prices-missing>Missing|])
                          PriceDiffers ->  Just ([], [shamlet|<span.label.label-danger data-label=sales-prices-differ>Diff|])
                        PurchasePriceStatusColumn -> case purchasePricesStatus (suppliersToKeep cache param) master of
                          PriceOk ->  Just ([], [shamlet|<span.label.label-success data-label=purchase-prices-ok>Ok|])
                          PriceMissing ->  Just ([], [shamlet|<span.label.label-warning data-label=purchase-prices-missing>Missing|])
                          PriceDiffers ->  Just ([], [shamlet|<span.label.label-danger data-label=purchase-prices-differ>Diff|])

            differs = or diffs where
              diffs = [ "text-danger" `elem `kls
                      | col <- columns
                      , let kls = maybe [] fst (val col)
                      ]
            classes :: [Text]
            classes = ("style-" <> unStyle (iiStyle item0))
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


      -- We keep row grouped so we can change the style of the first one of every group_.
      rowGroup = map (\(base, vars) -> map (itemToF base) vars) itemGroups
      styleFirst ((fn, klasses):rs) = (fn, "style-start":klasses):rs
      styleFirst [] = error "Shouldn't happend"

      styleGroup klass rs = [(fn, klass:klasses) | (fn,klasses) <- rs]

  columnToTitle <- getColumnToTitle cache param
        

  return $ displayTable columns columnToTitle
                        (concat  (zipWith styleGroup (List.cycle ["group_-2","group_-3"])
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
toInactive :: Bool -> Text
toInactive False = "Active"
toInactive True = "Inactive"

badgify :: Text -> Text -> Html
badgify label str = [shamlet|<span data-label="#{strip label}-#{strip str}">#{str}|]

columnForPrices :: Int -> (IntMap (PriceF ((,) [Text]))) -> Maybe ([Text], Html)
columnForPrices colInt prices = do -- Maybe
  value <- IntMap.lookup colInt prices
  return $ toHtml . tshow <$> pfPrice value where

columnForPurchData :: Int -> (IntMap (PurchDataF ((,) [Text]))) -> Maybe ([Text], Html)
columnForPurchData colInt purchData = do -- Maybe
  value <- IntMap.lookup colInt purchData
  return $ toHtml . tshow <$> pdfPrice value where

columnForFAStatus :: FAStatusColumn -> (ItemStatusF ((,) [Text])) -> Maybe ([Text], Html)
columnForFAStatus col iStatus@ItemStatusF{..} =
  case col of
    QuantityOnHand FAAvailable -> Just (toHtml . tshow <$> isfQoh)
    QuantityOnHand FAAll-> Just (toHtml . tshow <$> isfAllQoh)
    OnDemand FAAvailable-> Just (toHtml . tshow <$> isfOnDemand)
    OnDemand FAAll -> Just (toHtml . tshow <$> isfAllOnDemand)
    OnOrder-> Just (toHtml . tshow <$> isfOnOrder)
    RunningStatus -> Just (statusBadge <$> faRunningStatus iStatus )
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


columnForCategory :: Text -> Text -> ([Text], Html)
columnForCategory catName category = ([], badgify catName category <> link ) where
  link = [shamlet|<a href="/items/index/Nothing?category=#{catName}&category-filter=#{category}">@|]
  --  hack because shamlet can't use a route, so we need to set the route manually instead
  --  of using @{..}
-- * Rendering 
renderButton :: IndexParam -> Text -> Button -> Html
renderButton param bclass button = case buttonStatus param button of
  BtnActive ->  [shamlet|
               <button.btn class="btn-#{bclass}" type="submit" name="button" value="#{tshow button}">#{buttonName (ipMode param) button}
               |]
  BtnInactive tooltip ->  [shamlet|
                 <a href="#" data-toggle="tooltip" title="#{tooltip}">
                    <div.btn.disabled class="btn-#{bclass} type="submit" name="button" value="#{tshow button}">#{buttonName (ipMode param) button}
               |]
  BtnHidden ->  [shamlet||]

-- | Check if the user param returns all variaton of a given style or filter
-- some of them. Creating missing items only works if we are sure that
-- we are displaying the item.
areVariationsComplete :: IndexParam -> Bool
areVariationsComplete IndexParam{..} = (ipShowInactive == ShowAll) && null ipFAStatusFilter && null ipCategoryFilter
buttonStatus :: IndexParam -> Button -> ButtonStatus
buttonStatus param CreateMissingBtn = case (ipMode param, areVariationsComplete param) of
  (ItemGLView, False) -> BtnInactive "Please show inactive item before creating items. This is to avoid trying to create disabled items."
  _ -> BtnActive
 
buttonStatus param ActivateBtn = case ipMode param of
  ItemPriceView -> BtnHidden
  ItemPurchaseView -> BtnHidden
  _ -> BtnActive
buttonStatus param DeactivateBtn = case ipMode param of
  ItemPriceView -> BtnHidden
  ItemPurchaseView -> BtnHidden
  ItemCategoryView -> BtnHidden
  _ -> BtnActive
  
buttonStatus param DeleteBtn = case ipMode param of
  ItemPriceView -> BtnActive
  ItemPurchaseView -> BtnActive
  ItemFAStatusView | ipFAStatusFilter param == Just [ FAGhost ] -> BtnActive
                   | otherwise -> BtnInactive "Please select ghosts"
  _ -> BtnHidden

buttonName :: ItemViewMode -> Button -> Text
buttonName ItemCategoryView CreateMissingBtn = "Refresh Categories"
buttonName _ CreateMissingBtn = "Create Missings"
buttonName _ ActivateBtn = "Activate"
buttonName _ DeactivateBtn = "Deactivate"
buttonName _ DeleteBtn = "Delete"

  


renderIndex :: (?skuToStyleVar :: Sku -> (Style, Var))
                => IndexParam -> Status -> Handler TypedContent
renderIndex param0 status = do
  (param, form, encType) <- getPostIndexParam param0
  let categoriesm = case ipMode param  of
                      ItemCategoryView-> ipCategories param
                      _ -> Just []
  cache <- fillIndexCache' categoriesm
  ix <- itemsTable cache param
  purchAuth <- purchaseAuth
  if (ipMode param == ItemPurchaseView && not purchAuth)
    then permissionDenied "Contact your system administrator"
    else return ()
  let css = [cassius|
#items-index_
  tr.unchecked
    font-weight: normal
  tr
    font-weight: bold
  th
    writing-mode: sideways-lr
  .clickable
    cursor: pointer
  tr.base.group_-1 
    background: #f2dede
  tr.base.group_-2 
    background: #dff0d8
  tr.base.group_-3 
    background: #d0edf7
  tr.base.group_-4 
    background: #fcf8e3
  .base
    font-weight: 500
  tr.style-start
    border-top: 3px solid black
  tr.group_-1
    border-left: solid #d0534f
  tr.group_-2
    border-left: solid #93c54b
  tr.group_-3
    border-left: solid #29abe0
  tr.group_-4
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
<div #items-index_>
  <form #items-form role=form method=post action=@{ItemsR (ItemsIndexR (Just mode))} enctype=#{encType}>
    <div.form>
      ^{form}
      <button type="submit" name="button" value="search" class="btn btn-default">Search
      <button type="submit" name="button" value="clear-variations" class="btn btn-primary">Clear/Set Variations
      <button type="submit" name="button" value="refresh" class="btn btn-info">Refresh Cache
    <ul.nav.nav-tabs>
      $forall nav <- navs
        <li class="#{navClass nav}">
          <a.view-mode href="#" data-url="@{ItemsR (ItemsIndexR (Just nav))}">#{wordize (Just "Item") (Just "View") $ tshow nav}
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
                  in Left ([shamlet|#{name}
                                   <input type="checkbox" name="col-check-#{priceColumnCheckId i}" :checked:checked>|]
                          , ["price"]
                          )
            PurchaseColumn i -> case lookup i supplierNames of
                Nothing -> Right ""
                Just name -> let
                  checked = supplierColumnCheckId i `elem` (ipColumns param)
                  in Left ([shamlet|#{name}
                                   <input type="checkbox" name="col-check-#{supplierColumnCheckId i}" :checked:checked>|]
                          , ["purch_price"]
                          )
            FAStatusColumn t -> Right $ case t of
                QuantityOnHand FAAvailable -> "Quantity On Hand (Avl)"
                QuantityOnHand FAAll -> "Quantity On Hand (All)"
                OnDemand FAAvailable -> "On Demand (Avl)"
                OnDemand FAAll -> "On Demand (All)"
                OnOrder -> "On Order"
                RunningStatus -> "Running Status"
              
            CategoryColumn t -> Right t
            GLStatusColumn -> Right "GL Status"
            SalesPriceStatusColumn ->  Right "Sales Price Status"
            PurchasePriceStatusColumn ->  Right "Purchase Price Status"
          in toh title
    return go

columnClass :: IndexColumn -> Text
columnClass col = filter (/= ' ') (tshow col)
  
-- * Actions 
-- *** Missings 
createMissing :: (?skuToStyleVar :: Sku -> (Style, Var))
                  => IndexParam -> Handler ()
createMissing params = do
   resp <- (case ipMode params of
              ItemCategoryView -> refreshCategories
              _ -> createGLMissings
           ) params

   clearAppCache
   return resp

deleteItems :: (?skuToStyleVar :: Sku -> (Style, Var))
                  => IndexParam -> Handler ()
deleteItems params = do
  resp <- runDB ( case ipMode params of
              ItemPriceView -> deleteSalesPrices params
              ItemPurchaseView -> deletePurchasePrices params
              ItemFAStatusView | ipFAStatusFilter params == Just [FAGhost]-> do
                   deleteSalesPrices params
                   deletePurchasePrices params
                   deleteStockAndItems params
              _ -> error "Only ghost can be deleted. Please set the running status filter to Ghost"
      )
  clearAppCache
  return resp


-- ***  Gl 
createGLMissings :: (?skuToStyleVar :: Sku -> (Style, Var))
                => IndexParam -> Handler ()
createGLMissings params = do
  -- load inactive as well to avoid trying to create missing product
  -- If on the GL tab, create prices as well as items
  -- Otherwise, only create the data corresponding to the current tab (eg. sales/purchase prices)
  let newMode = case ipMode params of
                  ItemGLView -> ItemAllView
                  mode -> mode
  cache <- fillIndexCache
  itemGroups <- loadVariations cache (params {ipShowInactive = ShowAll, ipMode = newMode})
  let toKeep = checkFilter params

      missings = [ (sku, iiInfo info)
                 | (_, vars) <- itemGroups
                 , (status, info) <- vars
                 , let Sku sku = styleVarToSku (iiStyle info) (iiVariation info)
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
         let Sku sku = styleVarToSku (iiStyle info) (iiVariation info)
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
         let Sku sku = styleVarToSku (iiStyle info) (iiVariation info)
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
        
  runDB $ do
    insertEntityMany stockMasters
    insertMany_ itemCodes
    insertMany_ prices
    insertMany_ purchData
  refreshCategoryFor Nothing (ipSKU params)
  clearAppCache

  setSuccess (toHtml $ tshow (maximumEx [length stockMasters, length prices, length purchData]) <> " items succesfully created.")
  return ()

deleteSalesPrices :: (?skuToStyleVar :: Sku -> (Style, Var))
                  => IndexParam -> SqlHandler ()
deleteSalesPrices params = do
  cache <- lift fillIndexCache
  -- timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- lift $ loadVariationsToKeep cache (params {ipBases = mempty}) 
  let plIds = priceListsToKeep cache params
  -- delete all prices selected by the user for all given variations
  let deletePairs =
        [ [FA.PriceStockId ==. unSku (iiSku info), PriceSalesTypeId ==. unSalesTypeKey plId]
        | plId <- plIds
        , (_, items) <- itemGroups
        , (_, info) <- items
        ]

  mapM_ deleteWhere deletePairs

deletePurchasePrices :: (?skuToStyleVar :: Sku -> (Style, Var))
                  => IndexParam -> SqlHandler ()
deletePurchasePrices  params = do
  cache <- lift fillIndexCache
  -- timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- lift $ loadVariationsToKeep cache (params {ipBases = mempty}) 
  let supplierIds = suppliersToKeep cache params
  -- delete all prices selected by the user for all given variations
  let deletePairs =
        [ [FA.PurchDataStockId ==. unSku (iiSku info), PurchDataSupplierId ==. supplierId]
        | (SupplierKey supplierId) <- supplierIds
        , (_, items) <- itemGroups
        , (_, info) <- items
        ]

  mapM_ deleteWhere deletePairs

deleteStockAndItems :: (?skuToStyleVar :: Sku -> (Style, Var)) => IndexParam -> SqlHandler ()
deleteStockAndItems params =  do
  cache <- lift fillIndexCache
  -- timestamp <- round <$> liftIO getPOSIXTime
  itemGroups <- lift $ loadVariationsToKeep cache (params {ipBases = mempty} ) 
  let skus = [ unSku (iiSku info)
             | (_, items) <- itemGroups
             , (_, info) <- items
             , fmap (snd . faRunningStatus)  (impFAStatus (iiInfo info)) == Just  FAGhost
             ]
  forM_ skus \sku -> do
        print "======================================"
        print sku
        -- delete bom
        deleteWhere [ FA.BomParent ==. sku ]
        deleteWhere [ FA.BomComponent ==. sku ]
        -- delete items_code
        deleteWhere [ FA.ItemCodeStockId ==. sku ]
        -- delete stock_master
        deleteWhere [ FA.StockMasterId ==. FA.StockMasterKey sku ]

-- *** Categories
refreshCategories :: (?skuToStyleVar :: Sku -> (Style, Var)) => IndexParam -> Handler ()
refreshCategories params = do
  cache <- fillIndexCache
  itemGroup <- loadVariationsToKeep cache params

  forM_ itemGroup $ \(_, infos) -> do
       refreshCategoryFor Nothing (Just $ InFilter ',' $ map (unSku . iiSku . snd) infos)
  
-- ** Activation 
-- | Activates/deactivate and item in FrontAccounting.
changeFAActivation :: Bool -> IndexParam -> Handler ()
changeFAActivation activate_ param = do
  changeActivation param
                   [StockMasterInactive ==. activate_]
                   (\stockId ->  update (StockMasterKey stockId) [StockMasterInactive =. not activate_])
  changeActivation param
                   [StockMasterInactive ==. activate_]
                   (\stockId ->  updateWhere [ItemCodeStockId ==. stockId ]  [ItemCodeInactive =. not activate_])
  clearAppCache

changeActivation :: IndexParam -> [ Filter StockMaster ]  -> (Text -> SqlHandler ()) -> Handler ()
changeActivation param activeFilter updateFn =  runDB $ do
  -- we set ShowInactive to true to not filter anything yet using (StockMasterInactive)
  -- as it will be done before
  entities <- case selectedItemsFilter param {ipMode = ItemGLView, ipShowInactive = ShowAll } of
    Left err -> error err >> return []
    Right filter_ ->  selectList ( activeFilter ++ filter_) []
  -- traceShowM ("ACTI", param, entities)
  let toKeep = checkFilter param
      keys_ = [ unStockMasterKey key
             | (Entity key _) <- entities
             , toKeep (unStockMasterKey key)
             ] -- :: [Key StockMaster]
  mapM_ updateFn keys_

-- | Activated and item in  FrontAccounting or the Website
activate :: IndexParam -> Handler ()
activate param = case (ipMode param ) of
  _ -> changeFAActivation True param

-- | deactivate and item in  FrontAccounting or the Website
deactivate :: IndexParam -> Handler ()
deactivate param = case (ipMode param ) of
  _ -> changeFAActivation False param
  
  
