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
-- * Types
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data FilterExpression = LikeFilter Text  | RegexFilter Text deriving (Eq, Show, Read)
data IndexParam = IndexParam
  { ipStyles :: Maybe FilterExpression
  , ipVariations :: Maybe FilterExpression
  , ipVariationGroup :: Maybe Text -- ^ Alternative to variations
  , ipShowInactive :: Bool
  , ipBases :: Map Text Text -- ^ style -> selected base
  , ipChecked :: [Text] -- ^ styles to act upon
  , ipColumns :: [Text] -- ^ columns to act upon
  } deriving (Eq, Show, Read)

-- * Utils

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
  

itemsTable :: Map Text Text -> Maybe [Text] -> Maybe FilterExpression -> Either FilterExpression (Maybe Text) -> Bool ->  Handler Widget
itemsTable bases checkedItems styleF varF showInactive = do
  renderUrl <- getUrlRenderParams
  adjustBase <- getAdjustBase
  varGroupMap <- appVariationGroups <$> appSettings <$> getYesod
  runDB $ do
    let conv = StockMasterKey
    styles <- case styleF of
      Nothing -> do
                setWarning "Please enter a styles filter expression (SQL like expression or regexp starting with '/'')"
                return []
      Just _ -> selectList (filterE conv FA.StockMasterId styleF
                            ++ if showInactive then [] else [FA.StockMasterInactive ==. False]
                          )
                          [Asc FA.StockMasterId]
    variations <- case varF of
      (Right Nothing) -> return (Left styles)
      (Left filter_)  -> Left <$> selectList (filterE conv FA.StockMasterId (Just filter_)
                                            <> [FA.StockMasterInactive ==. False ]
                                            )
                                            [Asc FA.StockMasterId]
      (Right (Just group_)) -> return $ Right (Map.findWithDefault [] group_ varGroupMap)

    let columns' = [ "stock_id"
                  , "status"
                  , "categoryId"
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
        columns = ["check", "radio"] ++ columns'

    -- Church encoding ?
    let itemToF :: ItemInfo StockMaster
                -> (VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))
                -> (Text -> Maybe (Html, [Text]) -- Html + classes per column
                  , [Text]) -- classes for row

        itemToF item0 (status , ItemInfo style var stock) =
          let sku =  style <> "-" <> var
              val col = case col of
                "check" -> let checked = maybe False (sku `elem`) checkedItems
                           in Just ([], [shamlet|<input type=checkbox name="check-#{sku}" :checked:checked>|])
                "radio" -> let checked = var == iiVariation item0
                           in if status == VarMissing
                              then Nothing
                              else Just ([], [shamlet|<input type=radio name="base-#{style}" value="#{sku}"
                                                  :checked:checked
                                             >|])

                "stock_id" -> let route = ItemsR $ ItemsHistoryR sku
                              in Just ([], [hamlet|<a href=@{route} target="_blank">#{sku}|] renderUrl )
                "status" -> let label = case status of
                                    VarMissing -> [shamlet| <span.label.label-danger> Missing |]
                                    VarExtra -> [shamlet| <span.label.label-info> Extra |]
                                    VarOk -> [shamlet||]
                                label' = case False of
                                    True -> [shamlet| <span.label.label-warning> Diff |]
                                    _ -> [shamlet||]
                            in Just ([], [hamlet|#{label}#{label'}|] renderUrl )
                "categoryId" -> Just (toHtml . tshow <$> smiCategoryId stock )
                "taxTypeId" -> Just $ toHtml <$> smiTaxTypeId stock 
                "description" -> Just $ toHtml <$>  smiDescription stock 
                "longDescription" -> Just $ toHtml <$> smiLongDescription stock 
                "units" -> Just $ toHtml <$>  smiUnits stock 
                "mbFlag" -> Just $ toHtml <$>  smiMbFlag stock 
                "salesAccount" -> Just $ toHtml <$>  smiSalesAccount stock 
                "cogsAccount" -> Just $ toHtml <$>  smiCogsAccount stock 
                "inventoryAccount" -> Just $ toHtml <$>  smiInventoryAccount stock 
                "adjustmentAccount" -> Just $ toHtml <$>  smiAdjustmentAccount stock 
                "assemblyAccount" -> Just $ toHtml <$>  smiAssemblyAccount stock 
                "dimensionId" -> Just $ toHtml . tshow  <$> smiDimensionId stock 
                "dimension2Id" -> Just $ toHtml . tshow <$> smiDimension2Id stock 
                "actualCost" -> Just $ toHtml <$> smiActualCost stock 
                "lastCost" -> Just $ toHtml <$> smiLastCost stock 
                "materialCost" -> Just $ toHtml <$> smiMaterialCost stock 
                "labourCost" -> Just $ toHtml <$> smiLabourCost stock 
                "overheadCost" -> Just $ toHtml <$> smiOverheadCost stock 
                "inactive" -> Just $ toHtml <$> smiInactive stock 
                "noSale" -> Just $ toHtml <$> smiNoSale stock 
                "editable" -> Just $ toHtml <$> smiEditable stock 
                _ -> Nothing
              classes :: [Text]
              classes = "style-" <> iiStyle item0:
                case smiInactive stock of
                            (_, True) -> ["text-muted"]
                            _ -> []
                        -- ++ case status of
                        --     VarOk -> []
                        --     VarMissing -> ["danger"]
                        --     VarExtra -> ["info"]
                      ++ if var == iiVariation item0
                          then ["base"]
                          else ["variation"]

          in (\col -> fmap (\(fieldClasses, v)
                      -> (v, ("stock-master-"<>col):fieldClasses)
                        ) (val col)
            , classes
            )

        skuToStyleVar sku = (style, var) where
          style = take 8 sku
          var = drop 9 sku
        stockMasterToItem (Entity key val) = ItemInfo  style var val where
                    sku = unStockMasterKey key
                    (style, var) = skuToStyleVar sku
        itemStyles = map stockMasterToItem styles
        itemVars =  case variations of
          Left entities -> map (iiVariation . stockMasterToItem) entities
          Right vars -> vars
        itemGroups = joinStyleVariations (skuToStyleVar <$> bases) adjustBase itemStyles itemVars

        -- We keep row grouped so we can change the style of the first one of every group.
        rowGroup = map (\(base, _, vars) -> map (itemToF base) vars) itemGroups
        styleFirst ((fn, klasses):rs) = (fn, "style-start":klasses):rs
        styleFirst [] = error "Shouldn't happend"

        styleGroup klass rs = [(fn, klass:klasses) | (fn,klasses) <- rs]
        

    return $ displayTable columns
                          (\c -> case c of
                              "check" -> ("", [])
                              "radio" -> ("", [])
                              _ -> (toHtml c, [])
                          )
                          -- (concat  (zipWith styleGroup (List.cycle ["group-1","group-2","group-3","group-4"])
                          (concat  (zipWith styleGroup (List.cycle ["group-2","group-3"])
                                    .map styleFirst $ rowGroup))
                      

-- | Fill the parameters which are not in the form but have to be extracted
-- from the request parameters
fillTableParams :: IndexParam -> Handler IndexParam
fillTableParams params0 = do
  (params,_) <- runRequestBody
  traceShowM params
  let checked = mapMaybe (stripPrefix "check-" . fst)  params
      bases = Map.fromList $ mapMaybe (\(k,v) -> stripPrefix "base-" k <&> (\b -> (b, v))
                                     ) params
  return $ traceShowId $ params0 {ipChecked = checked, ipBases=bases}
   

-- * Rendering
getItemsIndexR :: Handler TypedContent
getItemsIndexR = do
  let param0 = IndexParam Nothing Nothing Nothing False mempty empty empty
  renderIndex param0 ok200

postItemsIndexR :: Handler TypedContent
postItemsIndexR = do
  let param0 = IndexParam Nothing Nothing Nothing False mempty empty empty
  param <- fillTableParams param0
  renderIndex param ok200
-- indexForm :: (MonadHandler m,
--               RenderMessage (HandlerSite m) FormMessage)
--           => [Text]
--           -> IndexParam
--           -> Markup
--           -> MForm m (FormResult IndexParam, WidgetT (HandlerSite m) IO ())
indexForm groups param = renderBootstrap3 BootstrapBasicForm form
  where form = IndexParam
          <$> (aopt filterEField "styles" (Just $ ipStyles param))
          <*> (aopt filterEField "variations" (Just $ ipVariations param))
          <*> (aopt (selectFieldList groups') "variation group" (Just $ ipVariationGroup param))
          <*> (areq boolField "Show Inactive" (Just $ ipShowInactive param))
          <*> pure (ipBases param)
          <*> pure (ipChecked param)
          <*> pure (ipColumns param)
        groups' =  map (\g -> (g,g)) groups

renderIndex :: IndexParam -> Status -> Handler TypedContent
renderIndex param0 status = do
  varGroup <- appVariationGroups <$> getsYesod appSettings
  ((resp, form), encType) <- runFormPost (indexForm (Map.keys varGroup) param0)
  let param = case resp of
        FormMissing -> param0
        FormSuccess par -> par
        FormFailure _ -> param0
      varP = case (ipVariations param, ipVariationGroup param) of
        (Just var, Nothing) -> Left var
        (_, group_) -> Right group_
      checkedItems = if null (ipChecked param) then Nothing else Just (ipChecked param)
  ix <- itemsTable (ipBases param) checkedItems (ipStyles param) varP (ipShowInactive param)
  let css = [cassius|
#items-index
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
|]
  let widget = [whamlet|
<div #items-index>
  <form #items-form role=form method=post action=@{ItemsR ItemsIndexR} enctype=#{encType}>
    <div.well>
      ^{form}
      <button type="submit" name="search" class="btn btn-default">Search
    <div#items-table>
      ^{ix}
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
      


  

getAdjustBase :: Handler (ItemInfo StockMaster -> Text -> ItemInfo StockMaster)
getAdjustBase = do
  settings <- appSettings <$> getYesod 
  let varMap = appVariations settings
      go item0@(ItemInfo _ _ stock ) var = let
        description = adjustDescription varMap (iiVariation item0) var (stockMasterDescription stock)
        in item0 {iiInfo = stock {stockMasterDescription=description}}
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

-- | Split a variation name to variations
-- ex: A/B -> [A,B]
variationToVars :: Text -> [Text]
variationToVars var = splitOn "/" var


-- | Inverse of variationToVars
varsToVariation :: [Text] -> Text
varsToVariation vars = intercalate "/" vars

-- | Lookup for list of variation code
lookupVars :: Map Text Text -> Text -> [Text]
lookupVars varMap = mapMaybe (flip Map.lookup varMap) . variationToVars
