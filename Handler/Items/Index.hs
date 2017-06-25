{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Index where

import Import
import Handler.Table
import Yesod.Form.Bootstrap3
import FA
import Items

-- * Types
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data FilterExpression = LikeFilter Text  | RegexFilter Text deriving (Eq, Show, Read)
data IndexParam = IndexParam
  { ipStyles :: Maybe FilterExpression
  , ipVariations :: Maybe FilterExpression
  , ipShowInactive :: Bool
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

filterEField = convertField readFilterExpression showFilterExpression textField

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
  

itemsTable :: Maybe FilterExpression -> Maybe FilterExpression -> Bool ->  Handler Widget
itemsTable styleF varF showInactive = do
  renderUrl <- getUrlRenderParams
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
      Nothing -> return styles
      Just _  -> selectList (filterE conv FA.StockMasterId varF <> [FA.StockMasterInactive ==. False ])
                  [Asc FA.StockMasterId]

    let columns = [ "stock_id"
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
                  , "actualCost"
                  , "lastCost"
                  , "materialCost"
                  , "labourCost"
                  , "overheadCost"
                  , "inactive"
                  , "noSale"
                  , "editable"
                  ] :: [Text]

    -- Church encoding ?
    let itemToF :: ItemInfo StockMaster
                -> (VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))
                -> (Text -> Maybe (Html, [Text]) -- Html + classes per column
                  , [Text]) -- classes for row

        itemToF item0 (status , ItemInfo style var stock) =
          let val col = case col of
                "stock_id" -> let sku =  style <> "-" <> var
                                  route = ItemsR $ ItemsHistoryR sku
                              in Just ([], [hamlet|<a href=@{route} target="_blank">#{sku}|] renderUrl )
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
                        ++ case status of
                            VarOk -> []
                            VarMissing -> ["danger"]
                            VarExtra -> ["info"]
                      ++ if var == iiVariation item0
                          then ["bg-info", "base"]
                          else ["variation"]

          in (\col -> fmap (\(fieldClasses, v)
                      -> (v, ("stock-master-"<>col):fieldClasses)
                        ) (val col)
            , classes
            )

        stockMasterToItem (Entity key val) = ItemInfo  style var val where
                    sku = unStockMasterKey key
                    style = take 8 sku
                    var = drop 9 sku
        itemStyles = map stockMasterToItem styles
        itemVars =  map stockMasterToItem variations
        itemGroups = joinStyleVariations itemStyles itemVars

    return $ displayTable columns
                          (\c -> (toHtml c, []))
                          (concatMap (\(base, aggregate, vars) -> map (itemToF base) vars) itemGroups)
                      

-- * Rendering
getItemsIndexR :: Handler Html
getItemsIndexR = renderIndex (IndexParam Nothing Nothing False) ok200

indexForm param = renderBootstrap3 BootstrapBasicForm form
  where form = IndexParam
          <$> (aopt filterEField "styles" (Just $ ipStyles param))
          <*> (aopt filterEField "variations" (Just $ ipVariations param))
          <*> (areq boolField "Show Inactive" (Just $ ipShowInactive param))

renderIndex :: IndexParam -> Status -> Handler Html
renderIndex param0 status = do
  ((resp, form), encType) <- runFormGet (indexForm param0)
  let param = case resp of
        FormMissing -> param0
        FormSuccess par -> par
        FormFailure _ -> param0
  index <- itemsTable (ipStyles param) (ipVariations param) (ipShowInactive param)
  let widget = [whamlet|
<div #items-index>
  <div.well>
    <form #items-form role=form method=get action=@{ItemsR ItemsIndexR} enctype=#{encType}>
      ^{form}
      <button type="submit" name="search" class="btn btn-default">Search
  ^{index}
|]
      fay = $(fayFile "ItemsIndex")
  sendResponseStatus status =<< defaultLayout (widget >> fay)


  




