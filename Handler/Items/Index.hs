{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Index where

import Import
import Yesod.Form.Bootstrap3
import FA

-- * Types
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data FilterExpression = LikeFilter Text  | RegexFilter Text deriving (Eq, Show, Read)
data IndexParam = IndexParam
  { ipStyles :: Maybe FilterExpression
  , ipVariations :: Maybe FilterExpression
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
  

itemsTable :: Maybe FilterExpression -> Maybe FilterExpression -> Handler Widget
itemsTable styleF varF = runDB $ do
  let conv = StockMasterKey
  styles <- selectList (filterE conv FA.StockMasterId styleF)
                [Asc FA.StockMasterId]
  variations <- selectList (filterE conv FA.StockMasterId varF <> [FA.StockMasterInactive ==. False ])
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
      rows = [(t"normal", key, val) | (Entity key val) <- styles]
      t = \x -> x :: Text
  
  return [whamlet|
<table.well.table.table-stripped.table-bordered>
  <theader>
    <tr>
      $forall col <- columns
        <th>#{col}
  <tbody>
    $forall (class_, sku, row) <- rows
      <tr class="#{class_}">
        <td.stock_id> #{ unStockMasterKey $ sku}
        <td.categoryId> #{ tshow $ stockMasterCategoryId row} 
        <td.taxTypeId> #{ tshow $ stockMasterTaxTypeId row} 
        <td.description> #{ stockMasterDescription row} 
        <td.longDescription> #{ stockMasterLongDescription row} 
        <td.units> #{ stockMasterUnits row} 
        <td.mbFlag> #{ stockMasterMbFlag row} 
        <td.salesAccount> #{ stockMasterSalesAccount row} 
        <td.cogsAccount> #{ stockMasterCogsAccount row} 
        <td.inventoryAccount> #{ stockMasterInventoryAccount row} 
        <td.adjustmentAccount> #{ stockMasterAdjustmentAccount row} 
        <td.assemblyAccount> #{ stockMasterAssemblyAccount row} 
        <td.dimensionId> #{ tshow $ stockMasterDimensionId row} 
        <td.dimension2Id> #{ tshow $ stockMasterDimension2Id row} 
        <td.actualCost> #{ tshow $ stockMasterActualCost row} 
        <td.lastCost> #{ tshow $ stockMasterLastCost row} 
        <td.materialCost> #{ tshow $ stockMasterMaterialCost row} 
        <td.labourCost> #{ tshow $ stockMasterLabourCost row} 
        <td.overheadCost> #{ tshow $ stockMasterOverheadCost row} 
        <td.inactive> #{ tshow $ stockMasterInactive row} 
        <td.noSale> #{ tshow $ stockMasterNoSale row} 
        <td.editable> #{ tshow $ stockMasterEditable row} 
                 |]

-- * Rendering
getItemsIndexR :: Handler Html
getItemsIndexR = renderIndex (Just $ IndexParam (Just "%AD1%") (Just "/^MW16-.*-BLK")) ok200

indexForm param = renderBootstrap3 BootstrapBasicForm form
  where form = IndexParam
          <$> (aopt filterEField "styles" (fmap ipStyles param))
          <*> (aopt filterEField "variations" (fmap ipVariations param))

renderIndex :: (Maybe IndexParam) -> Status -> Handler Html
renderIndex param status = do
  (form, encType) <- generateFormPost (indexForm param)
  index <- itemsTable (ipStyles =<< param) (ipVariations =<< param)
  let widget = [whamlet|
<div #items-index>
  <div.well>
    <form #items-form role=form method=post action=@{ItemsR ItemsIndexR} enctype=#{encType}>
      ^{form}
      <button type="submit" name="search" class="btn btn-default">Search
 <div.well>
   ^{index}
|]
  sendResponseStatus status =<< defaultLayout widget


  




