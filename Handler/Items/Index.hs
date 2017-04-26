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
                [Asc FA.StockMasterId, LimitTo 100]
  variations <- selectList (filterE conv FA.StockMasterId varF <> [FA.StockMasterInactive ==. False ])
                [Asc FA.StockMasterId, LimitTo 100]

  let columns = [ "stock_id"
                -- , "categoryId"
                -- , "taxTypeId"
                , "description"
                , "longDescription"
                -- , "units"
                -- , "mbFlag"
                , "salesAccount"
                , "cogsAccount"
                , "inventoryAccount"
                , "adjustmentAccount"
                -- , "assemblyAccount"
                , "dimensionId"
                , "dimension2Id"
                -- , "actualCost"
                -- , "lastCost"
                -- , "materialCost"
                -- , "labourCost"
                -- , "overheadCost"
                , "inactive"
                -- , "noSale"
                -- , "editable"
                ] :: [Text]

  -- Church encoding ?
  let itemToF (status , ItemInfo style var stock) =
        let val col = case col of
              "stock_id" -> Just ( style <> "-" <> var)
              "categoryId" -> Just $ tshow $ stockMasterCategoryId stock 
              "taxTypeId" -> Just $ tshow $ stockMasterTaxTypeId stock 
              "description" -> Just $ stockMasterDescription stock 
              "longDescription" -> Just $ stockMasterLongDescription stock 
              "units" -> Just $ stockMasterUnits stock 
              "mbFlag" -> Just $ stockMasterMbFlag stock 
              "salesAccount" -> Just $ stockMasterSalesAccount stock 
              "cogsAccount" -> Just $ stockMasterCogsAccount stock 
              "inventoryAccount" -> Just $ stockMasterInventoryAccount stock 
              "adjustmentAccount" -> Just $ stockMasterAdjustmentAccount stock 
              "assemblyAccount" -> Just $ stockMasterAssemblyAccount stock 
              "dimensionId" -> Just $ tshow $ stockMasterDimensionId stock 
              "dimension2Id" -> Just $ tshow $ stockMasterDimension2Id stock 
              "actualCost" -> Just $ tshow $ stockMasterActualCost stock 
              "lastCost" -> Just $ tshow $ stockMasterLastCost stock 
              "materialCost" -> Just $ tshow $ stockMasterMaterialCost stock 
              "labourCost" -> Just $ tshow $ stockMasterLabourCost stock 
              "overheadCost" -> Just $ tshow $ stockMasterOverheadCost stock 
              "inactive" -> Just $ tshow $ stockMasterInactive stock 
              "noSale" -> Just $ tshow $ stockMasterNoSale stock 
              "editable" -> Just $ tshow $ stockMasterEditable stock 
              _ -> Nothing
            classes :: [Text]
            classes = if stockMasterInactive stock
                          then  ["inactive"]
                          else []
                      ++ case status of
                           VarOk -> []
                           VarMissing -> ["danger"]
                           VarExtra -> ["info"]
     
        in (\col -> fmap (\v -> (toHtml v, ["stock-master-col"])) (val col), classes)

      stockMasterToItem (Entity key val) = ItemInfo  style var val where
                  sku = unStockMasterKey key
                  style = take 8 sku
                  var = drop 10 sku
      itemStyles = map stockMasterToItem styles
      itemVars =  map stockMasterToItem variations
      items = joinStyleVariations itemStyles itemVars
        
  return $ displayTable columns
                        (\c -> (toHtml c, []))
                        (map itemToF items)
                      

-- * Rendering
getItemsIndexR :: Handler Html
getItemsIndexR = renderIndex (Just $ IndexParam Nothing Nothing) ok200

indexForm param = renderBootstrap3 BootstrapBasicForm form
  where form = IndexParam
          <$> (aopt filterEField "styles" (fmap ipStyles param))
          <*> (aopt filterEField "variations" (fmap ipVariations param))

renderIndex :: (Maybe IndexParam) -> Status -> Handler Html
renderIndex param0 status = do
  ((resp, form), encType) <- runFormGet (indexForm param0)
  let param = case resp of
        FormMissing -> param0
        FormSuccess par -> Just par
        FormFailure _ -> param0
  index <- itemsTable (ipStyles =<< param) (ipVariations =<< param)
  let widget = [whamlet|
<div #items-index>
  <div.well>
    <form #items-form role=form method=get action=@{ItemsR ItemsIndexR} enctype=#{encType}>
      ^{form}
      <button type="submit" name="search" class="btn btn-default">Search
  ^{index}
|]
  sendResponseStatus status =<< defaultLayout widget


  




