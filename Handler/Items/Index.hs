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
      limit = case styleF of
                Nothing -> [LimitTo 200]
                Just _ -> []
  styles <- selectList (filterE conv FA.StockMasterId styleF)
                ([Asc FA.StockMasterId] ++ limit)
  variations <- case varF of
    Nothing -> return styles
    Just _  -> selectList (filterE conv FA.StockMasterId varF <> [FA.StockMasterInactive ==. False ])
                [Asc FA.StockMasterId]

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
  let itemToF :: (VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))
              -> (Text -> Maybe (Html, [Text]) -- Html + classes per column
                 , [Text]) -- classes for row

      itemToF (status , ItemInfo style var stock) =
        let val col = case col of
              "stock_id" -> Just ([], style <> "-" <> var)
              "categoryId" -> Just ( tshow <$> smiCategoryId stock )
              "taxTypeId" -> Just $ tshow <$> smiTaxTypeId stock 
              "description" -> Just $ smiDescription stock 
              "longDescription" -> Just $ smiLongDescription stock 
              "units" -> Just $ smiUnits stock 
              "mbFlag" -> Just $ smiMbFlag stock 
              "salesAccount" -> Just $ smiSalesAccount stock 
              "cogsAccount" -> Just $ smiCogsAccount stock 
              "inventoryAccount" -> Just $ smiInventoryAccount stock 
              "adjustmentAccount" -> Just $ smiAdjustmentAccount stock 
              "assemblyAccount" -> Just $ smiAssemblyAccount stock 
              "dimensionId" -> Just $ tshow <$> smiDimensionId stock 
              "dimension2Id" -> Just $ tshow <$> smiDimension2Id stock 
              "actualCost" -> Just $ tshow <$> smiActualCost stock 
              "lastCost" -> Just $ tshow <$> smiLastCost stock 
              "materialCost" -> Just $ tshow <$> smiMaterialCost stock 
              "labourCost" -> Just $ tshow <$> smiLabourCost stock 
              "overheadCost" -> Just $ tshow <$> smiOverheadCost stock 
              "inactive" -> Just $ tshow <$> smiInactive stock 
              "noSale" -> Just $ tshow <$> smiNoSale stock 
              "editable" -> Just $ tshow <$> smiEditable stock 
              _ -> Nothing
            classes :: [Text]
            classes = case smiInactive stock of
                           (_, True) -> ["text-muted"]
                           _ -> []
                      ++ case status of
                           VarOk -> []
                           VarMissing -> ["danger"]
                           VarExtra -> ["info"]
     
        in (\col -> fmap (\(fieldClasses, v)
                    -> (toHtml v, "stock-master-col":fieldClasses)
                       ) (val col)
           , classes
           )

      stockMasterToItem (Entity key val) = ItemInfo  style var val where
                  sku = unStockMasterKey key
                  style = take 8 sku
                  var = drop 9 sku
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


  




