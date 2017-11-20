{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Handler.WH.Boxtake
( getWHBoxtakeR
, postWHBoxtakeR
, getWHBoxtakeDetailR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import Data.List(nub)

-- * Types
data FormParam  = FormParam
  { pBarcode :: Maybe Text
  , pLocation :: Maybe Text
  , pDescription :: Maybe Text
  , pShowInactive :: Bool
  , pCompactView :: Bool
  }
defaultParam = FormParam Nothing Nothing Nothing False True

-- * Handlers
getWHBoxtakeR :: Handler Html
getWHBoxtakeR = do
  renderBoxtakes defaultParam

postWHBoxtakeR :: Handler Html
postWHBoxtakeR = do
  ((resp, _), _) <- runFormPost (paramForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderBoxtakes param

-- | Display the detail of a box given a barcode.
getWHBoxtakeDetailR :: Text -> Handler Html
getWHBoxtakeDetailR barcode = do
  boxtakem <- runDB $ getBy  (UniqueBB barcode)
  stocktakes<- runDB $ selectList [StocktakeBarcode ==. barcode] [Desc StocktakeDate, Asc StocktakeIndex]
  operatorsMap <- allOperators
  defaultLayout =<< case boxtakem of
      Nothing -> setError (toHtml ("Can't find any box with barcode '" <> barcode <> "'")) >> return ""
      Just boxtake -> return $ renderBoxtakeDetail operatorsMap boxtake stocktakes
  
-- * Forms
paramForm :: Maybe FormParam -> _
paramForm param0 = renderBootstrap3 BootstrapBasicForm form
  where form = FormParam
          <$> aopt textField "Barcode" (pBarcode <$> param0)
          <*> aopt textField "Location" (pLocation <$> param0)
          <*> aopt textField "Description" (pDescription <$> param0)
          <*> areq boolField "Show Inactive" ((pShowInactive) <$> param0)
          <*> areq boolField "Compact mode" (pCompactView <$> param0 <|> Just True)

-- * Rendering
-- ** Main
renderBoxtakes :: FormParam -> Handler Html
renderBoxtakes param = do
  boxtakes <- loadBoxtakes param
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  let body = case pCompactView param  of
        True -> renderBoxtakeTable boxtakes
        False -> renderBoxtakeList boxtakes
  defaultLayout $ [whamlet|
<form #boxtakes role=form method=post action=@{WarehouseR WHBoxtakeR} encType="#{encType}"}>
  ^{formW}
  <button type="submit" name="Search" .btn.btn-primary> Search
<div>
  ^{body}
          |]
  
-- *** Table
renderBoxtakeTable :: [Entity Boxtake] -> Widget
renderBoxtakeTable boxtakes = do
  [whamlet|
<table.table.table-bordered.table-striped.table-hover>
  <tr>
      <th> Barcode 
      <th> Description 
      <th> Dimensions
      <th> Reference 
      <th> Location
      <th> Date 
      <th> Active
  $forall (Entity _ boxtake) <- boxtakes
    <tr>
      <td> <a href=@{WarehouseR (WHBoxtakeDetailR (boxtakeBarcode boxtake))}>#{boxtakeBarcode boxtake}
      <td> #{fromMaybe "" (boxtakeDescription boxtake)}
      <td> #{tshow (boxtakeLength boxtake)} x #{tshow (boxtakeWidth boxtake)} x #{tshow (boxtakeHeight boxtake)}
            ^{dimensionPicture 64 boxtake}
      <td> #{boxtakeReference boxtake}
      <td> #{boxtakeLocation boxtake}
      <td> #{tshow (boxtakeDate boxtake)}
      <td> #{displayActive (boxtakeActive boxtake)}
          |]
  
-- *** List
renderBoxtakeList :: [Entity Boxtake] -> Widget
renderBoxtakeList = return "list"

-- ** Detail

renderBoxtakeDetail :: (Map (Key Operator) Operator) -> Entity Boxtake  -> [Entity Stocktake] -> Widget
renderBoxtakeDetail opMap (Entity _ boxtake@Boxtake{..}) stocktakes = do
  let panelClass = if boxtakeActive
                   then  "success" :: Text
                   else  "danger"
      day'locS = nub $ (boxtakeDate, boxtakeLocation) : boxtakeLocationHistory
      opName key = maybe "" operatorNickname (lookup key opMap)
      history = [whamlet|
<table.table.table-bordered.table-striped.table-hover>
    <tr>
      <th> Date
      <th> Location
    $forall  (day, loc) <- day'locS
      <tr>
        <td> #{tshow day}
        <td> #{loc}
                             |]
      content = case stocktakes of
        [] -> return ()
        _ -> renderStocktakes opMap stocktakes
  [whamlet|
<div.panel class="panel-#{panelClass}">
  <div.panel-heading> Box: #{boxtakeBarcode}
  <div.panel-body>
    <div.col-sm-6>
      <table.table>
        <tr>
          <td>Barcode
          <td>#{boxtakeBarcode}
        <tr>
          <td>Description
          <td>#{fromMaybe "" boxtakeDescription}
        <tr>
          <td> Reference
          <td> #{boxtakeReference}
        <tr>
          <td> Active
          <td> #{displayActive boxtakeActive}
        <tr>
          <td> Dimensions
          <td> #{boxtakeLength} x #{boxtakeWidth} x #{boxtakeHeight}
        <tr>
          <td> Location
          <td> #{boxtakeLocation}
        <tr>
           <td> Last scan
           <td> #{opName boxtakeOperator}, the #{tshow boxtakeDate}
        ^{content}
    <div.col-sm-6>
      ^{history}
      ^{dimensionPicture 400 boxtake}
          |]
  

renderStocktakes :: (Map (Key Operator) Operator) -> [Entity Stocktake]  -> Widget
renderStocktakes opMap stocktakes = do
  [whamlet|
<table.table.table-bordered.table-striped.table.hover>
  <tr>
     <th> Stock Id
     <th> Quantity
     <th> Date
     <th> Active
  $forall (Entity _ stocktake) <- stocktakes
    <tr>
      <td> #{stocktakeStockId stocktake}
      <td> #{tshow (stocktakeQuantity stocktake)}
      <td> #{tshow (stocktakeDate stocktake)}
      <td> #{displayActive (stocktakeActive stocktake)}
          |]


-- ** DB Access
loadBoxtakes :: FormParam -> Handler [Entity Boxtake]
loadBoxtakes param = do
  runDB $ selectList [] [Desc BoxtakeId, LimitTo 50]
  
-- ** Util
displayActive :: Bool -> Text
displayActive act = if act then "Active" else "Inactive"

  
dimensionPicture :: Int -> Boxtake -> Widget
dimensionPicture width Boxtake{..} =  do
  let dimRoute = WarehouseR $ WHDimensionOuterR (round boxtakeLength) (round boxtakeWidth) (round boxtakeLength)
  [whamlet|
      <a href="@{dimRoute}" ><img src=@?{(dimRoute , [("width", tshow width)])}>
         |]
