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

-- * Handlers
getWHBoxtakeR :: Handler Html
getWHBoxtakeR = undefined

postWHBoxtakeR :: Handler Html
postWHBoxtakeR = undefined

-- | Display the detail of a box given a barcode.
getWHBoxtakeDetailR :: Text -> Handler Html
getWHBoxtakeDetailR barcode = do
  boxtakem <- runDB $ getBy  (UniqueBB barcode)
  stocktakes<- runDB $ selectList [StocktakeBarcode ==. barcode] [Desc StocktakeDate, Asc StocktakeIndex]
  operatorsMap <- allOperators
  defaultLayout =<< case boxtakem of
      Nothing -> setError (toHtml ("Can't find any box with barcode '" <> barcode <> "'")) >> return ""
      Just boxtake -> return $ renderBoxtakeDetail operatorsMap boxtake stocktakes
   

-- * Rendering
-- ** Detail

renderBoxtakeDetail :: (Map (Key Operator) Operator) -> Entity Boxtake  -> [Entity Stocktake] -> Widget
renderBoxtakeDetail opMap (Entity _ Boxtake{..}) stocktakes = do
  let dimRoute = WarehouseR $ WHDimensionOuterR (round boxtakeLength) (round boxtakeWidth) (round boxtakeLength)
      panelClass = if boxtakeActive
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
      <a href="@{dimRoute}" ><img src=@?{(dimRoute , [("width", "400")])}>
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


-- ** Util
displayActive :: Bool -> Text
displayActive act = if act then "Active" else "Inactive"

  
