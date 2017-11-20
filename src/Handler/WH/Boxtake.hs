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
  operatorsMap <- allOperators
  defaultLayout =<< case boxtakem of
      Nothing -> setError (toHtml ("Can't find any box with barcode '" <> barcode <> "'")) >> return ""
      Just boxtake -> return $ renderBoxtakeDetail operatorsMap boxtake
  

-- * Rendering
-- ** Detail

renderBoxtakeDetail :: (Map (Key Operator) Operator) -> Entity Boxtake -> Widget
renderBoxtakeDetail opMap (Entity _ Boxtake{..}) = do
  let dimRoute = WarehouseR $ WHDimensionOuterR (round boxtakeLength) (round boxtakeWidth) (round boxtakeLength)
      (displayActive, panelClass) = if boxtakeActive
                                   then ("Active", "success") :: (Text, Text)
                                   else ("Inactive", "danger")
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
  [whamlet|
<div.panel class="panel-#{panelClass}">
  <div.panel-heading> #{boxtakeBarcode}
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
          <td> #{displayActive}
        <tr>
          <td> Dimensions
          <td> #{boxtakeLength} x #{boxtakeWidth} x #{boxtakeHeight}
        <tr>
          <td> Location
          <td> #{boxtakeLocation}
        <tr>
           <td> Last scan
           <td> #{opName boxtakeOperator}, the #{tshow boxtakeDate}
        ^{history}
    <div.col-sm-6>
      <a href="@{dimRoute}" ><img src=@?{(dimRoute , [("width", "400")])}>
          |]
  
