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
import Text.Printf(printf)

-- * Types
data FormParam  = FormParam
  { pBarcode :: Maybe FilterExpression
  , pLocation :: Maybe FilterExpression
  , pDescription :: Maybe FilterExpression
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
          <$> aopt filterEField "Barcode" (pBarcode <$> param0)
          <*> aopt filterEField "Location" (pLocation <$> param0)
          <*> aopt filterEField "Description" (pDescription <$> param0)
          <*> areq boolField "Show Inactive" ((pShowInactive) <$> param0)
          <*> areq boolField "Compact mode" (pCompactView <$> param0 <|> Just True)

-- * Rendering
-- ** Main
renderBoxtakes :: FormParam -> Handler Html
renderBoxtakes param = do
  boxtakes <- loadBoxtakes param
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  opMap <- allOperators
  let body = case pCompactView param  of
        True -> renderBoxtakeTable opMap boxtakes
        False -> do
          renderBoxtakeList opMap boxtakes
  defaultLayout $ [whamlet|
<form #boxtakes role=form method=post action=@{WarehouseR WHBoxtakeR} encType="#{encType}"}>
  ^{formW}
  <button type="submit" name="Search" .btn.btn-primary> Search
<div>
  <div.panel.panel-info>
    <div.panel-heading><h2> Summary
    <div.panel-body> ^{renderSummary boxtakes}
  ^{body}
          |]
  
-- *** Table
renderBoxtakeTable :: (Map (Key Operator) Operator) -> [Entity Boxtake] -> Widget
renderBoxtakeTable opMap boxtakes = do
  [whamlet|
<table.table.table-bordered.table-striped.table-hover>
  <tr>
      <th> Barcode 
      <th> Description 
      <th> Dimensions
      <th> Volume
      <th> Location
      <th> Date 
      <th> Operator
      <th> Active
  $forall (Entity _ boxtake) <- boxtakes
    <tr>
      <td> <a href=@{WarehouseR (WHBoxtakeDetailR (boxtakeBarcode boxtake))}>#{boxtakeBarcode boxtake}
      <td> #{fromMaybe "" (boxtakeDescription boxtake)}
      <td> #{tshow (boxtakeLength boxtake)} x #{tshow (boxtakeWidth boxtake)} x #{tshow (boxtakeHeight boxtake)}
            ^{dimensionPicture 64 boxtake}
      <td> #{formatVolume (boxtakeVolume boxtake)}
      <td> #{boxtakeLocation boxtake}
      <td> #{tshow (boxtakeDate boxtake)}
      <td> #{opName opMap (boxtakeOperator boxtake)}
      <td> #{displayActive (boxtakeActive boxtake)}
          |]
  
-- *** List
renderBoxtakeList :: (Map (Key Operator) Operator) ->  [Entity Boxtake] -> Widget
renderBoxtakeList opMap boxtakes = do
  mapM_ (\box -> renderBoxtakeDetail opMap box [] ) boxtakes

-- ** Detail

renderBoxtakeDetail :: (Map (Key Operator) Operator) -> Entity Boxtake  -> [Entity Stocktake] -> Widget
renderBoxtakeDetail opMap (Entity _ boxtake@Boxtake{..}) stocktakes = do
  let panelClass = if boxtakeActive
                   then  "success" :: Text
                   else  "danger"
      day'locS = nub $ (boxtakeDate, boxtakeLocation) : boxtakeLocationHistory
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
           <td> #{opName opMap boxtakeOperator}, the #{tshow boxtakeDate}
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

renderSummary :: [Entity Boxtake] -> Widget
renderSummary boxtakes =  do
  let n = length boxtakes
      volume = sum (map (boxtakeVolume . entityVal) boxtakes)
  [whamlet|
<table.table>
  <tr>
     <td> Number of Boxes
     <td> #{tshow n}
  <tr>
     <td> Total Volume
     <td> #{formatVolume volume} m<sup>3
          |]


-- ** DB Access
loadBoxtakes :: FormParam -> Handler [Entity Boxtake]
loadBoxtakes param = do
  let filter = filterE id BoxtakeBarcode (pBarcode param)
            <> filterE id BoxtakeLocation (pLocation param)
            <> filterE Just BoxtakeDescription (pDescription param)
      opts = case filter of
        [] -> -- no filter, we want the last ones
             [Desc BoxtakeId, LimitTo 50]
        _ -> -- filter, we use the filter to sort as well
          catMaybes [ pBarcode param <&> (const $ Asc BoxtakeBarcode)
                    , pLocation param <&> (const $ Asc BoxtakeLocation)
                    , pDescription param <&> (const $ Asc BoxtakeDescription)
                    ] <> [Asc BoxtakeDescription]
      active = if pShowInactive param
               then []
               else [BoxtakeActive ==. True]
  runDB $ selectList (active <> filter) opts
  
-- ** Util
displayActive :: Bool -> Text
displayActive act = if act then "Active" else "Inactive"

  
dimensionPicture :: Int -> Boxtake -> Widget
dimensionPicture width Boxtake{..} =  do
  let dimRoute = WarehouseR $ WHDimensionOuterR (round boxtakeLength) (round boxtakeWidth) (round boxtakeLength)
  [whamlet|
      <a href="@{dimRoute}" ><img src=@?{(dimRoute , [("width", tshow width)])}>
         |]

opName :: (Map (Key Operator) Operator) -> Key Operator -> Text
opName opMap key = maybe "" operatorNickname (lookup key opMap)

boxtakeVolume :: Boxtake -> Double
boxtakeVolume Boxtake{..} = boxtakeLength * boxtakeWidth * boxtakeHeight / 1e6

formatVolume :: Double -> String
formatVolume v = printf "%0.3f" v
