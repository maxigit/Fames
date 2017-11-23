{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Handler.WH.Boxtake
( getWHBoxtakeR
, postWHBoxtakeR
, getWHBoxtakeDetailR
, getWHBoxtakeValidateR
, postWHBoxtakeValidateR
, getWHBoxtakeSaveR
, postWHBoxtakeSaveR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import Data.List(nub)
import qualified Data.Map.Strict as Map
import Text.Printf(printf)
import Handler.WH.Boxtake.Upload

-- * Types
data RuptureMode = BarcodeRupture | LocationRupture | DescriptionRupture
  deriving (Eq, Read, Show, Enum, Bounded)

-- | Parameters for boxtake history,lookup,
data FormParam  = FormParam
  { pBarcode :: Maybe FilterExpression
  , pLocation :: Maybe FilterExpression
  , pDescription :: Maybe FilterExpression
  , pRuptureMode :: RuptureMode
  , pRuptureLength :: Maybe Int
  , pShowInactive :: Bool
  , pCompactView :: Bool
  }
defaultParam = FormParam Nothing Nothing Nothing LocationRupture Nothing  False True

-- | Validate or save spreadsheet.
data SavingMode = Validate | Save deriving (Eq, Read, Show)

-- | If a boxtake has been done on a full shelf, we should
-- be able to inactivate all the box previously on this shelf.
data WipeMode = WipeShelves | Addition
  deriving (Eq, Read, Show, Enum, Bounded)

-- | Parameters to upload box moves/scans.
data UploadParam = UploadParam
  { uFileInfo :: Maybe FileInfo
  , uFileKey :: Maybe Text
  , uFilePath :: Maybe Text
  , uEncoding :: Encoding
  , uWipeMode :: WipeMode
  }

-- defaultUploadParam = UploadParam Nothing Nothing Nothing UTF8 WipeShelves

-- * Requests
-- ** Boxtake history
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
  
-- ** Upload
getWHBoxtakeValidateR :: Handler Html
getWHBoxtakeValidateR = renderBoxtakeSheet Validate Nothing 202 (return ()) (return ())

postWHBoxtakeValidateR :: Handler Html
postWHBoxtakeValidateR = processBoxtakeSheet Validate

getWHBoxtakeSaveR :: Handler Html
getWHBoxtakeSaveR = renderBoxtakeSheet Save Nothing 202 (return ()) (return ())

postWHBoxtakeSaveR :: Handler Html
postWHBoxtakeSaveR = processBoxtakeSheet Validate

-- * Forms
paramForm :: Maybe FormParam -> _
paramForm param0 = renderBootstrap3 BootstrapBasicForm form
  where form = FormParam
          <$> aopt filterEField "Barcode" (pBarcode <$> param0)
          <*> aopt filterEField "Location" (pLocation <$> param0)
          <*> aopt filterEField "Description" (pDescription <$> param0)
          <*> areq (selectField optionsEnum) "Rupture" (pRuptureMode <$> param0)
          <*> aopt intField "Rupture length" (pRuptureLength <$> param0)
          <*> areq boolField "Show Inactive" ((pShowInactive) <$> param0)
          <*> areq boolField "Compact mode" (pCompactView <$> param0 <|> Just True)

uploadForm :: SavingMode -> Maybe UploadParam -> _ -> _ (FormResult UploadParam, Widget)
uploadForm mode paramM = renderBootstrap3 BootstrapBasicForm (form mode)
  where form Save = UploadParam
             <$> pure Nothing -- hidden field
             <*> areq hiddenField "key" (Just $ uFileKey =<< paramM)
             <*> areq hiddenField "path" (Just $ uFilePath =<< paramM)
             <*> areq (selectField optionsEnum ) "encoding" (fmap uEncoding paramM <|> Just UTF8)
             <*> areq (selectField optionsEnum ) "wipe mode " (fmap uWipeMode paramM <|> Just WipeShelves)
        form Validate = UploadParam
             <$> (Just <$> areq fileField "upload" (uFileInfo =<< paramM))
             <*> pure Nothing
             <*> pure Nothing
             <*> areq (selectField optionsEnum ) "encoding" (uEncoding <$> paramM <|> Just UTF8)
             <*> areq (selectField optionsEnum ) "wipe mode" (uWipeMode <$> paramM <|> Just WipeShelves)
            
-- * Rendering
-- ** Boxtake history
renderBoxtakes :: FormParam -> Handler Html
renderBoxtakes param = do
  boxtakes <- loadBoxtakes param
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  opMap <- allOperators
  body <- case pCompactView param  of
        True -> return $ renderBoxtakeTable opMap boxtakes
        False -> do
          withStocktakes <- loadStocktakes boxtakes
          return $ renderBoxtakeList opMap withStocktakes
  defaultLayout $ [whamlet|
<form #boxtakes role=form method=post action=@{WarehouseR WHBoxtakeR} encType="#{encType}"}>
  ^{formW}
  <button type="submit" name="Search" .btn.btn-primary> Search
<div>
  <div.panel.panel-info>
    <div.panel-heading><h2> Summary
    <div.panel-body> ^{renderSummary param boxtakes}
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
renderBoxtakeList :: (Map (Key Operator) Operator) ->  [(Entity Boxtake, [Entity Stocktake])] -> Widget
renderBoxtakeList opMap boxtake'stocktakesS = do
  mapM_ (\(box, sts) -> renderBoxtakeDetail opMap box sts ) boxtake'stocktakesS

-- ** Detail

renderBoxtakeDetail :: (Map (Key Operator) Operator) -> Entity Boxtake  -> [Entity Stocktake] -> Widget
renderBoxtakeDetail opMap (Entity _ boxtake@Boxtake{..}) stocktakes = do
  let panelClass = case (boxtakeActive, stocktakes) of
                       (True, []) -> "warning" :: Text
                       (True, _) -> "success" :: Text
                       (False, _ ) ->   "danger"
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

renderSummary :: FormParam -> [Entity Boxtake] -> Widget
renderSummary param boxtakes =  do
  let (n, volume) = summary boxtakes
      groups = case pRuptureLength  param of
                Just l | l /= 0 -> rupture (pRuptureMode param) l boxtakes
                _ -> []
  [whamlet|
<table.table>
  <tr>
     <th>
     <th> Number of Boxes
     <th> Volume
  $forall (key, group) <- groups
    $with (n1, v1) <- summary group
     <tr>
       <td> #{key}
       <td> #{tshow n1}
       <td> #{formatVolume v1}
  <tr>
     <th> Total
     <th> #{tshow n}
     <th> #{formatVolume volume} m<sup>3
          |]


renderSessions :: [Session] -> Widget
renderSessions sessions = do
  let rowsW = mapM_ (render . sessionRows) sessions
  [whamlet|
<div.panel.panel-primary>
  <div.panel-heading> Heading
  <div.panel-body>
    ^{rowsW}
|]
-- ** Upload 
-- | Displays the main form to upload a boxtake spreadsheet.
-- It is also use to display the result of the validation (the 'pre' Widget)
renderBoxtakeSheet :: SavingMode -> Maybe UploadParam -> Int -> Handler() -> Widget -> Handler Html
renderBoxtakeSheet mode paramM status message pre = do
  let (action, button, btn) = case mode of
        Validate -> (WHBoxtakeValidateR, "validate" :: Text, "primary" :: Text)
        Save -> (WHBoxtakeSaveR, "save", "danger")

  (uploadFileFormW, encType) <- generateFormPost $ uploadForm mode paramM
  message
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
  <div.pre> ^{pre}
  <div.well>
    <form #upload-form role=form method=post action=@{WarehouseR action} enctype=#{encType}>
      ^{uploadFileFormW}
      <button type="submit" name="#{button}" .btn class="btn-#{btn}">#{button}
|]

processBoxtakeSheet :: SavingMode -> Handler Html
processBoxtakeSheet mode = do
  ((fileResp, postFileW), enctype) <- runFormPost (uploadForm mode Nothing)
  case fileResp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show (mode, a)
    FormSuccess param0@UploadParam{..} -> do
      skpM <- readUploadOrCacheUTF8 uEncoding uFileInfo uFileKey uFilePath
      case skpM of
        Nothing -> do
          let msg = setError "No file provided."
          renderBoxtakeSheet  Validate
                              (Just $ param0{uFileInfo=Nothing, uFileKey=Nothing, uFilePath=Nothing})
                              422
                              msg
                              (return ())
        Just (spreadsheet, key , path) -> do
          let paramWithKey = param0 {uFileInfo=Nothing, uFileKey=Just key, uFilePath=Just path}
          sessions <- parseScan spreadsheet
          renderParsingResult (renderBoxtakeSheet Validate (Just paramWithKey) 422 )
                              (processBoxtakeMove paramWithKey)
                              sessions
processBoxtakeMove :: UploadParam -> [Session] -> Handler Html
processBoxtakeMove param sessions = do
  defaultLayout $ renderSessions sessions


-- * DB Access
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
  
loadStocktakes :: [Entity Boxtake] -> Handler [(Entity Boxtake , [Entity Stocktake])]
loadStocktakes boxtakes = 
  -- slow version
  forM boxtakes $ \e@(Entity _ box) -> do
     stocktakes <- runDB $ selectList [StocktakeBarcode ==. boxtakeBarcode box] []
     return (e, stocktakes)
-- * Util
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

summary :: [Entity Boxtake] -> (Int, Double)
summary bs = (length bs, sum (map (boxtakeVolume . entityVal) bs))

-- | Regroup boxtakes according to rupture mode
rupture :: RuptureMode -> Int -> [Entity Boxtake] -> [(Text, [Entity Boxtake])]
rupture mode l boxtakes = let
  field = case mode of
    BarcodeRupture -> boxtakeBarcode
    LocationRupture -> boxtakeLocation
    DescriptionRupture -> fromMaybe "" . boxtakeDescription
  key = (take l) . field
  groups = Map.fromListWith (<>) [ (key b, [e]) | e@(Entity _ b) <- boxtakes ]
  in mapToList groups

  
