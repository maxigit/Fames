{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Boxtake
( getWHBoxtakeR
, postWHBoxtakeR
, getWHBoxtakeDetailR
, getWHBoxtakeValidateR
, postWHBoxtakeValidateR
, postWHBoxtakeSaveR
, getWHBoxtakePlannerR
, getWHBoxtakeAdjustmentR
, postWHBoxtakeAdjustmentR
, getWHBoxtakeAdjustmentForR
, boxSourceToCsv
, plannerSource
) where

import Import hiding(Planner)
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import Data.List(nub)
import qualified Data.Map.Strict as Map
import Text.Printf(printf)
import Handler.WH.Boxtake.Common
import Handler.WH.Boxtake.Upload
import Handler.WH.Boxtake.Adjustment
import Data.Conduit.List(sourceList)
import Database.Persist.Sql (fromSqlKey)
-- * Types
data RuptureMode = BarcodeRupture | LocationRupture | DescriptionRupture
  deriving (Eq, Read, Show, Enum, Bounded)

-- | Parameters for boxtake history,lookup,
data BoxtakeInquiryParam  = BoxtakeInquiryParam
  { pBarcode :: Maybe FilterExpression
  , pLocation :: Maybe FilterExpression
  , pDescription :: Maybe FilterExpression
  , pRuptureMode :: RuptureMode
  , pRuptureLength :: Maybe Int
  , pShowInactive :: Bool
  , pCompactView :: Bool
  }
defaultInquiryParam = BoxtakeInquiryParam Nothing Nothing Nothing LocationRupture Nothing  False True

-- | Validate or save spreadsheet.
data SavingMode = Validate | Save deriving (Eq, Read, Show)

-- | Parameters to upload box moves/scans.
data UploadParam = UploadParam
  { uFileInfo :: Maybe FileInfo
  , uFileKey :: Maybe DocumentHash
  , uFilePath :: Maybe Text
  , uEncoding :: Encoding
  , uWipeMode :: WipeMode
  }

-- defaultUploadParam = UploadParam Nothing Nothing Nothing UTF8 WipeShelves


-- * Requests
-- ** Boxtake history
{-# NOINLINE getWHBoxtakeR #-}
getWHBoxtakeR :: Handler Html
getWHBoxtakeR = do
  renderBoxtakes defaultInquiryParam

{-# NOINLINE postWHBoxtakeR #-}
postWHBoxtakeR :: Handler Html
postWHBoxtakeR = do
  ((resp, _), _) <- runFormPost (inquiryForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderBoxtakes param

-- | Display the detail of a box given a barcode.
{-# NOINLINE getWHBoxtakeDetailR #-}
getWHBoxtakeDetailR :: Text -> Handler Html
getWHBoxtakeDetailR barcode = do
  boxtakem <- runDB $ getBy  (UniqueBB barcode)
  stocktakes<- runDB $ selectList [StocktakeBarcode ==. barcode] [Desc StocktakeDate, Asc StocktakeIndex]
  operatorsMap <- allOperators
  defaultLayout =<< case boxtakem of
      Nothing -> setError (toHtml ("Can't find any box with barcode '" <> barcode <> "'")) >> return ""
      Just boxtake -> return $ renderBoxtakeDetail operatorsMap boxtake stocktakes
  
-- ** Upload
{-# NOINLINE getWHBoxtakeValidateR #-}
getWHBoxtakeValidateR :: Handler Html
getWHBoxtakeValidateR = renderBoxtakeSheet Validate Nothing 202 (return ()) (return ())

{-# NOINLINE postWHBoxtakeValidateR #-}
postWHBoxtakeValidateR :: Handler TypedContent
postWHBoxtakeValidateR = processBoxtakeSheet Validate

{-# NOINLINE postWHBoxtakeSaveR #-}
postWHBoxtakeSaveR :: Handler TypedContent
postWHBoxtakeSaveR = do
  actionM <- lookupPostParam "action"
  case actionM of
    Just "Planner" -> spreadSheetToCsv
    _ -> processBoxtakeSheet Save

-- * Planner
{-# NOINLINE getWHBoxtakePlannerR #-}
getWHBoxtakePlannerR :: Handler TypedContent
getWHBoxtakePlannerR = do
  let source = plannerSource
  renderPlannerCsv source
  
renderPlannerCsv boxSources = do
  today <- todayH
  let source = boxSourceToSection today boxSources
  setAttachment ("10-Planner-" <> fromStrict (tshow today) <> ".org")
  respondSourceDB "text/plain" (source .| mapC (toFlushBuilder))

-- plannerSource :: _ => Source m Text
plannerSource :: ConduitM () (Entity Boxtake) SqlHandler ()
plannerSource = selectSource [BoxtakeActive ==. True] [Asc BoxtakeLocation, Asc BoxtakeDescription]
  -- yield "Bay No,Style,QTY,Length,Width,Height,Orientations\n"
  -- boxes .| mapC toPlanner
  
toPlanner :: (Entity Boxtake) -> Text
toPlanner (Entity _ Boxtake{..}) = 
  boxtakeLocation
  <> "," <> (fromMaybe "" boxtakeDescription) <> (mconcat $ map ("#" <>) tags )
  <> ",1"
  <> "," <> tshow boxtakeLength
  <> "," <> tshow boxtakeWidth
  <> "," <> tshow boxtakeHeight
  <> "," -- orientation
  <> "\n"
  where tags= [ "barcode=" <> boxtakeBarcode 
              , "date=" <> tshow boxtakeDate
              , "location=" <> boxtakeLocation
              ] ::  [Text]

boxSourceToSection today boxSources = do
  yield ("* Stocktake from Planner  [" <> tshow today <> "]\n")
  yield (":STOCKTAKE:\n")
  boxSourceToCsv boxSources
  yield (":END:\n")

boxSourceToCsv :: Monad m => ConduitM i (Entity Boxtake) m () -> ConduitT i Text m ()
boxSourceToCsv boxSources = do
  yield ("Bay No,Style,QTY,Length,Width,Height,Orientations\n" :: Text)
  boxSources .| mapC toPlanner
  
spreadSheetToCsv :: Handler TypedContent
spreadSheetToCsv = processBoxtakeSheet' Save go
  where go _ _ (sessions, _) = do
          let boxSources = sourceList (concatMap sessionBoxesWithNewLocation sessions)
          renderPlannerCsv boxSources
        sessionBoxesWithNewLocation Session{..} = do -- []
                 row <- sessionRows
                 Just (Entity bId box) <- return $ rowBoxtake row -- skip Nothing
                 return $ Entity bId box {boxtakeLocation = sessionLocation}

-- * Adjustment
-- Boxtake Adjustment allows to validate or invalidate boxtake
-- depending on the actual stock

{-# NOINLINE getWHBoxtakeAdjustmentR #-}
getWHBoxtakeAdjustmentR :: Handler TypedContent
getWHBoxtakeAdjustmentR = do
  setWarning "Make sure orderXtra cache has been reset on FrontAccounting"
  flip renderBoxtakeAdjustments Nothing =<< defaultAdjustmentParamH

{-# NOINLINE postWHBoxtakeAdjustmentR #-}
postWHBoxtakeAdjustmentR :: Handler TypedContent
postWHBoxtakeAdjustmentR = do
  ((resp, _ ) , _) <- adjustmentForm <$> defaultAdjustmentParamH >>= runFormPost
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      actionM <- lookupPostParam "action"
      when (actionM == Just "Process") processBoxtakeAdjustment
      result <- displayBoxtakeAdjustments param
      renderBoxtakeAdjustments param (Just result)

-- | Narrow the search to the given style.
-- Used mainly as a target from link
{-# NOINLINE getWHBoxtakeAdjustmentForR #-}
getWHBoxtakeAdjustmentForR :: Text -> Bool -> Maybe Day -> Handler TypedContent
getWHBoxtakeAdjustmentForR style skipOk todaym = do
  param0 <- defaultAdjustmentParamH
  let param = param0 { aStyleFilter = Just (LikeFilter style)
                    , aShowDetails = True
                    , aSkipOk = skipOk
                    , aStyleSummary = False
                    , aDate = todaym
                    }
  renderBoxtakeAdjustments param =<< Just <$> displayBoxtakeAdjustments param

  

-- * Forms
inquiryForm :: Maybe BoxtakeInquiryParam -> _
inquiryForm param0 = renderBootstrap3 BootstrapBasicForm form
  where form = BoxtakeInquiryParam
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
             <*> areq (selectField optionsEnum ) "wipe mode " (fmap uWipeMode paramM <|> Just FullStylesAndShelves)
        form Validate = UploadParam
             <$> (Just <$> areq fileField "upload" (uFileInfo =<< paramM))
             <*> pure Nothing
             <*> pure Nothing
             <*> areq (selectField optionsEnum ) "encoding" (uEncoding <$> paramM <|> Just UTF8)
             <*> areq (selectField optionsEnum ) "wipe mode" (uWipeMode <$> paramM <|> Just FullStylesAndShelves)
            
-- adjustForm :: Maybe AdjustmentParam -> _ 
adjustmentForm param = renderBootstrap3 BootstrapBasicForm form where
  form = AdjustmentParam <$> aopt filterEField "style" (Just $ aStyleFilter param)
                         <*> pure (aLocation param)
                         <*> areq boolField "Skip OK" (Just $ aSkipOk param)
                         <*> areq boolField "Show Details" (Just $ aShowDetails param)
                         <*> areq boolField "Group Style" (Just $ aStyleSummary param)
                         <*> aopt dayField "Stock Valuation Date" (Just $ aDate param)

-- * Rendering
-- ** Boxtake history
renderBoxtakes :: BoxtakeInquiryParam -> Handler Html
renderBoxtakes param = do
  boxtakes <- loadBoxtakes param
  (formW, encType) <- generateFormPost $ inquiryForm (Just param)
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
<table#render-boxtake-table *{datatable} >
  <thead>
    <tr>
        <th> Barcode 
        <th> Description 
        <th> Dimensions
        <th> Volume
        <th> Location
        <th> Date 
        <th> Operator
        <th> Active
  <tbody>
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
<table *{"table-bordered" <>. datatable}>
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
<table *{"table-bordered" <>. datatable}>
  <tr>
     <th> Stock Id
     <th> Quantity
     <th> Date
     <th> Active
     <th> Operator
  $forall (Entity _ stocktake) <- stocktakes
    <tr>
      <td> #{stocktakeStockId stocktake}
      <td> #{tshow (stocktakeQuantity stocktake)}
      <td> #{tshow (stocktakeDate stocktake)}
      <td> #{displayActive (stocktakeActive stocktake)}
      $with opId <- stocktakeOperator stocktake
        <td> #{maybe ("#" <> tshow (fromSqlKey opId)) operatorNickname (lookup opId opMap)}
          |]

renderSummary :: BoxtakeInquiryParam -> [Entity Boxtake] -> Widget
renderSummary param boxtakes =  do
  let (n, volume) = summary boxtakes
      groups = case pRuptureLength  param of
                Just l | l /= 0 -> rupture (pRuptureMode param) l boxtakes
                _ -> []
  [whamlet|
<table#boxtake-summary *{datatable}>
  <thead>
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


renderSessions :: _ -> [Session] -> [StyleMissing] -> Widget
renderSessions renderUrl sessions missingStyles = do
  let allRows = concatMap sessionRows sessions
      rowCount = length allRows
      volume = sum (map rowVolume allRows)
      missingRows = concatMap sessionMissings sessions <> concatMap missingBoxes missingStyles
      missing = length missingRows
      volumeMissing = sum (map (boxVolume . entityVal) missingRows)
      founds = filter rowIsFound allRows
      volumeFound = sum (map rowVolume founds)
      ids = ["session-" <> tshow i | i <- [1..]]
      mainW = mapM_ (uncurry $ renderSession renderUrl) (zip ids sessions)
  [whamlet|
<div.panel.panel-info>
  <div.panel-heading><h2>Summary
  <table.table.table-striped.table-hover>
    <tr>
      <th>
      <th> Count
      <th> Volume
    <tr>
      <td> Scanned
      <td> #{rowCount}
      <td> #{formatDouble volume} m<sup>3
    <tr>
      <td> Missing
      <td> #{missing}
      <td> #{formatDouble volumeMissing} m<sup>3
    <tr>
      <td> Found
      <td> #{length founds}
      <td> #{formatDouble volumeFound} m<sup>3
    <tr>
      <td>
      <td>
      <td.panel-heading data-toggle="collapse" data-target="#main .collapse">
        <span.badge.badge-info>Collapse/Expand All
^{mapM_ (renderMissingStylesPanel renderUrl) missingStyles}
^{mainW}
          |]
  
renderMissingStylesPanel :: _ -> StyleMissing -> Widget
renderMissingStylesPanel renderUrl StyleMissing{..} = do
  let rowCount = length missingBoxes
      volume = sum (map (boxVolume . entityVal) missingBoxes)
      collapseId = "missing-" <> missingStyle
  [whamlet|
<div.panel.panel-danger data-toggle="collapse" data-target="##{collapseId}">
  <div.panel-heading>
    <table style="width:100%;"><tr>
      <td> missing #{missingStyle} from non-scanned locations
      <td> #{tshow rowCount} boxes
      <td> #{formatDouble volume} m<sup>3
  <table.table.table-hover.collapse id="#{collapseId}">
    $forall missing <- missingBoxes
      ^{renderMissingBoxRow renderUrl missing}
|]

  
-- or wipe full style ?
-- or both ?
-- - add link to boxtake
-- - add like to location boxtake

renderSession :: _ -> Text -> Session -> Widget
renderSession renderUrl sessionId Session{..} = do
  let rowsW = renderRows renderUrl sessionRows
      hasFound =  any rowIsFound sessionRows
      class_ = case (sessionRows, sessionMissings, hasFound) of
                 (_, _, True) -> "info" :: Text
                 ([], [], _) -> "success" :: Text
                 ([], _, _)-> "warning"
                 (_, [], _) -> "success" :: Text
                 (_,_:_, _) -> "danger" -- some missing
      volume = sum (map rowVolume sessionRows)
  toWidget [cassius|
table.collapse.in
  display: table
|]
  [whamlet|
<div.panel class="panel-#{class_}" data-toggle="collapse" data-target="##{sessionId}">
  <div.panel-heading>
    <table style="width:100%;"><tr>
      <td> #{sessionLocation}
      <td> #{tshow $ length sessionRows} Boxes
      <td> #{tshow $ length sessionMissings } Missings
      <td> #{formatDouble volume} m<sup>3
      <td><span style="float:right">  #{operatorNickname (entityVal sessionOperator)} - #{tshow sessionDate}
  <table.table.table-hover.collapse id="#{sessionId}">
    ^{rowsW}
    $forall missing <- sessionMissings
      ^{renderMissingBoxRow renderUrl missing}
|]

renderMissingBoxRow :: _ -> Entity Boxtake -> Widget
renderMissingBoxRow renderUrl (Entity _ missing) = [whamlet|
      <tr.bg-danger>
        <td> #{renderBarcode renderUrl (boxtakeBarcode missing)}
        <td> #{fromMaybe "" (boxtakeDescription missing)}
        <td.text-danger> #{boxtakeLocation missing}
        <td> ∅
        <td> #{boxtakeActive missing}
        <td> #{tshow (boxtakeDate missing)}
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
      $if (mode == Save) 
        <button type="submit" name="action" value="Planner" .btn.btn-info>Export
|]

processBoxtakeSheet' :: SavingMode
                     -> (SavingMode -> UploadParam -> ([Session], [StyleMissing]) -> Handler TypedContent)
                     -> Handler TypedContent
processBoxtakeSheet' mode onSuccess = do
  ((fileResp, __postFileW), __enctype) <- runFormPost (uploadForm mode Nothing)
  case fileResp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show (mode, a)
    FormSuccess param0@UploadParam{..} -> do
      skpM <- readUploadOrCacheUTF8 uEncoding uFileInfo uFileKey uFilePath
      case skpM of
        Nothing -> do
          let msg = setError "No file provided."
          selectRep . provideRep $ renderBoxtakeSheet  Validate
                              (Just $ param0{uFileInfo=Nothing, uFileKey=Nothing, uFilePath=Nothing})
                              422
                              msg
                              (return ())
        Just (spreadsheet, key , path) -> do
          let paramWithKey = param0 {uFileKey=Just key, uFilePath=Just path}
          sessions <- parseScan uWipeMode spreadsheet
          documentKey'msgM <- runDB $ loadAndCheckDocumentKey key
          _ <- forM documentKey'msgM $  \(entity,  msg) -> do
              setWarning msg
              return entity

          let convert f w = toTypedContent <$> (f w)
          renderParsingResult (convert .  renderBoxtakeSheet Validate (Just paramWithKey) 422 )
                              (onSuccess mode paramWithKey)
                              sessions

processBoxtakeSheet :: SavingMode -> Handler TypedContent
processBoxtakeSheet mode0 = processBoxtakeSheet' mode0 go where
  go mode param sessions = fmap toTypedContent $ processBoxtakeMove mode param sessions

processBoxtakeMove :: SavingMode
                   -> UploadParam
                   -> ([Session], [StyleMissing])
                   -> Handler Html
processBoxtakeMove Validate param (sessions, styleMissings) = do
  renderUrl <- getUrlRenderParams
  sessionW <- do
    return $ renderSessions renderUrl sessions styleMissings
  renderBoxtakeSheet Save (Just param) (fromEnum ok200) (setSuccess "Spreadsheet valid") sessionW

processBoxtakeMove Save _ (sessions, styleMissings) = do
  runDB $ do
        today <- todayH
        let maxDate = fromMaybe today $ maximumMay (map sessionDate sessions)
              
        mapM_ saveFromSession sessions
        mapM_ (deactivateBoxtake maxDate) (concatMap missingBoxes styleMissings)
  renderBoxtakeSheet Validate Nothing (fromEnum created201) (setSuccess "Boxtake uploaded successfully") (return ())

-- ** Adjustment
          
renderBoxtakeAdjustments :: AdjustmentParam -> Maybe Widget -> Handler TypedContent
renderBoxtakeAdjustments param resultM = do
  (formW, encType) <- generateFormPost $ adjustmentForm param
  toTypedContent <$> defaultLayout ( do
     adjustmentCSS
     adjustmentJS
     [whamlet|
<form #box-adjustment role=form method=POST action=@{WarehouseR WHBoxtakeAdjustmentR} enctype="#{encType}">
  <div.well>
    ^{formW}
    <button type="submit" name="Search" .btn.btn-primary> Search
  $maybe result <- resultM
    <div.panel.panel-info>
      <div.panel-heading><h2> Adjustments
      <div.panel-body> ^{result}
  $if aShowDetails param
    <button type="submit" name="action" value="Process" .btn.btn-danger> Activate/Deactivate
                        |]
                                       )
  

-- * DB Access
loadBoxtakes :: BoxtakeInquiryParam -> Handler [Entity Boxtake]
loadBoxtakes param = do
  let filter_ = filterE id BoxtakeBarcode (pBarcode param)
              <> filterE id BoxtakeLocation (pLocation param)
              <> filterE Just BoxtakeDescription (pDescription param)
  opts <- case filter_ of
        [] -> do -- no filter, we want the last ones
                setWarning "Only the last 50 boxtakes are being displayed. Please refine the filter to get a full selection"
                return [Desc BoxtakeId, LimitTo 50]
        _ -> return $ -- filter, we use the filter to sort as well
          catMaybes [ pBarcode param <&> (const $ Asc BoxtakeBarcode)
                    , pLocation param <&> (const $ Asc BoxtakeLocation)
                    , pDescription param <&> (const $ Asc BoxtakeDescription)
                    ] <> [Asc BoxtakeDescription]
  let active = if pShowInactive param
               then []
               else [BoxtakeActive ==. True]
  runDB $ selectList (active <> filter_) opts
  
-- * Util

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
