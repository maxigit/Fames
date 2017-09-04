{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- TODO remove:
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Handler.WH.PackingList
( getWHPackingListR
, postWHPackingListR
, postWHPackingListEditR
, getWHPackingListViewR
, postWHPackingListEditDetailsR
, postWHPackingListDeliverR
, contentToMarks
-- For test
, deliverCart
, parseDeliverList
, EditMode(..)
-- , postWHPackingListViewR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import Handler.Util(generateLabelsResponse)
import qualified Data.Csv as Csv
import Data.List (transpose)
import qualified Data.List as List
import Database.Persist.MySQL
import qualified Data.Map as Map
import Data.Time (diffDays, addGregorianMonthsClip)
import Handler.WH.Barcode
import WH.Barcode
import Handler.WH.Legacy.Box hiding(main)
import Text.Printf(printf)
import Data.Conduit.List (consume)
import Data.Text(splitOn)
import Text.Blaze.Html(Markup, ToMarkup)

data Mode = Validate | Save deriving (Eq, Read, Show)
data EditMode = Replace | Insert | Delete deriving (Eq, Read, Show, Enum)

-- | Type of file
data Format = PartialFirst deriving (Eq, Read, Show)
data UploadParam = UploadParam
  { orderRef :: Text -- ^ original order reference
  , invoiceRef :: Text -- ^ name of the file to upload
  , container :: Maybe Text
  , vessel :: Maybe Text
  , departure :: Maybe Day
  , arriving :: Maybe Day
  , comment :: Maybe Textarea -- ^ any comment
  , spreadsheet :: Textarea -- ^ the actual spreadsheet to upload/process
  } deriving (Eq, Read, Show)

uploadForm :: Maybe UploadParam -> Markup ->  _ (FormResult UploadParam, Widget)
uploadForm param = renderBootstrap3 BootstrapBasicForm form
  where form = UploadParam
            <$> (areq textField "order ref" (fmap orderRef param))
            <*> (areq textField "invoice ref" (fmap invoiceRef param ))
            <*> (aopt textField "container" (fmap container param ))
            <*> (aopt textField "vessel" (fmap vessel param ))
            <*> (aopt dayField "departure" (fmap departure param ))
            <*> (aopt dayField "arriving" (fmap arriving param ))
            <*> (aopt textareaField "comment" (fmap comment param) )
            <*> (areq textareaField "spreadsheet" (fmap spreadsheet param) )

getWHPackingListR :: Handler Html
getWHPackingListR = renderWHPackingList Validate Nothing ok200 (setInfo "Enter a packing list") (return ())

renderWHPackingList :: Mode -> (Maybe UploadParam) -> Status -> Handler () ->  Widget -> Handler Html
renderWHPackingList mode param status message pre = do
  let (btn, plViewH) = case mode of
        Validate -> ("primary" :: Text, Just viewPLLists)
        Save -> ("danger", Nothing)

  plViewM <- forM plViewH id


  (form, encType) <- generateFormPost (uploadForm param)
  message
  sendResponseStatus status =<< defaultLayout
--     toWidget [cassius|
-- tr.table-top-border td
--   border-top: black thin solid black
--                      |]

    [whamlet|
    $maybe plView <- plViewM
      <div.well>
        ^{plView}
    <div.well> ^{pre}
    <div.panel.panel-info>
      <div.panel-heading data-toggle="collapse" data-target=".pl-upload-colnames">
        Make sure the header is present and contains the following columns.
      <table.pl-upload-colnames.table.table-bordered.table-hover.collapse.in>
        <tr>
          $forall header <- (map fst columnNames) 
            <th>#{header}
        $forall values <- transposeM (map snd columnNames)
          <tr>
            $forall value <- values
              <td>#{fromMaybe "" value}
    <div.well>
      <form #upload-form role=form method=post action=@{WarehouseR WHPackingListR} enctype=#{encType}>
        ^{form}
        <button type="submit" name="action" value="#{tshow mode}" class="btn btn-#{btn}">#{tshow mode}
                        |]

viewPLLists :: Handler Widget
viewPLLists = do
  entities <- runDB $ selectList [] []
  today <- utctDay <$> liftIO getCurrentTime
  let pls = map entityVal entities
      minDate = Just $ List.minimum (today : catMaybes (map (packingListDeparture) pls))
      maxDate = Just $ List.maximum (today : catMaybes (map (packingListArriving) pls))

  return [whamlet|
<table.table.table-bordered>
  <tr>
    <th> Id
    <th> Invoice Ref
    <th> Vessel
    <th> Container
    <th> Left
    <th> Departure
    <th.col-xs-4.col-md-6> 
    <th> Arriving
  $forall (Entity key pl) <- entities
    <tr>
      <td> <a href="@{WarehouseR (WHPackingListViewR (unSqlBackendKey $ unPackingListKey key) Nothing)}">
       ##{tshow $ unSqlBackendKey $ unPackingListKey key}
      <td> #{packingListInvoiceRef pl}
      <td> #{fromMaybe "" $ packingListVessel pl}
      <td> #{fromMaybe "" $ packingListContainer pl}
      <td> #{packingListBoxesToDeliver_d pl}
      <td> #{maybe "" tshow (packingListDeparture pl) }
      <td> ^{timeProgress minDate maxDate today pl}
      <td> #{maybe "" tshow (packingListArriving pl) }
          |]

transposeM :: [[a]] -> [[Maybe a]]
transposeM lines_  =
  let maxLength = List.maximum (map List.length lines_)
      linesM = map (take maxLength . (++ repeat Nothing) . map Just) lines_
  in transpose linesM



postWHPackingListR :: Handler Html
postWHPackingListR = do
  action <- lookupPostParam "action"
  ((resp, view), _encType) <- runFormPost (uploadForm Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess param ->  do
      processUpload (fromMaybe Validate (readMay =<< action)) param


processUpload :: Mode -> UploadParam -> Handler Html
processUpload mode param = do
  let bytes = encodeUtf8 . unTextarea $ spreadsheet param

      renderBoxGroup :: PLBoxGroup -> Widget
      renderBoxGroup (partials, full) = do
        topBorder
        [whamlet|
<tr.table-top-border> ^{renderRow full}
  $forall partial <- partials
    <tr.text-muted> ^{renderRow partial}
                                         |]
      renderSection :: (PLOrderRef, [PLBoxGroup]) -> Widget
      renderSection (orderRef, groups) = do
        let orderRefId = validValue $ plStyle orderRef
        [whamlet|
<div.panel.panel-primary>
  <div.panel-heading data-toggle="collapse" data-target="##{tshow orderRefId}-pl"> #{orderRefId}
  <table.table.table-hover.collapse.in id="#{tshow orderRefId}-pl">
    ^{renderHeaderRow}
    $forall group <- groups
      ^{renderBoxGroup group}
      |]

      key = computeDocumentKey bytes

  documentKey <- runDB $ (getBy (UniqueSK key))
      -- TODO used in Stocktake factorize
  _ <- forM documentKey $ \(Entity _ doc) -> do
    uploader <- runDB $ get (documentKeyUserId doc)
    let msg = [shamlet|Document has already been uploaded 
$maybe u <- uploader
  as "#{documentKeyName doc}"
  by #{userIdent u}
  on the #{tshow $ documentKeyProcessedAt doc}.
|]
    case mode of
      Validate -> setWarning msg >> return ""
      Save -> renderWHPackingList Save (Just param) expectationFailed417 (setError msg) (return ()) -- should exit

  let onSuccess Save rows = do
        __saved <-savePLFromRows key param rows
        renderWHPackingList Validate Nothing created201 (setSuccess "Packing list uploaded successfully") (return ())

      onSuccess Validate groups = do

        renderWHPackingList Save (Just param)
                            ok200 (setSuccess "Packing list valid")
                            [whamlet|
<div.panel-group>
  $forall section <- groups
    ^{renderSection section}
                                    |]

  renderParsingResult (renderWHPackingList mode (Just param) badRequest400) 
                      (onSuccess mode)
                      (parsePackingList (orderRef param) bytes)

  -- | Update denormalized fields, ie boxesToDeliver_d
updateDenorm :: (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m, PersistQueryRead backend) => Key PackingList -> ReaderT backend m ()
updateDenorm plKey = do
  boxesToDeliver <- count [ PackingListDetailPackingList ==. plKey
                          , PackingListDetailDelivered ==. False
                          ]

  update plKey [PackingListBoxesToDeliver_d =. boxesToDeliver ]

updatePackingListDetails :: EditMode -> Int64 -> Textarea -> HandlerT App IO TypedContent
updatePackingListDetails mode key cart = do
  let bytes = encodeUtf8 . unTextarea $ cart
      plKey = PackingListKey (SqlBackendKey key)
      onSuccess sections = do
        runDB $ do case mode of
                        Replace -> replacePLDetails plKey sections bytes
                        Insert -> insertPLDetails plKey sections
                        Delete -> deletePLDetails plKey sections
                   updateDenorm plKey
        setSuccess "Packing list updated."
        viewPackingList Details key (return ())

  -- we the first detail reference use if possible 
  firstDetail <- runDB $ selectFirst [PackingListDetailPackingList ==. plKey]
                              [Asc PackingListDetailId]

  let orderRef = maybe "" (packingListDetailReference . entityVal) firstDetail


  renderParsingResult (\msg pre -> do msg >>  viewPackingList EditDetails key pre)
                      onSuccess 
                      (parsePackingList orderRef bytes)

updatePackingList :: Int64 -> UploadParam -> HandlerT App IO TypedContent
updatePackingList key param = do
  let plKey = PackingListKey (SqlBackendKey key)
      plUpdates = 
                [ (PackingListInvoiceRef =. invoiceRef param)
                , (PackingListContainer =. container param)
                , (PackingListVessel =. vessel param)
                , (PackingListDeparture =. departure param)
                , (PackingListArriving =. arriving param)
                ]
      docUpdates = catMaybes
                 [ (DocumentKeyComment =.) . unTextarea <$> comment param ]
  runDB $ do
    unless (null plUpdates) $ update plKey plUpdates
    pl <- getJust plKey
    let docKey = packingListDocumentKey pl
    -- TODO remove 
    __doc <- get docKey
    unless (null docUpdates) $ update docKey docUpdates

    setSuccess "Packing list updated successfully"

  viewPackingList Details key (return ())


-- | Marks all details in the cart as delivered.
-- unmarks the ones which are not.
deliverPackingList :: Int64 -> DeliveryParam -> Handler TypedContent
deliverPackingList key param = do
  let plKey =  PackingListKey (SqlBackendKey key)
      onSuccess (delivers, undelivers) = do
          runDB $ do
            -- prefill information
            (pl) <- getJust plKey
            let docKey = packingListDocumentKey pl
            -- deliver
            forM_ delivers $ \k -> update k [PackingListDetailDelivered =. True]
            -- create associated boxtakes
            details <- selectList [PackingListDetailId <-. delivers] []
            let boxtakes = map (detailToBoxtake param docKey . entityVal) details
            insertMany_ boxtakes
            -- undeliver
            forM_ undelivers $ \k -> update k [PackingListDetailDelivered =. False]
            undetails <- selectList [PackingListDetailId <-. undelivers] []
            let unbarcodes = map (packingListDetailBarcode . entityVal) undetails
            mapM_ (deleteBy <$> UniqueBB) unbarcodes

            updateDenorm plKey
            setSuccess "Packing List Delivered"
          viewPackingList Details key (return ())

  renderParsingResult (\msg pre -> do msg >> viewPackingList Details key pre)
                      onSuccess
                      (parseDeliverList (dpCart param))


detailToBoxtake :: DeliveryParam -> DocumentKeyId -> PackingListDetail -> Boxtake
detailToBoxtake param docKey detail = Boxtake
  (Just $ packingListDetailStyle detail)
  reference
  (packingListDetailLength detail)
  (packingListDetailWidth detail)
  (packingListDetailHeight detail)
  (packingListDetailBarcode detail)
  (dpLocation param)
  (dpDate param)
  True
  (dpOperator param)
  docKey
  []
  where reference =  intercalate "-" ([ packingListDetailReference
                     , packingListDetailStyle
                     , tshow . packingListDetailBoxNumber
                     ] <*> [detail]
                                     )
  



-- ** View


getWHPackingListViewR :: Int64 -> Maybe PLViewMode -> Handler TypedContent
getWHPackingListViewR key mode = viewPackingList (fromMaybe Details mode) key (return ())

postWHPackingListEditDetailsR :: Int64 -> Handler TypedContent
postWHPackingListEditDetailsR key = do
  actionM <- lookupPostParam "action"
  ((resp, view), __encType)  <- runFormPost (editDetailsForm Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess cart -> do
      case actionM >>= readMay of
        Nothing -> error "Action missing"
        Just action -> updatePackingListDetails action key cart

postWHPackingListEditR :: Int64 -> Handler TypedContent
postWHPackingListEditR key = do
  ((resp, view), __encType)  <- runFormPost (editForm Nothing Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess param -> updatePackingList key param

postWHPackingListDeliverR :: Int64 -> Handler TypedContent
postWHPackingListDeliverR key = do
  ((resp, view), __encType) <- runFormPost (deliverDetailsForm Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess param -> deliverPackingList key param

viewPackingList :: PLViewMode -> Int64 ->  Widget -> Handler TypedContent
viewPackingList mode key pre = do
  let plKey = PackingListKey (SqlBackendKey key)
  (plM, docKeyM, entities) <- runDB $ do
      plM <- get plKey
      entities <- selectList [PackingListDetailPackingList ==. plKey] [Asc PackingListDetailId]
      docKey <- do
        case plM of
          Nothing -> return Nothing
          Just pl -> get (packingListDocumentKey pl)
      return (plM, docKey, entities)

  today <- utctDay <$> liftIO getCurrentTime


  case (plM, docKeyM) of
    (Just pl, Just docKey) -> do

      let header = [ ("Invoice Ref" , packingListInvoiceRef pl )
                  , ("To deliver", tshow (packingListBoxesToDeliver_d pl))
                  ] :: [(Text, Text)]
      let fieldsS = [ [ ("Container" , packingListContainer pl )
                      , ("Vessel", packingListVessel pl)
                      ]
                    , [ -- ("Comment", Just $ documentKeyComment docKey)
                      ("user", Just . tshow . unSqlBackendKey . unUserKey $ documentKeyUserId docKey)
                      , ("processed", Just . tshow $ documentKeyProcessedAt docKey)
                      ]
                    , [ ("DEPARTURE", tshow <$> packingListDeparture pl)
                      , ("ARRIVING", tshow <$> packingListArriving pl)
                      ]
                    ] :: [[(Text, Maybe Text)]]
      let panelClass delivered | delivered == 0 = "success" :: Text
                              | delivered == length entities = "info"
                              | otherwise = "danger"
          navClass nav | nav == mode = "active" :: Text
                       | otherwise = ""

          corridors :: [(Double, Double, Double)] -- TODO extract from config
          corridors = [(8, 1.9, 3 ), (5, 1.5, 3), (4, 1.2, 3)]

      if mode == Stickers
         then generateStickers pl entities
         else do
           entitiesWidget <-case mode of
             EditDetails -> renderEditDetails Nothing key entities
             Edit -> renderEdit key pl docKey
             Deliver -> renderDeliver Nothing key entities
             _ -> return . toWidget $ (case mode of
                                 Details -> renderDetails
                                 Textcart -> renderTextcart
                                 StickerCsv -> renderStickers today
                                 Chalk -> renderChalk corridors
                                 Planner -> renderPlanner
                                 PlannerColourless -> renderPlannerColourless
                                 Stickers ->  error "Shoudn't happen"
                                 EditDetails ->  error "Shoudn't happen"
                                 Edit ->  error "Shoudn't happen"
                                 Deliver ->  error "Shoudn't happen"
                           ) pl entities
           selectRep $ provideRep $ defaultLayout $ do
              [whamlet|
          <div.panel class="panel-#{panelClass (packingListBoxesToDeliver_d pl)}">
            <div.panel-heading>
                <p>
                $forall (field, value) <- header
                     <b>#{field}
                     #{value}
            <div.panel-body>
              <div.pl-time.col-xs-12>
                ^{timeProgress Nothing Nothing today pl}


              $forall fields <- fieldsS
                <p>
                  $forall (field, value) <- fields
                      <b>#{field}
                      #{fromMaybe "" value}

            <p>#{documentKeyComment docKey}
                |]
              [whamlet|
                      ^{pre}
          <ul.nav.nav-tabs>
            $forall nav <-[Details, Edit, EditDetails, Textcart,Stickers, StickerCsv, Chalk, Planner, PlannerColourless,Deliver]
              <li class="#{navClass nav}">
                <a href="@{WarehouseR (WHPackingListViewR key (Just nav)) }">#{tshow nav}
          ^{entitiesWidget}
                |]
    _ -> notFound

data DeliveryParam = DeliveryParam
  { dpCart :: !Text
  , dpOperator :: !OperatorId
  , dpDate :: !Day
  , dpLocation :: !Text
  }

deliverDetailsForm :: Maybe DeliveryParam -> _ -> _ (FormResult DeliveryParam, Widget)
deliverDetailsForm defParam = renderBootstrap3 BootstrapBasicForm form
  where form = DeliveryParam
                <$> unTextarea <$> (areq textareaField "cart" ((Textarea . dpCart) <$> defParam))
                <*> areq (selectField operators)  "operator" (dpOperator <$> defParam)
                <*> areq dayField "date" (dpDate <$> defParam)
                <*> areq textField "location" (dpLocation <$> defParam)
        operators = optionsPersistKey [OperatorActive ==. True] [Asc OperatorNickname] operatorNickname

deliverCart :: [Entity PackingListDetail] -> Text
deliverCart details = let
  lines_ = map toLine details
  prefix :: PackingListDetail -> Text
  prefix d = if packingListDetailDelivered d
             then "-- -" -- '--' :comment, '-' : ready to undeliver
             else ""

  toLine (Entity key detail) = [ prefix detail <> (tshow . unSqlBackendKey . unPackingListDetailKey $  key)
                               , packingListDetailStyle detail
                               , intercalate " " $ [ var <> "x" <> tshow qty
                                                   | (var, qty) <- Map.toList $ packingListDetailContent detail 
                                                   ]
                               ]
  in unlines (map (intercalate ",") lines_)

renderDeliver :: Maybe Text -> Int64 -> [Entity PackingListDetail] -> Handler Widget
renderDeliver defCart key details = do
  today <- utctDay <$> lift getCurrentTime
  location <- appFADefaultLocation . appSettings <$> getYesod
  operator <- runDB $ selectKeysList [] [Asc OperatorId, LimitTo 1]
  let opId = case operator of
                [] -> do
                    error "The operator table is empty. Please contact your administrator."
                [opId0] -> opId0
                _ -> error "Shoudn't happened, ^limit 1"
    
  let cart = fromMaybe (deliverCart details) defCart
      param = DeliveryParam cart opId today location

     
  (form, encType) <- generateFormPost (deliverDetailsForm (Just param))
  return [whamlet|
<div>
  <form #deliver-details role=form method=post action=@{WarehouseR $ WHPackingListDeliverR key} enctype=#{encType}>
    ^{form}
    <button type="submit" name="action" value="deliver" class="btn btn-default">Deliver
|]

renderDetails :: PersistEntity a => t -> [Entity a] -> Html
renderDetails _ entities = entitiesToTable getDBName entities

renderTextcart :: t -> [Entity PackingListDetail] -> Markup
renderTextcart _ entities =
  let skus = Map.toList $ Map.fromListWith (+) [ (packingListDetailStyle d <> "-" <> var, qty)
                                  | (Entity _ d) <- entities
                                  , (var, qty) <- Map.toList $ (packingListDetailContent d)
                                  ]
  in  [shamlet|
$forall (sku, qty) <- skus
  <div> #{sku},#{qty}
|]

renderStickers :: Day -> PackingList -> [Entity PackingListDetail] -> Html
renderStickers today pl entities = 
  -- we need a monad to use the conduit. Let's use Maybe ...
  let Just csv = stickerSource today pl entities $$ consume
  in [shamlet|
<p>
  $forall row <- csv
    <div>#{row}
|]

-- Transforms a serie of colour quantites, to box marks ie
-- colour name and circles for every 6.
-- see specs for explanation
detailToStickerMarks :: PackingListDetail -> [Text] -- 16 fields
detailToStickerMarks detail = let
  marks = contentToMarks . Map.toList $ packingListDetailContent detail
  in take 16 $ marks ++ (repeat "")

contentToMarks :: [(Text, Int)] -> [Text]
contentToMarks unsorted =  let
  sorted = sort unsorted
  groupBy3 [] = []
  groupBy3 [x] = [[x,"",""]]
  groupBy3 [x,y] = [[x,y,""]]
  groupBy3 (x:y:z:xs) = [x,y,z]:groupBy3 xs
  go (col, 1) = [col, "∅", "", ""]
  go (col, quantity) | quantity >= 50 = [col, "◇", "", ""]
  go (col, quantity) = let
    nbOfCircles = (quantity + 5) `div` 6
    circless = groupBy3 (replicate nbOfCircles "○")
    in concat $ zipWith (:) (col:repeat "") circless

  in concatMap go sorted

stickerSource ::
  Monad m =>
  Day
  -> PackingList
  -> [Entity PackingListDetail]
  -> ConduitM i Text m ()
stickerSource today pl entities = do
  let sorted = sortBy (comparing cmp) entities
      cmp (Entity _ detail) = (packingListDetailStyle detail, Down (packingListDetailContent detail, packingListDetailBoxNumber detail) )
  yield "style,delivery_date,reference,number,barcode,a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4\n"
  yieldMany [ packingListDetailStyle detail
            <> "," <> (tshow $ fromMaybe today (packingListArriving pl) )
            <> "," <> (packingListDetailReference detail )
            <> "," <> (tshow $ packingListDetailBoxNumber detail )
            <> "," <> (packingListDetailBarcode detail)
            <> "," <> (intercalate "," (detailToStickerMarks detail))
            <> "\n"
            | (Entity _ detail) <- sorted
            ]

generateStickers :: PackingList -> [Entity PackingListDetail] -> Handler TypedContent
generateStickers pl details = do
  today <- utctDay <$> liftIO getCurrentTime
  generateLabelsResponse ("label" <> ( maybe "" ("-" <>) (packingListContainer pl)) )
                         "/config/delivery-stickers.glabels"
                         (stickerSource today pl details)

renderChalk  :: [(Double, Double, Double)] -> PackingList -> [Entity PackingListDetail] -> Html
renderChalk _ _ details = let
  -- convert details into Box.box
  toBox (Entity _ PackingListDetail{..}) = Box ( Dimension packingListDetailLength
                                                           packingListDetailWidth
                                                           packingListDetailHeight
                                               )
                                               (unpack packingListDetailStyle)
                                               1
                                               1
  --
  boxes = map toBox details
  --
  slices = findSlices boxes
  --
  showf = (\x -> x :: String) . printf "%5.2f"
  --
  in [shamlet|
<table.table.table-striped>
  <tr>
    <th> Style
    <th> Number of Boxes
    <th> Total Width (cm)
    <th> Position
    <th> Depth
  $forall (st, n, nh, w, nd,d, cw ) <- slices
    <tr>
      <td> #{st}
      <td> #{tshow n} x #{tshow nh} (up)
      <td> #{showf w}
      <td> #{showf cw} - #{ showf (cw + w)}
      <td> #{tshow nd} (#{showf d})
|]

-- | CSV compatible with WarehousePlanner
renderPlanner :: PackingList -> [Entity PackingListDetail] -> Html
renderPlanner _ details = let
  groups = Map.fromListWith (+) [ ( ( style detail
                                   , packingListDetailLength
                                   , packingListDetailWidth
                                   , packingListDetailHeight
                                   )
                                 , (1 :: Int)
                                 )
                               | (Entity _ detail@PackingListDetail{..}) <- details
                               ]
  style PackingListDetail{..} = packingListDetailStyle <> (content $ Map.toList packingListDetailContent )
  content [] = ""
  content ((col,__qty):cs) = "-" <> col <> if null cs then "" else "*"
  in [shamlet|
style,quantity,l,w,h
$forall ((style, l, w, h),qty) <- Map.toList groups
    <br>
    #{style}
    , #{tshow qty}
    , #{l} 
    , #{w}
    , #{h}
|]

-- | CSV compatible with warehousePlanner. Identical to {renderPlanner}
-- but remove colour information so that it's easier to know actuall how many boxes
-- are coming for a given style.
renderPlannerColourless :: PackingList -> [Entity PackingListDetail] -> Html
renderPlannerColourless pl details = renderPlanner pl (map removeColour details) where
  removeColour (Entity key detail) = Entity  key detail {packingListDetailContent = Map.fromList []}



editDetailsForm :: Maybe Text -> Markup -> _ (FormResult Textarea, Widget)
editDetailsForm defCart  = renderBootstrap3 BootstrapBasicForm form
  where form = areq textareaField "cart" (Textarea <$> defCart)

detailsCart :: [Entity PackingListDetail] -> Text
  -- The order reference is not part of the PL row
  -- but is specified as a special row (order ref)
  -- we need to generate those rows


detailsCart details = let
  header = (map (pack.fst) columnNames)
  lines_ = concat . snd $ List.mapAccumL toLines "" details
  toLines lastRef (Entity _ detail) =
    let content = Map.toList (packingListDetailContent detail)
        tqty = sum (map snd content)
    -- in the case of many colours in the same boxes
    -- we only want the last line to carry the box information
    -- to do so, we just process the content in reverse order
    -- and reverse the result
        reference = packingListDetailReference detail
        orderRow = if reference == lastRef
                      then []
                      else [take (List.length columnNames) (reference : repeat "")]

    in (reference, orderRow ++ reverse [
      [ packingListDetailStyle detail
      , var
      , tshow qty
      , h $ tshow $ packingListDetailBoxNumber detail
      , h $ tshow $ packingListDetailBoxNumber detail
      , h $ tshow (1 :: Int)
      , h $ tshow tqty
      , h $ tshow tqty
      , h $ tshow $ packingListDetailLength detail
      , h $ tshow $ packingListDetailWidth detail
      , h $ tshow $ packingListDetailHeight detail
      , h $ tshow volume
      , h $ tshow volume
      , h $ tshow weight
      , h $ tshow weight
      ]

    | ((var, qty), main) <- zip (reverse content) (True : repeat False)
    , let h val = if main then val else ""
          weight = packingListDetailWeight detail
          volume = (product $ [packingListDetailLength
                             ,packingListDetailWidth
                             ,packingListDetailHeight
                             ] <*> [detail]
                   ) / 1000000
    ])


  in unlines (map (intercalate ",") (header:lines_))


renderEditDetails :: Maybe Text -> Int64 -> [Entity PackingListDetail] -> Handler Widget
renderEditDetails defCart key details = do
  -- we only display undelivered items
  let cart = defCart <|> Just ( detailsCart
                              $ filter (not . packingListDetailDelivered . entityVal) details
                              )
  (form, encType) <- generateFormPost (editDetailsForm cart)
  return [whamlet|
<div>
  <form #edit-details role=form method=post action=@{WarehouseR $ WHPackingListEditDetailsR key} enctype=#{encType}>
    ^{form}
    <div.form-inline>
      $forall action <- [Replace,Insert,Delete]
        <button type="submit" name="action" value=#{tshow action} class="btn btn-default">#{capitalize (show action)}
|]

editForm ::  Maybe PackingList -> Maybe DocumentKey -> Markup -> _ (FormResult UploadParam, Widget)
editForm pl doc = renderBootstrap3 BootstrapBasicForm form
  where form = UploadParam
            <$> pure ""
            <*> (areq textField "invoice ref" (Just . packingListInvoiceRef =<< pl))
            <*> (aopt textField "container" (Just . packingListContainer =<< pl))
            <*> (aopt textField "vessel" (Just . packingListVessel =<< pl))
            <*> (aopt dayField "departure" (Just . packingListDeparture =<< pl))
            <*> (aopt dayField "arriving" (Just . packingListArriving =<< pl))
            <*> (aopt textareaField "comment" (Just . Just . Textarea . documentKeyComment =<< doc))
            <*> pure (Textarea "")

renderEdit :: Int64  -> PackingList -> DocumentKey -> Handler Widget
renderEdit key pl doc = do
  (form, encType) <- generateFormPost (editForm (Just pl) (Just doc))
  return [whamlet|
<div>
  <form #edit role=form method=post action=@{WarehouseR $ WHPackingListEditR key} enctype=#{encType}>
    ^{form}
    <button type="submit" name=action class="btn bt-defaut">Update
|]


-- | Generates a progress bar to track the delivery timing
-- In order to normalize progress bars when displaying different packing list
-- we need some "bounds"
timeProgress :: Maybe Day -> Maybe Day -> Day -> PackingList -> Widget
timeProgress minDateM maxDateM today pl = do
  let minDate = List.minimum . (\x -> x :: [Day]) $ catMaybes [Just (addGregorianMonthsClip (-1) today), minDateM , packingListDeparture pl]
      maxDate = List.maximum . (\x -> x :: [Day]) $ catMaybes [Just (addGregorianMonthsClip 1 today), maxDateM, packingListArriving pl]
  -- let minDate = minimum $ mlcons today ( toMinLenZero $ catMaybes [minDateM , packingListDeparture pl])
  --     maxDate = maximum $ mlcons today (toMinLenZero $ catMaybes [Just today, maxDateM, packingListArriving pl])
      departure = fromMaybe today (packingListDeparture pl)
      arriving = fromMaybe today (packingListArriving pl)
      maxWidth = max 1 (diffDays maxDate minDate) :: Integer

      bars = traceShowId [ (col, 100 * fromIntegral w / fromIntegral maxWidth) | (col,w) <-
                case () of
                 _ | today < departure -> [ ("none" :: Text, diffDays today minDate)
                                          , ("primary" , 1)
                                          , ("none", (diffDays departure today) -1)
                                          , ("info", diffDays arriving departure)
                                          ]
                 _ | today >= departure && today <= arriving -> [ ("none", diffDays departure minDate)
                                                               , ("success", diffDays today departure)
                                                               , ("info", diffDays arriving today)
                                                               ]
                 -- arriving < tody
                 _ | packingListBoxesToDeliver_d pl <= 0 -> [ ("none", diffDays departure minDate)
                                                          , ("success", diffDays arriving departure )
                                                          , ("none", diffDays today arriving)
                                                          , ("primary", 1)
                                                          ] 
                 _ | True -> [ ("none", diffDays departure minDate)
                             , ("danger", diffDays arriving departure )
                             , ("none", diffDays today arriving)
                             , ("primary", 1)
                             ] 
             ]

  [whamlet|
<div.progress>
  $forall (style, width) <- bars
    <div.progress-bar class="progress-bar-#{style}"
                      role="progressbar"
                      style="min-width:0%; width:#{tshow width}%">&nbsp;<span class="sr-only"> #{tshow width}%
|]
  toWidget [cassius|
.progress-bar-none
  opacity:0
                   |]




-- | A row in the spreasheet
-- Depending on the type of box, the order quantity can represent the quantity
-- for the given colour, or total quantity (for all similar boxes)
data PLRow s = PLRow
  { plStyle             :: PLFieldTF s Text Identity Identity
  , plColour            :: PLFieldTF s Text Identity Maybe 
  , plOrderQuantity     :: PLFieldTF s Int Identity Null
  , plFirstCartonNumber :: PLFieldTF s Int Null Null
  , plLastCartonNumber  :: PLFieldTF s Int Null Null
  , plNumberOfCarton    :: PLFieldTF s Int Null Null
  , plQuantityPerCarton :: PLFieldTF s Int Null Null
  , plTotalQuantity     :: PLFieldTF s Int Null Null
  , plLength            :: PLFieldTF s  Double Maybe  Null
  , plWidth             :: PLFieldTF s  Double Maybe Null
  , plHeight            :: PLFieldTF s  Double Maybe Null
  , plVolume            :: PLFieldTF s  Double Maybe Null
  , plTotalVolume       :: PLFieldTF s  Double Maybe Null
  , plWeight            :: PLFieldTF s  Double Maybe Null
  , plTotalWeight       :: PLFieldTF s  Double Maybe Null
  }

data PLRowTypes = PLRawT
                | PLPartialT
                | PLFullBoxT
                | PLPartialBoxT
                | PLTotalT
                | PLOrderRefT
                | PLFinalT deriving (Eq, Read, Show) 

type PLRaw = PLRow 'PLRawT
type PLPartial = PLRow 'PLPartialT
type PLFullBox = PLRow 'PLFullBoxT
type PLPartialBox = PLRow 'PLPartialBoxT
type PLOrderRef = PLRow 'PLOrderRefT
type PLFinal = PLRow 'PLFinalT

deriving instance Show PLRaw
deriving instance Show PLPartial
deriving instance Show PLFullBox
deriving instance Show PLPartialBox
deriving instance Show PLOrderRef
deriving instance Show PLFinal

data PLValid
  = FullBox PLFullBox
  | PartialBox PLPartialBox
  | OrderRef PLOrderRef
type family PLFieldTF (s :: PLRowTypes) a f g where
  PLFieldTF 'PLFullBoxT a f g = FieldForValid a
  PLFieldTF 'PLPartialBoxT a f g = FieldForValid (UnIdentity (f a))
  PLFieldTF 'PLOrderRefT a f g = FieldForValid (UnIdentity (g a))
  PLFieldTF 'PLRawT a f g = FieldForRaw a
  PLFieldTF 'PLPartialT a f g = FieldForPartial a
  PLFieldTF 'PLFinalT a f g = a

columnNames :: [(String, [String])]
columnNames = [("Style", ["S", "Style No.", "Style No", "Style No ."] )
              , ("Colour", ["C", "Col", "Color"])
              ,("Quantity", ["Q", "QTY", "order QTY", "order Qty"])
              ,("1st carton number", ["start", "F", "cn", "C/NO", "first"])
              ,("last carton number", ["end", "last", "e"])
              ,("Number of Carton", ["CTNS", "CTN", "N"])
              ,("Quantity per Carton", ["qc", "Q/C", "QTY/CTN"])
              ,("Total Quantity", ["T", "Total", "TQTY"])
              ,("Length", ["L"])
              ,("Width", ["W"])
              ,("Height", ["H"])
              ,("Volume", ["V", "CBM/CTN"])
              ,("Total Volume", ["TV", "CBM", "TOTAL CBM"])
              ,("Weight", ["N.W/CTN", "N.W./CTN"])
              ,("Total Weight", ["N.W", "N.W.","TOTAL N.W"])
              ]
columnNameMap :: Map String [String]
columnNameMap = buildColumnMap columnNames

instance Csv.FromNamedRecord (PLRow 'PLRawT) where
  parseNamedRecord m = let
    [style, col, qty, cn, cne, n, qc, tot, l, w, h, vol, tvol, weight, tweight] = map fst columnNames
    parse = parseMulti columnNameMap
    in pure PLRow
        <*> m `parse` style
        <*> m `parse` col
        <*> m `parse` qty
        <*> m `parse` cn
        <*> m `parse` cne
        <*> m `parse` n
        <*> m `parse` qc
        <*> m `parse` tot
        <*> m `parse` l
        <*> m `parse` w
        <*> m `parse` h
        <*> m `parse` vol
        <*> m `parse` tvol
        <*> m `parse` weight
        <*> m `parse` tweight


traverseRow :: (PLFieldTF t Double Maybe Null ~ f (PLFieldTF s Double Maybe Null), PLFieldTF t Int Null Null ~ f (PLFieldTF s Int Null Null), PLFieldTF t Int Identity Null ~ f (PLFieldTF s Int Identity Null), PLFieldTF t Text Identity Maybe ~ f (PLFieldTF s Text Identity Maybe), PLFieldTF t Text Identity Identity ~ f (PLFieldTF s Text Identity Identity), Applicative f) => PLRow t -> f (PLRow s)
traverseRow PLRow{..} = pure PLRow
       <*> plStyle
       <*> plColour
       <*> plOrderQuantity
       <*> plFirstCartonNumber
       <*> plLastCartonNumber
       <*> plNumberOfCarton
       <*> plQuantityPerCarton
       <*> plTotalQuantity
       <*> plLength
       <*> plWidth
       <*> plHeight
       <*> plVolume
       <*> plTotalVolume
       <*> plWeight
       <*> plTotalWeight

transformRow :: (Transformable (PLFieldTF t Double Maybe Null) (PLFieldTF s Double Maybe Null), Transformable (PLFieldTF t Int Null Null) (PLFieldTF s Int Null Null), Transformable (PLFieldTF t Int Identity Null) (PLFieldTF s Int Identity Null), Transformable (PLFieldTF t Text Identity Maybe) (PLFieldTF s Text Identity Maybe), Transformable (PLFieldTF t Text Identity Identity) (PLFieldTF s Text Identity Identity)) => PLRow t -> PLRow s
transformRow PLRow{..} = PLRow
       (transform plStyle)
       (transform plColour)
       (transform plOrderQuantity)
       (transform plFirstCartonNumber)
       (transform plLastCartonNumber)
       (transform plNumberOfCarton)
       (transform plQuantityPerCarton)
       (transform plTotalQuantity)
       (transform plLength)
       (transform plWidth)
       (transform plHeight)
       (transform plVolume)
       (transform plTotalVolume)
       (transform plWeight)
       (transform plTotalWeight)

transformPartial :: PLPartialBox -> PLRaw
transformPartial PLRow{..} = PLRow
  (transform plStyle)
  (transform plColour)
  (transform plOrderQuantity)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (transform plLength)
  (transform plWidth)
  (transform plHeight)
  (transform plVolume)
  (transform plTotalVolume)
  (transform plWeight)
  (transform plTotalWeight)

transformOrder :: PLOrderRef -> PLRaw
transformOrder PLRow{..} = PLRow
  (transform plStyle)
  (transform plColour)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)
  (Right Nothing)

type PLBoxGroup = ([PLPartialBox] , PLFullBox)
parsePackingList :: Text -> ByteString -> ParsingResult PLRaw  [(PLOrderRef , [PLBoxGroup])]
parsePackingList orderRef bytes = either id ParsingCorrect $ do
        raws <- parseSpreadsheet columnNameMap Nothing bytes <|&> WrongHeader
        let rawsE = map validate raws
        rows <- sequence rawsE <|&> const (InvalidFormat . lefts $ rawsE)
        groups <-  groupRow orderRef rows <|&> InvalidData
        let validGroups = concatMap (map validateGroup . snd) groups
        -- sequence validGroups <|&> const (InvalidData $ concatMap (either id (\(partials,main) -> transformRow main : map transformPartial partials )) validGroups)
        _ <- sequence validGroups <|&> const (InvalidData . concat $ lefts validGroups)
        Right $  map (map reverse) groups
        -- Right $  valids

        where validate :: PLRaw -> Either PLRaw PLValid
              validate raw@PLRow{..} = do
                partial <- traverseRow raw <|&> const raw
                case partial of
                 _ | Just full <- traverseRow (partial :: PLPartial) -> Right (FullBox full)
                 PLRow{..} | Just style <- plStyle
                           , Just colour <- plColour
                           , Just qty <- plOrderQuantity
                           , 0 <- fromMaybe 0 (validValue <$> plTotalQuantity)
                           -> Right $ PartialBox ( PLRow style colour qty
                                                         () () () () ()
                                                         plLength plWidth plHeight
                                                         plWeight plTotalWeight
                                                         plVolume plTotalVolume
                                                 )
                 PLRow (Just ref) Nothing Nothing Nothing Nothing Nothing
                                  Nothing Nothing Nothing Nothing Nothing
                                  Nothing Nothing Nothing Nothing
                            -> Right $ OrderRef ( PLRow ref Nothing () () () ()
                                                                () () () () ()
                                                                () () () ()
                                                  )
                 _  -> Left raw

              createOrder orderRef0 = PLRow (Provided orderRef0) Nothing () () () () () () () () () () ()  () ():: PLOrderRef
              groupRow :: Text -> [PLValid] -> Either [PLRaw] [ (PLOrderRef , [PLBoxGroup])]
              groupRow orderRef0 rows = go (createOrder orderRef0) rows [] []
                where
                    go :: PLOrderRef -> [PLValid] -> [PLPartialBox] -> [PLBoxGroup] -> Either [PLRaw ][(PLOrderRef, [PLBoxGroup])]
                    -- go order valids partials groups
                    go order [] [] groups = Right [(order, groups)]
                    go _ [] partials@(_:_) _ = let raws = map transformPartial partials
                                         in  Left $ List.init raws ++ [(List.last raws) {plFirstCartonNumber = Left (InvalidValueError "Box not closed" "")}]
                    go order (FullBox full:rows0) partials groups = go order rows0 []  ((partials, full):groups)
                    go order (PartialBox partial:rows0) partials groups = go order rows0 (partials++[partial]) groups
                    go order (OrderRef order':rows0) [] groups = ((order, groups) :) <$> go order' rows0 [] []
                    go order (OrderRef order':rows0) _ _ = Left [(transformOrder order') {plColour = Left (InvalidValueError "Box not closed" "") } ]
                    go _ _ _ _ = error "PackingList::groupRow should not append"
              validateGroup :: PLBoxGroup -> Either [PLRaw] PLBoxGroup 
              validateGroup grp@(partials, main) = let
                final = transformRow main :: PLFinal
                -- TODO remove use of validValue and use final instead of main when needed
                orderQty = sum (map (validValue . plOrderQuantity) partials) + (validValue $ plOrderQuantity main)
                (l,w,h) = (,,) <$> plLength <*> plWidth <*> plHeight $ final
                (v,wg) = (,) <$> plVolume <*> plWeight $ final
                onErrors :: [PLRaw -> PLRaw]
                onErrors = catMaybes [ if orderQty /= validValue (plTotalQuantity main)
                                       then Just $ \r -> r {plTotalQuantity = Left (InvalidValueError "Total quantity doesn't match order quantities" (tshow . validValue $ plTotalQuantity main)) }
                                       else Nothing
                                     , if validValue (plTotalQuantity main)
                                          /= (validValue $ plQuantityPerCarton main)
                                             * (validValue $ plNumberOfCarton main)
                                       then Just $ \r -> r {plTotalQuantity = Left (InvalidValueError "Total quantity doesn't carton quantities" (tshow . validValue $ plTotalQuantity main)) }
                                       else Nothing
                                     , if (validValue $ plLastCartonNumber main)
                                          - (validValue $ plFirstCartonNumber main)
                                          +1 /= (validValue $ plNumberOfCarton main)
                                       then Just $ \r -> r {plNumberOfCarton = Left (InvalidValueError "Number of carton doesn't match carton numbers" (tshow . validValue $ plNumberOfCarton main)) }
                                       else Nothing
                  -- check if provid ed that partial dimension = main one
                                     , if not . null $ filter (/= plLength main) (mapMaybe plLength partials)
                                       then Just $ \r -> r { plLength = Left (InvalidValueError "Box Dimension should be the same within a box" (tshow . validValue $ plLength main)) }
                                       else Nothing
                                     , if not . null $ filter (/= plWidth main) (mapMaybe plWidth partials)
                                       then Just $ \r -> r { plWidth = Left (InvalidValueError "Box Dimension should be the same within a box" (tshow . validValue $ plWidth main)) }
                                       else Nothing
                                     , if not . null $ filter (/= plHeight main) (mapMaybe plHeight partials)
                                       then Just $ \r -> r { plHeight = Left (InvalidValueError "Box Dimension should be the same within a box" (tshow . validValue $ plHeight main)) }
                                       else Nothing
                                     , if abs(l*w*h/1000000 - v) > 1e-2
                                       then Just $ \r -> r { plVolume = Left (InvalidValueError "Volume doesn't match dimensions" (tshow . validValue $ plVolume main)) }
                                       else Nothing
                                     , if  abs (wg * fromIntegral (plNumberOfCarton final) - plTotalWeight final) > 1e-2 
                                       then Just $ \r -> r { plWeight = Left (InvalidValueError "Total weight doesn't match boxes weight" (tshow . validValue $ plWeight main)) }
                                       else Nothing
                                     , if abs (l*w*h/1000000 * fromIntegral (plNumberOfCarton final) - plTotalVolume final) > 1e-2 -- we can't use v(olume) as it's rounded
                                       then Just $ \r -> r { plVolume = Left (InvalidValueError "Total volume doesn't match boxes volume" (tshow . validValue $ plVolume main)) }
                                       else Nothing
                                     ]
                in case onErrors of
                    [] -> Right grp
                    updaters -> Left $ (foldr (($)) (transformRow main) updaters) : map transformPartial partials
                  -- checkDimensions = let







-- ** Parse delivery cart

-- | Wrap result of parseDeliveryList in a newtype to be renderable

newtype ParseDeliveryError = ParseDeliveryError (Either Text Text) deriving (Eq, Show)
parseDeliverList :: Text -> ParsingResult ParseDeliveryError ([PackingListDetailId], [PackingListDetailId])
parseDeliverList cart = let
  parsed = map parseLine (filter (not . isPrefixOf "--") (lines cart))
  parseLine line = let
    fields = splitOn "," line
    in case fields of
      (keyT:_) | Just key <- readMay keyT -> if key > 0  then Right (Right key, line)
                                                         else Right (Left (-key), line)

      _ -> Left line
  toKeys = map (PackingListDetailKey . SqlBackendKey)

  in case sequence parsed of
      Right keys -> let ks = map fst keys
                    in ParsingCorrect (toKeys $ rights ks, toKeys $ lefts ks)

      Left _ -> InvalidFormat $ map (ParseDeliveryError . map snd) parsed









-- * Render
instance Num ()
renderRow :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, Renderable (PLFieldTF t Text Identity Identity), Renderable (PLFieldTF t Text Identity Maybe), Renderable (PLFieldTF t Int Identity Null), Renderable (PLFieldTF t Int Null Null), Renderable (PLFieldTF t Double Maybe Null)) => PLRow t -> WidgetT App m ()
renderRow PLRow{..} = do
  [whamlet|
          <td.pl>^{render plStyle}
          <td.pl>^{render plColour}
          <td.pl>^{render plOrderQuantity}
          <td.pl>^{render plFirstCartonNumber}
          <td.pl>-
          <td.pl>^{render plLastCartonNumber}
          <td.pl>^{render plNumberOfCarton}
          <td.pl>^{render plQuantityPerCarton}
          <td.pl>^{render plTotalQuantity}
          <td.pl>^{render plLength}
          <td.pl>x
          <td.pl>^{render plWidth}
          <td.pl>x
          <td.pl>^{render plHeight}
          <td.pl>^{render plVolume}
          <td.pl>^{render plTotalVolume}
          <td.pl>^{render plWeight}
          <td.pl>^{render plTotalWeight}
          |]

renderHeaderRow :: (MonadBaseControl IO m, MonadThrow m, MonadIO m) => WidgetT site m ()
renderHeaderRow = [whamlet|
  <tr>
    <th>Style
    <th>Colour
    <th>Order Qty
    <th>1st CTN/N
    <th>
    <th>last CTN/N
    <th># of CTN
    <th>Qty/CTN
    <th>Total Qty
    <th>Length
    <th>
    <th>Width
    <th>
    <th>Height
    <th>Volume
    <th>Total Volume
    <th>Weight
    <th>Total Weight
|]
    -- <th>Vol/CNT
    -- <th>Total Vol

-- renderRows :: (ToMarkup a) => (r -> a) -> [r] -> Widget
renderRows :: _ => (r -> a) -> [r] -> Widget
renderRows classFor rows = do
  [whamlet|
<table.table.table-bordered.table-hover>
  ^{renderHeaderRow}
  $forall row <- rows
    <tr class="#{classFor row}">^{render row}
        |]


instance Renderable PLRaw where render = renderRow
instance Renderable [PLRaw] where render = renderRows (const ("" :: String))


instance Renderable [ParseDeliveryError] where
  render lines_= [whamlet|
<ul>
  $forall (ParseDeliveryError line) <- lines_
    $case line
      $of Left line'
        <li.text-danger>#{line'}
      $of Right line'
        <li>#{line'}
|]

-- * Saving
createDocumentKey :: (BaseBackend backend ~ SqlBackend, AuthId master ~ Key User, PersistStoreWrite backend, YesodAuth master) => Text -> Text -> Text -> ReaderT backend (HandlerT master IO) (Key DocumentKey)
createDocumentKey key reference comment = do
  userId <- lift requireAuthId
  processedAt <- liftIO getCurrentTime
  let documentKey = DocumentKey "packinglist" reference
                                comment
                                key userId processedAt
  insert documentKey



savePLFromRows :: Text -> UploadParam -> [(PLOrderRef, [PLBoxGroup])] -> Handler (PackingList, [PackingListDetail])
savePLFromRows key param sections = do
  runDB $ do
    docKey <- createDocumentKey key (invoiceRef param) (maybe "" unTextarea (comment param))
    let nOfCartons (_, groups) = sum (map (validValue . plNumberOfCarton . snd) groups)
        pl = createPLFromForm docKey (sum (map nOfCartons sections)) param
    pKey <- insert pl
    let detailFns = concatMap (createDetailsFromSection pKey) sections

    barcodes' <- generateBarcodes "DL" (packingListArriving pl) (packingListBoxesToDeliver_d pl)
    let barcodes = if take 5 (invoiceRef param) == "16107"
                     then -- get box number instead
                          map (\fn -> toStrict $ formatBarcode "DL16DE" (packingListDetailBoxNumber (fn "")))
                              detailFns
                     else barcodes'
    let details = zipWith ($) detailFns barcodes

    insertMany details
    return (pl, details)

createPLFromForm :: DocumentKeyId -> Int -> UploadParam ->  PackingList
createPLFromForm docKey nOfBoxes =
  pure PackingList
  <*> invoiceRef
  <*> vessel
  <*> container
  <*> (pure docKey)
  <*> (pure nOfBoxes)
  <*> departure
  <*> arriving

createDetailsFromSection :: PackingListId -> (PLOrderRef, [PLBoxGroup]) -> [Text -> PackingListDetail]
createDetailsFromSection pKey (orderRef, groups) = do
  let ref = validValue $ plStyle orderRef
  concatMap (createDetails pKey ref) groups

-- | one row in the spreadsheet can create multiple rows if there are more
-- than one carton
createDetails :: PackingListId -> Text -> PLBoxGroup -> [Text -> PackingListDetail]
createDetails pKey orderRef (partials, main') = do
  let main = transformRow main' :: PLFinal
  let content = Map.fromListWith (+)
                                 ( (plColour main, plOrderQuantity main `div` (end-begin+1))
                                 : [(validValue $ plColour p, validValue $ plOrderQuantity p) | p <- partials]
                                 )
      (begin, end) = (,) <$> plFirstCartonNumber <*> plLastCartonNumber $ main
      ns = [begin..end]
  [ (\bc -> pure PackingListDetail
     <*> (pure pKey)
     <*> plStyle
     <*> (pure content)
     <*> (pure orderRef)
     <*> (pure n)
     <*> (pure bc)
     <*> plLength
     <*> plWidth
     <*> plHeight
     <*> plWeight
     <*> (pure False)
     $ main
    )
   | n <- ns]

updateDocumentKey :: (AuthId master ~ Key User, BaseBackend backend ~ SqlBackend, YesodAuth master, PersistStoreWrite backend, PersistUniqueRead backend) => Key PackingList -> ByteString -> ReaderT backend (HandlerT master IO) ()
updateDocumentKey plKey bytes  = do
  let key = computeDocumentKey bytes

  documentKey <- getBy (UniqueSK key)


  pl <- getJust plKey
  oldKey <- getJust (packingListDocumentKey pl)

  let reference = packingListInvoiceRef pl
      comment = documentKeyComment oldKey ++ "\nedited"

  docKey <- case documentKey of
    Nothing -> createDocumentKey key reference comment
    Just k -> return $ entityKey k

  update plKey [PackingListDocumentKey =. docKey ]

replacePLDetails :: (Element mono ~ (PLOrderRef, [PLBoxGroup]), BaseBackend backend ~ SqlBackend, AuthId master ~ Key User, MonoFoldable mono, PersistQueryWrite backend, PersistUniqueRead backend, YesodAuth master) => Key PackingList -> mono -> ByteString -> ReaderT backend (HandlerT master IO) ()
replacePLDetails plKey sections cart = do
  -- As we total discard the content of the previous document
  -- There is no need to keep a link to the old document key.
  -- Instead, we need to update it with the current cart.

  updateDocumentKey plKey cart
  deleteWhere [ PackingListDetailPackingList ==. plKey
              , PackingListDetailDelivered ==. False
              ]
  insertPLDetails plKey sections

insertPLDetails :: (Element mono ~ (PLOrderRef, [PLBoxGroup]), BaseBackend backend ~ SqlBackend, MonoFoldable mono, PersistStoreWrite backend, PersistUniqueRead backend, MonadIO m) => Key PackingList -> mono -> ReaderT backend m ()
insertPLDetails plKey sections = do
  pl <- getJust plKey
  barcodes <- generateBarcodes "DL" (packingListArriving pl) (packingListBoxesToDeliver_d pl)

  let detailFns = concatMap (createDetailsFromSection plKey) sections
      details = zipWith ($) detailFns barcodes

  insertMany_ details



deletePLDetails :: (BaseBackend backend ~ SqlBackend, Element mono ~ (PLOrderRef, [PLBoxGroup]), MonadIO m, PersistUniqueWrite backend, MonoFoldable mono) => PackingListId -> mono -> ReaderT backend m ()
deletePLDetails plKey sections = do
  let detailFns = concatMap (createDetailsFromSection plKey) sections
      details = zipWith ($) detailFns (repeat "<dummy>")

  forM_ details (deleteBy . ( UniquePLD plKey <$> packingListDetailReference
                                              <*> packingListDetailStyle
                                              <*> packingListDetailBoxNumber
                          )
                )