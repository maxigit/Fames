{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
module Handler.WH.PackingList
( getWHPackingListR
, postWHPackingListR
, getWHPackingListViewR
-- , postWHPackingListViewR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import qualified Data.Csv as Csv
import Data.List (transpose)
import qualified Data.List as List
import Database.Persist.MySQL

data Mode = Validate | Save deriving (Eq, Read, Show)

-- | Type of file
data Format = PartialFirst deriving (Eq, Read, Show)
data UploadParam = UploadParam
  { orderRef :: Text -- ^ original order reference
  , invoiceRef :: Text -- ^ name of the file to upload
  , description :: Textarea -- ^ any comment
  , spreadsheet :: Textarea -- ^ the actual spreadsheet to upload/process
  } deriving (Eq, Read, Show)

uploadForm param = renderBootstrap3 BootstrapBasicForm form
  where form = UploadParam
            <$> (areq textField "order ref" (fmap orderRef param))
            <*> (areq textField "invoice ref" (fmap invoiceRef param ))
            <*> (areq textareaField "description" (fmap description param) )
            <*> (areq textareaField "spreadsheet" (fmap spreadsheet param) )

getWHPackingListR :: Handler Html
getWHPackingListR = renderWHPackingList Validate Nothing 200 (setInfo "Enter a packing list") (return ())

renderWHPackingList :: Mode -> (Maybe UploadParam) -> Int -> Handler () ->  Widget -> Handler Html
renderWHPackingList mode param status message pre = do
  let btn = case mode of
        Validate -> "primary" :: Text
        Save -> "danger"
      
  (form, encType) <- generateFormPost (uploadForm param)
  message
  sendResponseStatus (toEnum status) =<< defaultLayout
--     toWidget [cassius|
-- tr.table-top-border td
--   border-top: black thin solid black
--                      |]

    [whamlet|
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
        <button type="submit" name="#{tshow mode}" class="btn btn-#{btn}">#{tshow mode}
                        |]
  
--  [[1,2]
--  ,[3]
--  ] =>
-- [[Just 1,Just 3]]
-- ,[Just 2,Nothing]
-- ]
transposeM :: [[a]] -> [[Maybe a]]
transposeM lines  =
  let maxLength = List.maximum (map List.length lines)
      linesM = map (take maxLength . (++ repeat Nothing) . map Just) lines
  in transpose linesM


  
postWHPackingListR :: Handler Html
postWHPackingListR = do
  action <- lookupPostParam "action"
  ((resp, view), encType) <- runFormPost (uploadForm Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> defaultLayout [whamlet|^{view}|]
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
      onSuccess Save rows = error "Saving PL not implemented"
      onSuccess Validate groups =
        renderWHPackingList Save (Just param)
                            200 (setSuccess "Packing list valid")
                            [whamlet|
<div.panel-group>
  $forall section <- groups
    ^{renderSection section}
                                    |]
    
  renderParsingResult (renderWHPackingList mode (Just param) 422) 
                      (onSuccess mode)
                      (parsePackingList (orderRef param) bytes)
    
      
        
getWHPackingListViewR :: Int64 -> Handler Html
getWHPackingListViewR key = do
  let plKey = PackingListKey (SqlBackendKey key)
  (plM, entities) <- runDB $ do
      pl <- get plKey
      entities <- selectList [PackingListDetailPackingList ==. plKey] []
      return (pl, entities)
  case plM of
    Nothing -> notFound
    Just pl -> do
    
    let header = [ ("Invoice Ref" , packingListInvoiceRef pl )
                , ("To deliver", tshow (packingListBoxesToDeliver_d pl))
                ] :: [(Text, Text)]
    let fieldsS = [ [ ("Container" , packingListContainer pl )
                    , ("Vessel", packingListVessel pl)
                    ]
                  , error "document"
                  , [ ("ETD", tshow <$> packingListEtd pl)
                    , ("ETA", tshow <$> packingListEta pl)
                    ]
                  ] :: [[(Text, Maybe Text)]]
    let panelClass delivered | delivered == 0 = "success" :: Text
                             | delivered == length entities = "info"
                             | otherwise = "danger"



    defaultLayout $ do
      [whamlet|
        <div.panel. class="panel-#{panelClass (packingListBoxesToDeliver_d pl)}">
          <div.panel-heading>
              $forall (field, value) <- header
                <label>#{field}
                <p>#{value}
          <div.panel-body>
            $forall fields <- fieldsS
              <div>
                $forall (field, value) <- fields
                  <label>#{field}
                  <p>#{fromMaybe "" value}

          <p>#{fromMaybe "" (packingListComment pl)}
              |]
      [whamlet|
          <div.panel.panel-default>
            <div.panel-heading> Details
            ^{entitiesToTable getDBName entities}
              |]

-- getWHPackingListTextcartR :: Int64 -> Handler Html
-- getWHPackingListTextcartR plId = undefined


-- | A row in the spreasheet
-- Depending on the type of box, the order quantity can represent the quantity
-- for the given color, or total quantity (for all similar boxes)
data PLRow s = PLRow
  { plStyle :: PLFieldTF s Text Identity Identity
  , plColour :: PLFieldTF s Text Identity Maybe 
  , plOrderQuantity :: PLFieldTF s Int Identity Null
  , plFirstCartonNumber :: PLFieldTF s Int Null Null
  , plLastCartonNumber :: PLFieldTF s Int Null Null
  , plNumberOfCarton :: PLFieldTF s Int Null Null
  , plQuantityPerCarton :: PLFieldTF s Int Null Null
  , plTotalQuantity :: PLFieldTF s Int Null Null
  , plLength :: PLFieldTF s  Double Maybe  Null
  , plWidth :: PLFieldTF s  Double Maybe Null
  , plHeight :: PLFieldTF s  Double Maybe Null
  }

data PLRowTypes = PLRawT | PLPartialT | PLFullBoxT | PLPartialBoxT | PLTotalT | PLOrderRefT deriving (Eq, Read, Show) 

type PLRaw = PLRow PLRawT
type PLPartial = PLRow PLPartialT
type PLFullBox = PLRow PLFullBoxT
type PLPartialBox = PLRow PLPartialBoxT
type PLOrderRef = PLRow PLOrderRefT

deriving instance Show PLRaw
deriving instance Show PLPartial
deriving instance Show PLFullBox
deriving instance Show PLPartialBox
deriving instance Show PLOrderRef

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

columnNames :: [(String, [String])]
columnNames = [("Style", ["S", "Style No.", "Style No", "Style No ."] )
              , ("Colour", ["C", "Col", "Color"])
              ,("Quantity", ["Q", "QTY", "order QTY"])
              ,("1st carton number", ["start", "F", "cn", "C/NO", "first"])
              ,("last carton number", ["end", "last", "e"])
              ,("Number of Carton", ["CTNS", "N"])
              ,("Quantity per Carton", ["qc", "Q/C", "QTY/CTN"])
              ,("Total Quantity", ["T", "Total", "TQTY"])
              ,("Length", ["L"])
              ,("Width", ["W"])
              ,("Height", ["H"])
              ]
columnNameMap = buildColumnMap columnNames

instance Csv.FromNamedRecord (PLRow 'PLRawT) where
  parseNamedRecord m = let
    [style, col, qty, cn, cne, n, qc, tot, l, w, h] = map fst columnNames
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

type PLBoxGroup = ([PLPartialBox] , PLFullBox)
parsePackingList :: Text -> ByteString -> ParsingResult PLRaw  [(PLOrderRef , [PLBoxGroup])]
parsePackingList orderRef bytes = either id ParsingCorrect $ do
        raws <- parseSpreadsheet columnNameMap Nothing bytes <|&> WrongHeader
        let rawsE = map validate raws
        rows <- sequence rawsE <|&> const (InvalidFormat . lefts $ rawsE)
        groups <-  groupRow orderRef rows <|&> const (InvalidData . lefts $ rawsE)
        let validGroups = concatMap (map validateGroup . snd) groups
        -- sequence validGroups <|&> const (InvalidData $ concatMap (either id (\(partials,main) -> transformRow main : map transformPartial partials )) validGroups)
        sequence validGroups <|&> const (InvalidData . concat $ lefts validGroups)
        Right $  groups
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
                                                 )
                 PLRow (Just ref) Nothing Nothing Nothing Nothing Nothing
                                  Nothing Nothing Nothing Nothing Nothing
                            -> Right $ OrderRef ( PLRow ref Nothing () () () ()
                                                                () () () () ()
                                                  )
                 _  -> Left raw
                
              createOrder orderRef = PLRow (Provided orderRef) Nothing () () () () () () () () () :: PLOrderRef
              groupRow :: Text -> [PLValid] -> Either PLRaw [ (PLOrderRef , [PLBoxGroup])]
              groupRow orderRef rows = Right $ go (createOrder orderRef) rows [] []
                where
                    go :: PLOrderRef -> [PLValid] -> [PLPartialBox] -> [PLBoxGroup] -> [(PLOrderRef, [PLBoxGroup])]
                    go order [] [] groups = [(order, groups)]
                    go _ [] partials _ = error "box not finished"
                    go order (FullBox full:rows) partials groups = go order rows []  ((partials, full):groups)
                    go order (PartialBox partial:rows) partials groups = go order rows (partials++[partial]) groups
                    go order (OrderRef order':rows) [] groups = (order, groups) : go order' rows [] []
                    go order (OrderRef order':rows) _ _ = error "box not finished"
                -- go rows partials = traceShow (rows, partials) undefined
              validateGroup :: PLBoxGroup -> Either [PLRaw] PLBoxGroup 
              validateGroup group@(partials, main) = let
                orderQty = sum (map (validValue . plOrderQuantity) partials) + (validValue $ plOrderQuantity main)
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

                                     ]
                in case onErrors of
                    [] -> Right group
                    updaters -> Left $ (foldr (($)) (transformRow main) updaters) : map transformPartial partials
                  -- checkDimensions = let
                          
                   

                  

                     
                        


   
-- * Render
instance Num ()
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
          |]

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
|]
    -- <th>Vol/CNT
    -- <th>Total Vol

renderRows classFor rows = do
  [whamlet|
<table.table.table-bordered.table-hover>
  ^{renderHeaderRow}
  $forall row <- rows
    <tr class="#{classFor row}">^{render row}
        |]

instance Renderable PLRaw where render = renderRow
instance Renderable [PLRaw] where render = renderRows (const ("" :: String))
                              
  
