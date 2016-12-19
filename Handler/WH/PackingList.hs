{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
module Handler.WH.PackingList
( getWHPackingListR
, postWHPackingListR
, postWHPackingListSaveR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import qualified Data.Csv as Csv
import Data.List (transpose)
import qualified Data.List as List

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
    
      
        


postWHPackingListSaveR :: Handler Html
postWHPackingListSaveR = undefined

-- getWHPackingListViewR :: Int64 -> Handler Html
-- getWHPackingListViewR plId = undefined

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
  
type PLBoxGroup = ([PLPartialBox] , PLFullBox)
parsePackingList :: Text -> ByteString -> ParsingResult PLRaw  [(PLOrderRef , [PLBoxGroup])]
parsePackingList orderRef bytes = either id ParsingCorrect $ do
        raws <- parseSpreadsheet columnNameMap Nothing bytes <|&> WrongHeader
        rows <- mapM validate raws <|&> const (InvalidFormat raws)
        valids <-  groupRow orderRef rows <|&> const (InvalidData raws)
        -- Right $ (error "validate group total for whole group and number of boxes") valids
        Right $  valids

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
                              
  
