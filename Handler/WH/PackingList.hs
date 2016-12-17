{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
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
  { containerName :: Text -- ^ name of the container
  , reference :: Text -- ^ name of the file to upload
  , description :: Textarea -- ^ any comment
  , spreadsheet :: Textarea -- ^ the actual spreadsheet to upload/process
  } deriving (Eq, Read, Show)

uploadForm param = renderBootstrap3 BootstrapBasicForm form
  where form = UploadParam
            <$> (areq textField "container" (fmap containerName param))
            <*> (areq textField "reference" (fmap reference param ))
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
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
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

  renderParsingResult (renderWHPackingList mode (Just param) 422) 
                      undefined
                      (parsePackingList bytes)
      
        


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
  { plStyle :: PLFieldTF s Text
  , plColour :: PLFieldTF s Text
  , plOrderQuantity :: PLFieldTF s Int
  , plFirstCartonNumber :: PLFieldTF s Int
  , plLastCartonNumber :: PLFieldTF s Int
  , plNumberOfCarton :: PLFieldTF s Int
  , plQuantityPerCarton :: PLFieldTF s Int
  , plTotalQuantity :: PLFieldTF s Int
  , plLength :: PLFieldTF s  Double
  , plWidth :: PLFieldTF s  Double
  , plHeight :: PLFieldTF s  Double
  }

data PLRowTypes = PLRawT | PLFullBoxT | PLPartialBoxT | PLTotalT | PLContainerT deriving (Eq, Read, Show) 

type PLRaw = PLRow PLRawT
type PLFullBox = PLRow PLFullBoxT
type PLPartialBox = PLRow PLPartialBoxT
type PLContainer = PLRow PLContainerT

deriving instance Show PLRaw
deriving instance Show PLFullBox
deriving instance Show PLPartialBox
deriving instance Show PLContainer

data PLValid
  = FullBox PLFullBox
  | PartialPox PLPartialBox
  | Container PLContainer
type family PLFieldTF (s :: PLRowTypes) a where
  PLFieldTF 'PLFullBoxT a = FieldForValid a
  PLFieldTF 'PLRawT a = FieldForRaw a
  PLFieldTF b a = Maybe a

columnNames :: [(String, [String])]
columnNames = [("Style", ["S", "Style No.", "Style No"] )
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

type PLBoxGroup = ([PLPartialBox] , PLFullBox)
parsePackingList :: ByteString -> ParsingResult PLRaw  ([ PLBoxGroup ], [ (PLContainer , PLBoxGroup)])
parsePackingList bytes = either id ParsingCorrect $ do
        raws <- parseSpreadsheet columnNameMap Nothing bytes <|&> WrongHeader
        rows <- mapM validate raws <|&> const (InvalidFormat raws)
        valids <-  groupRow rows <|&> const (InvalidData raws)
        Right $ (error "row validation not implemented") valids

        where validate :: PLRaw -> Either PLRaw PLValid
              validate = error "validate not implementend"
              groupRow :: [PLValid] -> Either PLRaw ([ PLBoxGroup ], [ (PLContainer , PLBoxGroup)])
              groupRow = error "grouprow not implemented"
   
-- * Render
renderRow PLRow{..} = do
  [whamlet|
          <td.pl>^{render plStyle}
          <td.pl>^{render plColour}
          <td.pl>^{render plOrderQuantity}
          <td.pl>^{render plFirstCartonNumber}
          <td.pl>^{render plLastCartonNumber}
          <td.pl>^{render plNumberOfCarton}
          <td.pl>^{render plQuantityPerCarton}
          <td.pl>^{render plTotalQuantity}
          <td.pl>^{render plLength}
          <td.pl>^{render plWidth}
          <td.pl>^{render plHeight}
          |]

renderRows classFor rows = do
  [whamlet|
<table.table.table-bordered.table-hover>
  <tr>
    <th>Style
    <th>Colour
    <th>Order Qty
    <th>1st CTN/N
    <th>last CTN/N
    <th># of CTN
    <th>Qty/CTN
    <th>Total Qty
    <th>Length
    <th>Width
    <th>Height
    <th>Vol/CNT
    <th>Total Vol
    $forall row <- rows
      <tr class="#{classFor row}">^{render row}
          |]

instance Renderable PLRaw where render = renderRow
instance Renderable [PLRaw] where render = renderRows (const ("" :: String))
                              
  
