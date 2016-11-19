{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, LiberalTypeSynonyms #-}
module Handler.WH.Stocktake where

import Import
import Handler.CsvUtils

import qualified Data.Csv as Csv
import Yesod.Form.Bootstrap3 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)


-- * Requests

getWHStocktakeR :: Handler Html
getWHStocktakeR = renderWHStocktake 200 "Enter Stocktake" (return ())

renderWHStocktake :: Int -> Text -> Widget -> Handler Html
renderWHStocktake status title pre = do
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  setMessage (toHtml title)
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
<h1> Upload a stocktake spreadsheet
  <div .pre> ^{pre}
  <form #upload-form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{upEncType}>
    ^{uploadFileFormW}
    <button type="sumbit" name="validate" .btn .btn-default>Validate
    <button type="submit" name="process" .btn .btn-default>Process
|]


postWHStocktakeR :: Handler Html
postWHStocktakeR = do
  ((fileResp, postFileW), enctype) <- runFormPost uploadFileForm
  case fileResp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess (fileInfo, encoding) -> do
      spreadsheet <- readUploadUTF8 fileInfo encoding
      either id defaultLayout $ do
        -- expected results
        --   Everything is fine : [These Stocktake Boxtake]
        --   wrong header : 
        --   good header but invalid format
        --   parsable but invalid
        case parseTakes spreadsheet of
          WrongHeader invalid -> Left $ renderWHStocktake 422 "Invalid file or columns missing" (render invalid)
          InvalidFormat raws -> Left $ renderWHStocktake 422 "Invalid cell format" (renderRows raws)
          InvalidData raws ->  Left $ renderWHStocktake 422 "Invalid data" (renderRows raws)
          ParsingCorrect rows -> Right $ renderRows rows
        
  

-- * Csv

-- | a take row can hold stocktake and boxtake information
data TakeRow s = TakeRow
  { rowStyle :: FieldTF s Text
  , rowColour :: FieldTF s Text 
  , rowQuantity :: FieldTF s Int
  , rowLocation :: FieldTF s Text
  , rowBarcode :: FieldTF s Text
  , rowLength :: FieldTF s Double
  , rowWidth :: FieldTF s Double
  , rowHeight :: FieldTF s Double
  , rowDate :: FieldTF s Day
  , rowOperator :: FieldTF s Text
  }
  

data RowTypes = RawT | PartialT | ValidT deriving (Read, Show, Eq)
type RawRow = TakeRow RawT
type PartialRow = TakeRow PartialT
type ValidRow = TakeRow ValidT

type family FieldTF (s :: RowTypes) a where
  FieldTF 'RawT (Maybe a) = Either InvalidField (Maybe a)
  FieldTF 'RawT a = Either InvalidField (Maybe a)
  FieldTF 'PartialT (Maybe a) = (Maybe a)
  FieldTF 'PartialT a = (Maybe a)
  FieldTF 'ValidT a = a
 

-- | Validates if a raw row has been parsed properly. ie cell are well formatted
validateRaw :: RawRow -> Either RawRow PartialRow
validateRaw raw= either (const $ Left raw) Right $ do
  rowStyle <- rowStyle raw
  rowColour <- rowColour raw
  rowQuantity <- rowQuantity raw
  rowLocation <- rowLocation raw
  rowBarcode <- rowBarcode raw
  rowLength <- rowLength raw
  rowWidth <- rowWidth raw
  rowHeight <- rowHeight raw
  rowDate <- rowDate raw
  rowOperator <- rowOperator raw
  Right TakeRow{..}

-- | Validates if a row is invalid or not. ie 
validateRows :: [PartialRow] -> Either [RawRow] [ValidRow]
validateRows rows = Left (map transformRow rows)

data ParsingResult
  = WrongHeader InvalidSpreadsheet
  | InvalidFormat [RawRow]-- some cells can't be parsed
  | InvalidData [RawRow]-- each row is well formatted but invalid as a whole
  | ParsingCorrect [ValidRow] -- Ok

parseTakes  :: ByteString -> ParsingResult
parseTakes bytes = either id ParsingCorrect $ do
  raws <- parseSpreadsheet mempty Nothing bytes <|&> WrongHeader
  rows <- validateAll validateRaw raws <|&> InvalidFormat
  validateRows rows <|&> InvalidData

  where validateAll = error "validateAll not implemented" :: (a-> Either b c) -> [a] -> Either [b] [c]
        toTake = error "toTake not implemented"




transformRow TakeRow{..} = TakeRow
  (transform rowStyle)
  (transform rowColour)
  (transform rowQuantity)
  (transform rowLocation)
  (transform rowBarcode)
  (transform rowLength)
  (transform rowWidth)
  (transform rowHeight)
  (transform rowDate)
  (transform rowOperator)
 

    

-- * Csv
instance Csv.FromNamedRecord RawRow where
  parseNamedRecord m = pure TakeRow
    <*> m `parse` "rowStyle"
    <*> m `parse` "rowColour"
    <*> m `parse` "rowQuantity"
    <*> m `parse` "rowLocation"
    <*> m `parse` "rowBarcode"
    <*> m `parse` "rowLength"
    <*> m `parse` "rowWidth"
    <*> m `parse` "rowHeight"
    <*> (allFormatsDay <$$$> m `parse` "rowDate" )
    <*> m `parse` "rowOperator"
    where parse m colname = do
            -- parse as a text. To get the cell content
            t <- m Csv..: colname
            -- the real value to parse, can fail
            val <- m Csv..: colname
            return $ toError t val


-- * Rendering

renderRow TakeRow{..} = do
  [whamlet|
<td.stocktakeStyle>^{render rowStyle}
<td.stocktakeColour>^{render rowColour}
<td.stocktakeQuantity>^{render rowQuantity}
<td.stocktakeLocation>^{render rowLocation}
<td.stocktakeBarcode>^{render rowBarcode}
<td.stocktakeLength>^{render rowLength}
<td.stocktakeWidth>^{render rowWidth}
<td.stocktakeHeight>^{render rowHeight}
<td.stocktakeDate>^{render rowDate}
<td.stocktakeOperator>^{render rowOperator}
|]

instance Renderable RawRow where render = renderRow
instance Renderable ValidRow where render = renderRow


renderRows rows = do
  [whamlet|
<table.table.table-bordered>
  <tr>
    <th>Style
    <th>Colour
    <th>Quantity
    <th>Location
    <th>Barcode
    <th>Length
    <th>Width
    <th>Height
    <th>Date
    <th>Operator
    $forall row  <- rows
      <td> ^{render row}
|]
