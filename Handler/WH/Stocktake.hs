{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms, LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
module Handler.WH.Stocktake where

import Import
import Handler.CsvUtils
import WH.Barcode
import Data.List(scanl)

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
                                                               
  <div .pre> ^{pre}
  <form #upload-form role=form method=post action=@{WarehouseR WHStocktakeR} enctype=#{upEncType}>
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

deriving instance Show RawRow
deriving instance Show PartialRow
deriving instance Show ValidRow

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
validateRows [] = Left []
validateRows (row:rows) = do
  -- fill blank with previous value if possible
  let filleds = scanl fillFromPrevious (validateRow row) rows :: [Either RawRow ValidRow]
      transformRow' r = let p = transformRow r  
                        in transformRow (p :: PartialRow)
  valids <- sequence  filleds <|&> (const $ map (either id (transformRow')) filleds)
  Right valids


validateRow :: PartialRow -> Either RawRow ValidRow
validateRow row@(TakeRow (Just rowStyle) (Just rowColour) ( Just rowQuantity)
                    (Just rowLocation) (Just rowBarcode)
                    (Just rowLength) (Just rowHeight) ( Just rowWidth)
                    (Just rowDate) (Just rowOperator))
  | isBarcodeValid (fromStrict rowBarcode) =  Right TakeRow{..}
validateRow invalid =  Left $ transformRow invalid

fillFromPrevious :: Either RawRow ValidRow -> PartialRow -> Either RawRow ValidRow
fillFromPrevious (Left prev) partial =  Left $ transformRow partial
fillFromPrevious (Right previous) partial
  | Right valid  <- validateRow partial = Right valid
  | otherwise = let
      style    = rowStyle partial <|> Just (rowStyle previous)
      colour   = rowColour partial <|> Just (rowColour previous)
      quantity = rowQuantity partial <|> Just (rowQuantity previous)
      location = rowLocation partial <|> Just (rowLocation previous)
      barcode  = rowBarcode partial `fillBarcode` (rowBarcode previous)
      length   = rowLength partial <|> Just (rowLength previous)
      width    = rowWidth partial <|> Just (rowWidth previous)
      height   = rowHeight partial <|> Just (rowHeight previous)
      date     = rowDate partial <|> Just (rowDate previous)
      operator = rowOperator partial <|> Just (rowOperator previous)

      new = TakeRow style colour quantity location barcode
                    length width height
                    date operator :: PartialRow
      in  validateRow new

fillBarcode :: (Maybe Text) -> Text -> Maybe Text
fillBarcode new prev = case (new) of
  (Nothing) -> toStrict <$> nextBarcode (fromStrict prev)
  (Just  "-") -> Just $ prev
  (Just barcode) -> do -- we need to check if it's valid or miss a prefix
    (prefix, n, c) <- splitBarcode (fromStrict barcode)
    (prefix0, n0, _) <- splitBarcode (fromStrict prev)
    -- add prefix if missing
    let prefix' = if null prefix then prefix0 else prefix
        new = formatBarcode prefix' n
    -- check suffix is correct
    (_,_,newC) <- splitBarcode new
    if newC == c || True
      then Just (toStrict new)
      else Nothing



    

data ParsingResult
  = WrongHeader InvalidSpreadsheet
  | InvalidFormat [RawRow]-- some cells can't be parsed
  | InvalidData [RawRow]-- each row is well formatted but invalid as a whole
  | ParsingCorrect [ValidRow] -- Ok

parseTakes  :: ByteString -> ParsingResult
parseTakes bytes = either id ParsingCorrect $ do
  raws <- parseSpreadsheet mempty Nothing bytes <|&> WrongHeader
  rows <- validateAll validateRaw raws <|&> InvalidFormat
  valids <- validateRows rows <|&> InvalidData 
  Right valids

  where -- validateAll :: (a-> Either b c) -> [a] -> Either [b] [c]
        validateAll validate rows = let
          a = (traverse validate rows)
          in a <|&> (const $ map transformRow rows)


          




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
    <*> m `parse` "Style"
    <*> m `parse` "Colour"
    <*> m `parse` "Quantity"
    <*> m `parse` "Location"
    <*> m `parse` "Barcode Number"
    <*> m `parse` "Length"
    <*> m `parse` "Width"
    <*> m `parse` "Height"
    <*> (allFormatsDay <$$$> m `parse` "Date Checked" )
    <*> m `parse` "Operator"
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
      <tr> ^{render row}
|]
