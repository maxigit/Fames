{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
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

data SavingMode = Validate | Save deriving (Eq, Read, Show)

getWHStocktakeR :: Handler Html
getWHStocktakeR = renderWHStocktake Validate 200 "Enter Stocktake" (return ())

renderWHStocktake :: SavingMode -> Int -> Text -> Widget -> Handler Html
renderWHStocktake mode status title pre = do
  let (action, button) = case mode of
        Validate -> (WHStocktakeR, "vaildate" :: Text)
        Save -> (WHStocktakeSaveR, "save")
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  setMessage (toHtml title)
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
                                                               
  <div .pre> ^{pre}
  <form #upload-form role=form method=post action=@{WarehouseR action} enctype=#{upEncType}>
    ^{uploadFileFormW}
    <button type="sumbit" name="#{button}" .btn .btn-default>Validate
|]


postWHStocktakeR :: Handler Html
postWHStocktakeR = processStocktakeSheet Validate

postWHStocktakeSaveR :: Handler Html
postWHStocktakeSaveR = processStocktakeSheet Save

-- | Display or save the uploaded spreadsheet
-- At the moment saving a spreadsheet involves having to upload it twice.
-- Once for validation and once for processing. We could use a session or similar
-- to "save" the validation processing.
processStocktakeSheet :: SavingMode -> Handler Html 
processStocktakeSheet mode = do
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
          WrongHeader invalid -> Left $ renderWHStocktake mode 422 "Invalid file or columns missing" (render invalid)
          InvalidFormat raws -> Left $ renderWHStocktake mode 422 "Invalid cell format" (renderRows raws)
          InvalidData raws ->  Left $ renderWHStocktake mode 422 "Invalid data" (renderRows raws)
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
  

data RowTypes = RawT | PartialT | ValidT | FinalT deriving (Read, Show, Eq)
type RawRow = TakeRow RawT -- Raw data. Contains if the original text value if necessary
type PartialRow = TakeRow PartialT -- Well formatted row. Can contains blank
type ValidRow = TakeRow ValidT -- Contains valid value with guessed/provided indicator
type FinalRow = TakeRow FinalT -- Contains value with ornement

deriving instance Show RawRow
deriving instance Show PartialRow
deriving instance Show ValidRow

type family FieldTF (s :: RowTypes) a where
  FieldTF 'RawT (Maybe a) = Either InvalidField (Maybe (ValidField a))
  FieldTF 'RawT a = Either InvalidField (Maybe (ValidField a))
  FieldTF 'PartialT (Maybe a) = (Maybe (ValidField a))
  FieldTF 'PartialT a = (Maybe (ValidField a))
  FieldTF 'ValidT a = ValidField a
  FieldTF 'FinalT a = a
 

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
  let filleds = scanl fillFromPrevious (validateRow NoCheckBarcode row) rows :: [Either RawRow ValidRow]
      transformRow' r = let p = transformRow r  
                        in transformRow (p :: PartialRow)
  valids <- sequence  filleds <|&> (const $ map (either id (transformRow')) filleds)
  Right valids


data ValidationMode = CheckBarcode | NoCheckBarcode deriving Eq

validateRow :: ValidationMode -> PartialRow -> Either RawRow ValidRow
validateRow validateMode row@(TakeRow (Just rowStyle) (Just rowColour) ( Just rowQuantity)
                    (Just rowLocation) (Just rowBarcode)
                    (Just rowLength) (Just rowHeight) ( Just rowWidth)
                    (Just rowDate) (Just rowOperator))
  = if isBarcodeValid (fromStrict $ validValue rowBarcode) || validateMode == NoCheckBarcode
    then Right $ (TakeRow{..}  )
    else Left $ (transformRow row) {rowBarcode=Left (ParsingError "Invalid barcode" (validValue rowBarcode))}
validateRow _ invalid =  Left $ transformRow invalid

fillFromPrevious :: Either RawRow ValidRow -> PartialRow -> Either RawRow ValidRow
fillFromPrevious  _ _ = error "IMplement barcode sequence check"
fillFromPrevious (Left prev) partial =  Left $ transformRow partial
fillFromPrevious (Right previous) partial
  | Right valid  <- validateRow CheckBarcode partial = Right valid
  | otherwise = let
      style    = rowStyle partial `fillValue` transform (rowStyle previous)
      colour   = rowColour partial `fillValue` transform (rowColour previous)
      quantity = rowQuantity partial `fillValue` transform (rowQuantity previous)
      location = rowLocation partial `fillValue` transform (rowLocation previous)
      barcode  = rowBarcode partial `fillBarcode` transform (rowBarcode previous)
      length   = rowLength partial `fillValue` transform (rowLength previous)
      width    = rowWidth partial `fillValue` transform (rowWidth previous)
      height   = rowHeight partial `fillValue` transform (rowHeight previous)
      date     = rowDate partial `fillValue` transform (rowDate previous)
      operator = rowOperator partial `fillValue` transform (rowOperator previous)

      raw = (TakeRow (Just <$> style) (Just <$> colour) (Just <$> quantity)
                    (Just <$> location) (Just <$> barcode)
                    (Just <$> length) (Just <$> width) (Just <$> height)
                    (Just <$> date) (Just <$> operator) :: RawRow)
      in (validateRow CheckBarcode) =<< validateRaw raw

fillValue :: (Maybe (ValidField a)) -> Either InvalidField (ValidField a) -> Either  InvalidField (ValidField a)
fillValue (Just new) _ = Right new
fillValue Nothing old = old

-- | Fill barcodes in a sequence (ie previous + 1)
-- However, we also need to check the last of a sequence is given and correct.

fillBarcode :: (Maybe (ValidField Text)) -> Either InvalidField (ValidField Text) -> Either InvalidField  (ValidField Text)
fillBarcode new prevE =
  case (new, prevE) of
    (Just  (Provided "-"), Right prev) -> Right (guess prev)
    (Just barcode, Right prev) -> do -- we need to check if it's valid or miss a prefix
      let invalid = Left $ ParsingError "Invalid Barcode" (validValue barcode)
          splitBarcodeE s = maybe invalid  Right $ splitBarcode s

      -- even though the prefix has been set, we don't count it as guessed.
      (prefix, n, c) <- splitBarcodeE (fromStrict $ validValue barcode)
      (prefix0, n0, _) <- splitBarcodeE (fromStrict $ validValue prev)

      -- add prefix if missing
      let prefix' = if null prefix then prefix0 else prefix
          new = formatBarcode prefix' n
      -- check suffix is correct
      (_,_,newC) <- splitBarcodeE new

      if newC == c
        then Right $ Provided (toStrict new)
        else Left $ ParsingError "Invalid Barcode" (validValue barcode)

    (Just barcode, _)  -> Right (barcode)
    (Nothing,_) -> do -- Either
                 prev <- validValue <$> prevE
                 case nextBarcode $ fromStrict prev of
                   Nothing -> Left $ ParsingError ("Previous barcode invalid" <> prev) ""
                   Just barcode -> Right (Guessed $ toStrict barcode)
    

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
    <*> (allFormatsDay <$$$$> m `parse` "Date Checked" )
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
