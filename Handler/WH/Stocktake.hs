{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Handler.WH.Stocktake where

import Import hiding(last)
import Handler.CsvUtils
import WH.Barcode
import Data.List(scanl, last, init, length, head)
import Data.Either

import qualified Data.Csv as Csv
import Yesod.Form.Bootstrap3 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

-- * Requests

data SavingMode = Validate | Save deriving (Eq, Read, Show)

getWHStocktakeR :: Handler Html
getWHStocktakeR = renderWHStocktake Validate 200 (formatInfo "Enter Stocktake") (return ())

renderWHStocktake :: SavingMode -> Int -> Html -> Widget -> Handler Html
renderWHStocktake mode status title pre = do
  let (action, button) = case mode of
        Validate -> (WHStocktakeR, "validate" :: Text)
        Save -> (WHStocktakeSaveR, "save")
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  setMessage title
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
  <div>
    <p>
      <a href=@{WarehouseR WHStocktakeLocationR}>Available locations
  <div .pre> ^{pre}
  <div.well>
    <form #upload-form role=form method=post action=@{WarehouseR action} enctype=#{upEncType}>
      ^{uploadFileFormW}
      <button type="sumbit" name="#{button}" .btn .btn-primary>#{button}
|]


postWHStocktakeR :: Handler Html
postWHStocktakeR = processStocktakeSheet Validate


getWHStocktakeSaveR :: Handler Html
getWHStocktakeSaveR = renderWHStocktake Save 200 (formatInfo "Enter Stocktake") (return ())

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
      (spreadsheet, key) <- readUploadUTF8 fileInfo encoding

      -- TODO move to reuse
      -- check if the document has already been uploaded
      -- and reject it.
      documentKey <- runDB $ getBy (UniqueSK key)
      forM documentKey $ \(Entity _ doc) -> do
        uploader <- runDB $ get (documentKeyUserId doc)
        let msg = [shamlet|Document has already been uploaded
$maybe u <- uploader
  as "#{documentKeyName doc}"
  by #{userIdent u}
  on the #{tshow $ documentKeyProcessedAt doc}.
|]
        case mode of
          Validate -> setWarning msg >> return ""
          Save -> renderWHStocktake mode 422 (formatError msg) (return ()) -- should exit

      locations <- appStockLocations . appSettings <$> getYesod
    -- get all operators and generate a map name, firstname  -> operator, operator id
      operators <- runDB $ selectList [OperatorActive ==. True] [] 
      let operatorKeys = Map.fromListWith (++) $
                   [ (toLower $ operatorNickname op, [Operator' opId op] ) | op'@(Entity opId op) <- operators ]
                <> [ (toLower $ operatorFirstname op <> operatorSurname op, [Operator' opId op] )
                   | op'@(Entity opId op) <- operators
                   ] :: Map Text [Operator']
           -- we need to filter operators key with more than one solution
          pks = Map.map (Data.List.head)  (Map.filter (\ops -> Data.List.length ops ==1) operatorKeys)
          -- findOps = Map.lookup (Map.map (Data.List.head)  (Map.filter (\ops -> Data.List.length ops /=1) operatorKeys)) . toLower :: Text -> Entity Operator
          findOps = (flip Map.lookup) pks  . toLower . (filter (/= ' '))

      userId <- requireAuthId
      processedAt <- liftIO getCurrentTime

      let docKey = DocumentKey (fileName fileInfo) "" key (userId) processedAt

      either id (process locations docKey mode) $ do
        -- expected results
        --   Everything is fine : [These Stocktake Boxtake]
        --   wrong header : 
        --   good header but invalid format
        --   parsable but invalid
        case parseTakes findOps locations spreadsheet of
          WrongHeader invalid -> Left $ renderWHStocktake mode 422 (formatError "Invalid file or columns missing") (render invalid)
          InvalidFormat raws -> Left $ renderWHStocktake mode 422 (formatError "Invalid cell format") (render raws)
          InvalidData raws ->  Left $ renderWHStocktake mode 422 (formatError "Invalid data") (render raws)
          ParsingCorrect rows -> Right rows
          
  where process _ doc Validate rows = renderWHStocktake mode 200 (formatSuccess "Validation ok!") (render rows)
        process locations doc Save rows = (runDB $ do
          let finals = map transformRow rows :: [FinalRow]

          -- rows can't be converted straight away to stocktakes and boxtakes
          -- if in case of many row for the same barcodes, stocktakes needs to be indexed
          -- and boxtake needs to keep only the first one.
          let groups = groupBy ((==) `on` rowBarcode)
                     . sortBy (comparing rowBarcode)
                     $ finals :: [[FinalRow]]
              
          keyId <- insert doc
          let stocktakes =  do 
                group <- groups
                zipWith ($)(mapMaybe (toStocktakeF keyId) group) [1..]

              boxtakes = do
                group <- groups
                take 1 $ mapMaybe (toBoxtake keyId locations) group
          

          forM stocktakes $ \stock -> do
              -- inactive similar key
              updateWhere [StocktakeBarcode ==. stocktakeBarcode stock] [StocktakeActive =. False]
              insert_ stock

          forM boxtakes $ \box -> do
              -- inactive similar key
              updateWhere [BoxtakeBarcode ==. boxtakeBarcode box] [BoxtakeActive =. False]
              insert_ box
            
          setSuccess "Spreadsheet processed!"
          ) >> getWHStocktakeR
       
     


          
        
  

-- * Csv

-- | Wrapper around Operator
data Operator' = Operator' {opId :: OperatorId, op :: Operator} deriving (Eq, Show)
type OpFinder = Text -> Maybe Operator'

-- | a take row can hold stocktake and boxtake information
data TakeRow s = TakeRow
  { rowStyle :: FieldTF s Text 
  , rowColour :: FieldTF s Text  
  , rowQuantity :: FieldTF s (Known Int)
  , rowLocation :: FieldTF s Text 
  , rowBarcode :: FieldTF s Text 
  , rowLength :: FieldTF s Double 
  , rowWidth :: FieldTF s Double 
  , rowHeight :: FieldTF s Double 
  , rowDate :: FieldTF s Day 
  , rowOperator :: FieldTF s Operator'
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
validateRaw :: OpFinder -> RawRow -> Either RawRow PartialRow
validateRaw ops raw= do
  -- preprocess  Operator
  -- Operator is special in a sense it can't be parsed without a map name -> operator
  -- So, when the spreadsheet is parsed, rowOperator is necessary `Left`.
  -- We need to check if the name means something, and if so replace it the operator
  -- However, if we are validating again a valid value, ops shouldn't be need.
  -- We just keep the right value
  let rowOperator' =
        case rowOperator raw of
          Right operator -> Right operator
          Left (ParsingError _ name) -> maybe (Left $ ParsingError ("Can't find operator with name " <> name) name)
                             (Right . Just .Provided)
                             (ops name)
          Left field -> Left field

  either (const $ Left raw {rowOperator = rowOperator'}) Right $ do
  rowStyle <- rowStyle raw
  rowColour <- rowColour raw
  rowQuantity <- rowQuantity raw
  rowLocation <- rowLocation raw
  rowBarcode <- rowBarcode raw
  rowLength <- rowLength raw
  rowWidth <- rowWidth raw
  rowHeight <- rowHeight raw
  rowDate <- rowDate raw
  rowOperator <- rowOperator'

  Right TakeRow{..}

-- | Validates if a row is invalid or not. ie 
validateRows :: [String] -> [PartialRow] -> Either [RawRow] [ValidRow]
validateRows _ [] = Left []
validateRows locations (row:rows) = do
  -- fill blank with previous value if possible
  -- All row needs to be validated. However, except the firs one
  -- we don't the check the barcode as it can be missing the prefix
  let filleds = scanl (fillFromPrevious locations) (validateRow locations CheckBarcode row) rows :: [Either RawRow ValidRow]
      transformRow' r = let p = transformRow r  
                        in transformRow (p :: PartialRow)
  -- we need to check that the last barcode is not guessed
  
      errors = map (either id (transformRow')) filleds
  valids <- sequence  filleds <|&> const errors
  
  case rowBarcode (last valids) of
    Guessed barcode -> let l = last errors
                           l' = l {rowBarcode = Left $ ParsingError "Last barcode of a sequence should be provided" barcode }
                      
                           in Left $ (init errors) ++ [l']
    _ -> Right valids


data ValidationMode = CheckBarcode | NoCheckBarcode deriving Eq

validateRow :: [String] -> ValidationMode -> PartialRow -> Either RawRow ValidRow
validateRow locations validateMode row@(TakeRow (Just rowStyle) (Just rowColour) (Just rowQuantity)
                    (Just rowLocation) (Just rowBarcode)
                    (Just rowLength) (Just rowWidth) ( Just rowHeight)
                    (Just rowDate) (Just rowOperator))
  =  case catMaybes [ if (isBarcodeValid (fromStrict $ validValue rowBarcode)
                         || validateMode == NoCheckBarcode)
                      then Nothing
                      else Just (\r -> r {rowBarcode=Left $ ParsingError "Invalid barcode" (validValue rowBarcode)})
                    , if isLocationValid locations (unpack $ validValue rowLocation)
                      then Nothing
                      else Just(\r -> r  {rowLocation=Left $ ParsingError "Invalid location" (validValue rowLocation)})
                    ] of
       [] -> Right $ TakeRow{..}
       modifiers -> Left $ foldr ($) (transformRow row) modifiers

validateRow _ _ invalid =  Left $ transformRow invalid


fillFromPrevious :: [String] -> Either RawRow ValidRow -> PartialRow -> Either RawRow ValidRow
fillFromPrevious locations (Left prev) partial =
  case validateRow locations CheckBarcode partial of
    Left _ ->  Left $ transformRow partial
    Right valid -> Right valid -- We don't need to check the sequence barcode as the row the previous row is invalid anyway

fillFromPrevious locations (Right previous) partial
  -- | Right valid  <- validateRow CheckBarcode partial = Right valid
  --  ^ can't do that, as we need to check if a barcode sequence is correct

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
      a /~ b = (guess <$> a) /=(guess <$> b)
      modifiers = case rowBarcode partial of
      -- belongs to the previous box, we need to check that box information
        -- are identical (if set)
        Just (Provided "-" ) ->
          [ if location /~ Right (rowLocation previous)
            then \r -> r {rowLocation = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowLocation partial))
                         }
            else id
          , if length /~ Right (rowLength previous)
            then \r -> r {rowLength = Left $ ParsingError (tshow (length, rowLength previous)) -- "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowLength partial))
                         }
            else id
                         
          , if width /~ Right (rowWidth previous)
            then \r -> r {rowWidth = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowWidth partial))
                         }
            else id
          , if height /~ Right (rowHeight previous)
            then \r -> r {rowHeight = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowHeight partial))
                         }
            else id
          , if operator /~ Right (rowOperator previous)
            then \r -> r {rowOperator = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowOperator partial))
                         }
            else id
          ]
        _ -> []
     
        
      in (validateRow locations CheckBarcode) =<< validateRaw (const Nothing) (foldr ($) raw modifiers)

fillValue :: (Maybe (ValidField a)) -> Either InvalidField (ValidField a) -> Either  InvalidField (ValidField a)
fillValue (Just new) _ = Right new
fillValue Nothing old = guess <$> old

-- | Fill barcodes in a sequence (ie previous + 1)
-- However, we also need to check the last of a sequence is given and correct.

fillBarcode :: (Maybe (ValidField Text)) -> Either InvalidField (ValidField Text) -> Either InvalidField  (ValidField Text)
fillBarcode new prevE =
  case (new, prevE) of
    (Just  (Provided "-"), Right prev) -> Right prev -- use the same level of guessing than the previous one
  -- TODO Add datatype to deal with type of barcode
    (Just barcode, Right prev) -> do
      -- we need to check if it's valid or miss a prefix
      -- as well as if it's the end of a sequence
      -- and the same barcode is not used twice.
      -- in theory we should check if a barcode hasn't been used at all
      -- but in practice checking the last one should be enough
      
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

      case (newC == c, prev) of
        (False, _) -> Left $ ParsingError "Invalid Barcode" (validValue barcode)
        (_, Guessed _ ) | n /= n0+1 -> Left $ ParsingError "Barcode Sequence broken" (toStrict new)
        _ | n == n0 -> Left $ ParsingError "Barcode alread in use" (validValue barcode)
        _  -> Right $ Provided (toStrict new)

    (Just barcode, _)  -> Right (barcode)
    (Nothing,_) -> do -- Either
                 prev <- validValue <$> prevE
                 case nextBarcode $ fromStrict prev of
                   Nothing -> Left $ ParsingError ("Previous barcode invalid" <> prev) ""
                   Just barcode -> Right (Guessed $ toStrict barcode)
    
-- | Expand a location pattern to the matching locations
-- ex : ["AB" "AC" "BD"] "A?" -> AB AC
expandLocation :: [String] -> String -> [String]
expandLocation [] pat = [pat]
expandLocation locations pat = let
  pat' = Glob.compile pat
  in filter (Glob.match pat') locations



isLocationValid :: [String] -> String -> Bool
isLocationValid = not . null <$$> expandLocation

data ParsingResult
  = WrongHeader InvalidSpreadsheet
  | InvalidFormat [RawRow]-- some cells can't be parsed
  | InvalidData [RawRow]-- each row is well formatted but invalid as a whole
  | ParsingCorrect [ValidRow] -- Ok

-- | Parses a csv of stocktakes. If a list of locations is given
-- parseTakes that the locations are valid (matches provided location using unix file globing)
parseTakes  :: OpFinder -> [String] -> ByteString -> ParsingResult
parseTakes ops locations bytes = either id ParsingCorrect $ do
    
  raws <- parseSpreadsheet mempty Nothing bytes <|&> WrongHeader
  rows <- validateAll (validateRaw ops) raws <|&> InvalidFormat
  valids <- validateRows locations rows <|&> InvalidData 
  Right valids

  where -- validateAll :: (a-> Either b c) -> [a] -> Either [b] [c]
        validateAll validate rows = let
          validateds = map validate rows
          in case (lefts validateds, rights validateds) of
            ([], valids) -> Right valids
            (invalids,_) -> Left invalids

          -- a = (traverse validate rows)
          -- in a <|&> -- (const $ lefts ) (const $ map transformRow rows)


toStocktakeF :: DocumentKeyId -> FinalRow -> Maybe (Int -> Stocktake)
toStocktakeF docId TakeRow{..} = case rowQuantity of
  Unknown -> Nothing
  Known quantity -> Just $ \index -> Stocktake (rowStyle <> "-" <> rowColour)
                      quantity
                      rowBarcode
                      index
                      rowDate
                      True
                      (opId rowOperator)
                      docId


toBoxtake :: DocumentKeyId -> [String] -> FinalRow -> Maybe Boxtake
toBoxtake docId locations TakeRow{..} =
  Just $ Boxtake rowLength rowWidth rowHeight
                 rowBarcode ( intercalate "" $ pack <$> expandLocation locations (unpack rowLocation))
                 rowDate
                 True
                 (opId rowOperator)
                 docId

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
    <*> m Csv..: "Operator"
    where parse m colname = do
            -- parse as a text. To get the cell content
            t <- m Csv..: colname
            -- the real value to parse, can fail
            val <- m Csv..: colname
            return $ toError t val

instance Csv.FromField (Either InvalidField (Maybe (ValidField (Operator')))) where
  parseField bs = do
    t <- Csv.parseField bs
    let types = t :: Maybe Text 
    case t of
      Nothing -> return $ Right Nothing
      Just t' -> return $ Left (ParsingError "Operator doesn't exist" t')


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
instance Renderable [RawRow ]where render = renderRows classForRaw
instance Renderable [ValidRow] where render = renderRows (const ("" :: Text))

instance Renderable Operator' where
  render (Operator' opId op) = render $ operatorNickname op

classForRaw :: RawRow -> Text                                 
classForRaw raw = case validateRaw (const Nothing) raw of
  Left _ -> "invalid bg-danger"
  Right row -> case validateRow [] NoCheckBarcode row of
    Left _ -> "bg-warning"
    Right _ -> ""

renderRows classFor rows = do
  [whamlet|
<table.table.table-bordered.table-hover>
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
      <tr class=#{classFor row}> ^{render row}
|]



-- | Displays the list of locations
getWHStocktakeLocationR :: Handler Html
getWHStocktakeLocationR = do
  locations <- appStockLocations . appSettings <$> getYesod
  defaultLayout [whamlet|
<h1> Stock locations
  <table.table.table-striped> 
    <tr>
      <th> Name
    $forall location <- locations
      <tr>
        <td> #{location}
|]
