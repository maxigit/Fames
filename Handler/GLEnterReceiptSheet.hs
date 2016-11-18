{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Handler.GLEnterReceiptSheet where

import Import hiding(InvalidHeader)
import GL.Receipt
import Handler.GLEnterReceiptSheet.ReceiptRow

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import qualified Data.Csv as Csv
-- import Data.Csv hi
import Data.Either
import Data.Char (ord,toUpper)
import Data.Time(parseTimeM)
import Text.Blaze.Html(ToMarkup(toMarkup))
import Text.Printf(printf)
import qualified Data.List.Split  as S
import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Data.Ratio (approxRational)
import qualified Data.ByteString as BL
import qualified Data.Map as Map
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

-- | process GET for GLEnterReceiptSheetR route
-- used to upload a receip sheet.
getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = renderGLEnterReceiptSheet 200 ""  (return ())

-- | Render a page with a form to upload a receipt sheet
-- via text or file. It can also display the content of the previous attempt if any.
renderGLEnterReceiptSheet :: Int -> Text -> Widget -> Handler Html
renderGLEnterReceiptSheet status title pre = do
  (postTextFormW, postEncType) <- generateFormPost postTextForm
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  setMessage (toHtml title)
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
<h1>Enter a receipts spreadsheet
<ul>
   <li #gl-enter-receipt-sheet-pre> ^{pre}
   <li>
     <form #text-form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{postEncType}>
         ^{postTextFormW}
          <button type="submit" .btn .btn-default>Process
   <li> Or Upload a document
      <form #upload-form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{upEncType}>
         ^{uploadFileFormW}
         <button type="submit" .btn .btn-default>Process
   <li> Or use an existing document
        Not implemented
   <li> Or download a spreadsheet template
        Not implemented
|]

postTextForm :: Form (Text, Textarea)
postTextForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField "Sheet name" Nothing
  <*> areq textareaField "Receipts" Nothing


postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  ((textResp, postTextW), enctype) <- runFormPost postTextForm
  ((fileResp, postFileW), enctype) <- runFormPost uploadFileForm
  spreadSheet <- case (textResp, fileResp) of
                        (FormMissing, FormMissing) -> error "missing"
                        (FormSuccess (title, spreadsheet), _) -> return $ encodeUtf8 $ unTextarea spreadsheet
                        (_, FormSuccess (fileInfo, encoding)) -> do
                          readUploadUTF8 fileInfo encoding
                          
                        (FormFailure a,FormFailure b) -> error $ "Form failure : " ++  show a ++ ", " ++ show b
  either id defaultLayout $ do
    rawRows <- parseReceiptRow spreadSheet <|&>  renderGLEnterReceiptSheet 422 "Invalid file or columns missing." . render
    let receiptRows = map analyseReceiptRow rawRows
    receipts <- makeReceipt receiptRows <|&> renderGLEnterReceiptSheet  422 "Invalid cell format." . renderReceiptSheet
    return $ renderReceiptSheet receipts


renderReceiptSheet :: ( ReceiptRowTypeClass h, Renderable h
                      , ReceiptRowTypeClass r, Renderable r
                      , Show h, Show r) => [(h, [r])] -> Widget
renderReceiptSheet receipts =  do
  let ns = [1..] :: [Int]
  [whamlet| 
<table..table.table-bordered>
  <tr>
    <th>
    <th>Date
    <th>Counterparty
    <th>Bank Account
    <th>Comment
    <th>Totalparse
    <th>GL Account
    <th>Amount
    <th>Net Amount
    <th>Memo
    <th>Tax Rate
    <th>Dimension 1
    <th>Dimension 2
  $forall (receipt, i) <- zip receipts ns
    ^{render (i, receipt)}
|]

t = id :: Text -> Text

setMessage' msg = {-trace ("set message : " ++ show msg)-} (setMessage $ toHtml msg)
-- ** Widgets
instance Renderable (ReceiptRow ValidRowT) where render = renderReceiptRow ValidRowT
instance Renderable (ReceiptRow InvalidRowT) where render = renderReceiptRow InvalidRowT

-- renderReceipt :: (ReceiptRow header, [ReceiptRow row]) -> Widget
renderReceiptRow rowType ReceiptRow{..}= do
  let groupIndicator ValidHeaderT = ">" :: Text
      groupIndicator InvalidHeaderT = ">"
      groupIndicator _ = ""
  toWidget [cassius|
.receipt-header
  font-size: 1.2 em
  font-weight: bold
|]
  [whamlet|
<td.receiptGroup>#{groupIndicator rowType}
<td.date>^{render rowDate}
<td.counterparty>^{render rowCounterparty}
<td.bankAccount>^{render rowBankAccount}
<td.totalAmount>^{render rowTotal}
<td.glAccount>^{render rowGlAccount}
<td.amount>^{render rowAmount}
<td.tax>^{render rowTax}
|]

instance Renderable (ReceiptRow ValidHeaderT) where render = renderReceiptRow ValidHeaderT
instance Renderable (ReceiptRow InvalidHeaderT) where render = renderReceiptRow InvalidHeaderT


instance ( ReceiptRowTypeClass h, Renderable h
         , ReceiptRowTypeClass r, Renderable r
         , Show h, Show r) => Renderable (Int, (h, [r])) where
  render (i, (header, rows)) = [whamlet|
<tr class="#{class_ (rowType header)}" id="receipt#{i}">
  ^{render header}
$forall (row, j) <- zip rows is
  <tr class="#{class_ (rowType row)}" id="receipt#{i}-#{j}">
      ^{render row}
|] where is = [1..] :: [Int]
         class_ ValidHeaderT = "receipt-header valid bg-info" :: Text
         class_ InvalidHeaderT = "receipt-header invalid bg-danger"
         class_ ValidRowT = "receipt-row valid"
         class_ InvalidRowT = "receipt-row invalid bg-warning"
-- <span.rowTax>#{render rowTax}

class Renderable r where
  render :: r -> Widget

instance Renderable () where
  render () = return ()
instance Renderable Int where
  render i = [whamlet|#{tshow i}|]

instance Renderable Double where
  render d = [whamlet|#{formatDouble d}|]

instance Renderable Text where
  render t = [whamlet|#{t}|]

instance Renderable Day where
instance (Renderable r) => Renderable (Maybe r) where
  render (Just x) = render x
  render Nothing = [whamlet||]

instance (Renderable l, Renderable r) => Renderable (Either l r) where
  render (Left r) = [whamlet|<span.left>^{render r}|]
  render (Right r) = [whamlet|<span.right>^{render r}|]

instance Renderable InvalidField where
  render invField = do
    let (class_, value) = case invField of
          ParsingError _ v -> ("parsing-error" :: Text, v)
          MissingValueError _ -> ("missing-value" :: Text,"<Empty>")
    toWidget [cassius|
.parsing-error, .missing-value
  .description
     display:none
.missing-value
  .message
    font-style: italic
|]
    [whamlet|
<span class="#{class_}">
  <span.description>#{invalidFieldError invField}
  <span.message.text-danger data-toggle="tooltip" title="#{invalidFieldError invField}">#{value}
|]
    toWidget [julius|
$('[data-toggle="tooltip"]').tooltip();
|]
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  

infixl 4 <$$$>, <$$>
(<$$$>) = fmap . fmap . fmap
(<$$>) = fmap . fmap . fmap
-- ** To move in app
columnMap :: Map String [String]
columnMap = Map.fromList
  [ (col, concatMap expandColumnName (col:cols)  )
  | (col, cols) <-
    [ ("date", [])
    , ("counterparty", ["company"])
    , ("bank account", ["account"])
    , ("comment", [])
    , ("total", ["total price"])
    , ("gl account", ["account"])
    , ("amount", ["item price"])
    , ("net amount", ["net", "net item"])
    , ("memo" , ["item"])
    , ("tax rate" , ["vat"])
    , ("dimension 1", ["dim1", "dimension1"])
    , ("dimension 2", ["dim2", "dimension2"])
    ]
  ]
                          
-- Represents a row of the spreadsheet.
instance Csv.FromNamedRecord (ReceiptRow RawT)where
  parseNamedRecord m = pure ReceiptRow
    <*> m `parse` "date"
    <*> m `parse` "counterparty" 
    <*> m `parse` "bank account"
    <*> m `parse` "comment"
    <*> (unCurrency <$$$> m  `parse` "total" )

    <*> m `parse` "gl account"
    <*> (unCurrency <$$$> m `parse` "amount" )
    <*> (unCurrency <$$$> m `parse` "net amount" )
    <*> m `parse` "memo" 
    <*> m `parse` "tax rate" 
    <*> m `parse` "dimension 1"
    <*> m `parse` "dimension 2"
    where parse m colname = do
            -- try colname, Colname and COLNAME
            let Just colnames'' = (Map.lookup colname columnMap)
                colnames = fromString <$> colnames''
                mts = map (m Csv..:) colnames
                mts' = asum $ map (m Csv..:) colnames
            -- let types = mts :: [Csv.Parser Text]
            t <- asum mts
            res <-  toError t <$>  mts'
            -- return $ trace (show (colname, t, res )) res
            return res

expandColumnName :: String -> [String]
expandColumnName colname = [id, capitalize, map Data.Char.toUpper] <*> [colname]

capitalize [] = []
capitalize (x:xs) = Data.Char.toUpper x : xs

-- | temporary class to remove currency symbol
newtype Currency = Currency {unCurrency :: Double} deriving (Show, Eq, Num, Fractional)
instance Csv.FromField Currency where
  parseField bs = do
    case stripPrefix "-" bs of
      Just bs' -> negate <$> Csv.parseField bs'
      Nothing -> do
        let stripped = bs `fromMaybe` stripPrefix (encodeUtf8 "£") bs
            res = Currency <$> Csv.parseField stripped
        -- trace ("Currency: " ++ show (bs, stripped)) res
        res
      
parseDay bs = do
  str <- Csv.parseField bs
  case  concat [parseTimeM True defaultTimeLocale f str | f <- formats] of
    [day] -> pure day
    (d:ds) -> error (show (d:ds))
    _ -> mzero
  where
      -- formats to try. Normally there shouldn't be any overlap bettween the different format.
      -- The 0 in %0Y is important. It guarantes that only 4 digits are accepted.
      -- without 11/01/01 will be parsed by %Y/%m/%d and %d/%m/%y
      formats = [ "%0Y/%m/%d"
                , "%d/%m/%0Y"
                , "%d/%m/%y"
                , "%0Y-%m-%d"
                , "%d %b %0Y"
                , "%d-%b-%0Y"
                , "%d %b %y"
                , "%d-%b-%y"
                , "%0Y %b %d"
                , "%0Y-%b-%d"
                , "%a %d %b %0Y"
                ]

toError :: Text -> Either Csv.Field a -> Either InvalidField a
toError t e = case e of
  Left err -> Left (ParsingError ("Invalid format") t)
  Right v -> Right v


 {-
parseReceipts :: ByteString
              -> Either (Either InvalidReceiptSheet
                                [( Either InvalidHeader ValidHeader
                                , [Either InvalidRow ValidRow]
                                )]
                        )
                        [ (ValidHeader
                          , [ValidRow]
                          )
                        ]
parseReceipts bytes = do
  rows <- either (Left . Left) (Right . map analyseReceiptRow) $ parseReceiptRow bytes

-}
-- | Regroups receipts rows starting with a header (valid or invalid)
makeReceipt :: [Either (Either InvalidRow ValidRow)
                       (Either InvalidHeader ValidHeader)
               ]
             -> Either [( Either InvalidHeader ValidHeader
                         , [Either InvalidRow ValidRow]
                         )]
                       [ (ValidHeader
                          , [ValidRow]
                          )
                       ]
makeReceipt [] = Left []
makeReceipt rows = 
  -- split by header, valid or not
  let (orphans:groups) =  S.split (S.keepDelimsL $ S.whenElt  isRight) rows
      -- we know header are right and rows are left

      go (Right header:rows) = (header , lefts rows)

      headerToRowE :: (Either InvalidHeader ValidHeader) -> (Either InvalidRow ValidRow)
      headerToRowE = either (Left . transformRow) (Right . transformRow)

      receipts = if null orphans then map go groups else error "orphans"

      toMaybe = either (const Nothing) Just
      -- to valid a 'receipt' we traverse all of its constituent rows
      validReceipt (header, rows) =
        liftA2 (,)
        (toMaybe header)
        (traverse toMaybe (headerToRowE header : rows))
        
        
  in case traverse validReceipt receipts of
    Just valids -> Right valids
    Nothing -> Left receipts

-- | If we can't parse the csv at all (columns are not present),
-- we need a way to gracefully return a error
data InvalidReceiptSheet = InvalidReceiptSheet
  { errorDescription :: Text
  , missingColumns :: [Text]
  , columnIndexes :: [Int] -- ^ index of present columns
  , sheet :: [[ Either Csv.Field Text]]  -- ^ origin file
  } deriving Show
  {-
  | InvalidFileFormat
  { errMessage :: Text
  , missingColumns :: [Text]
  }
-}


      
-- | Parse a csv and return a list of receipt row if possible
parseReceiptRow :: ByteString -> Either InvalidReceiptSheet [RawRow]
parseReceiptRow bytes = either (Left . parseInvalidReceiptSheet bytes')  (Right . toList)$ do
    (header, vector) <- try
    Right vector
  where
    try = Csv.decodeByNameWith (sep) bytes'
    -- try = trace ("decoded: " ++ show try') try'
    (sep:_) = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- ",;\t" ]
    bytes' = fromStrict bytes

parseInvalidReceiptSheet bytes err =
  let columns = fromString <$> keys columnMap 
      decoded = toList <$> Csv.decode Csv.NoHeader bytes :: Either String [[Either Csv.Field Text]]
      onEmpty = InvalidReceiptSheet "The file is empty" columns [] []
  in case (null bytes, decoded) of
       (True, _ )  -> onEmpty
       (_, Right [])  -> onEmpty
       (False, Left err) -> InvalidReceiptSheet "Can't parse file. Please check the file is encoded in UTF8 or is well formatted." columns [] [[Left (toStrict bytes)]]
       (False, Right sheet@(headerE:_)) -> do
         let headerPos = Map.fromList (zip header [0..]) :: Map Text Int
             header = map (either decodeUtf8 id) headerE
             -- index of a given column in the current header. Starts a 0.
             indexes = [ (fromString col
                         , asum [ lookup (fromString col') headerPos
                                | col' <- cols
                                ]
                         )
                       | (col, cols)  <- Map.toList columnMap
                       ]
             missingColumns = [col | (col, Nothing) <- indexes]
             columnIndexes = catMaybes (map snd indexes)
             errorDescription = case traverse sequence sheet of
                                     Left _ -> "Encoding is wrong. Please make sure the file is in UTF8"
                                     Right _ -> tshow err
         InvalidReceiptSheet{..}
    
-- ** to move in general helper or better in App
-- formatAmount :: Amount -> Text
formatAmount = (\t -> t :: String) .  printf "" . (\x -> x :: Double) .  fromRational
formatDouble = (\t -> t :: String) .  printf "%0.2f"


instance Renderable InvalidReceiptSheet where
  render i@InvalidReceiptSheet{..} = let
    colClass = go 0 (sort columnIndexes)
    go :: Int -> [Int]-> [Text]
    go i [] = "" : go i []
    go i (ix:ixs) | ix == i = "bg-success" : go (i+1) ixs
                  | otherwise = "" : go (i+1) (ix:ixs)
    convertField :: Either Csv.Field Text -> (Text, Text)
    convertField (Left bs) = ("bg-danger text-danger", decodeUtf8 bs)
    convertField (Right t) = ("", t)
    in  {-trace ("toHtml" ++ show i ) -}[whamlet|
<div .invalid-receipt>
  <div .error-description> #{errorDescription}
  $if not (null missingColumns)
    <div .missing-columns .bg-danger .text-danger>
      The following columns are missing:
      <ul>
        $forall column <- missingColumns
          <li> #{column}
  <table.sheet.table.table-bordered>
    $forall line <- sheet
      <tr>
        $forall (class_, field) <- zip colClass line
          $with (fieldClass, fieldValue) <- convertField field
            <td class="#{class_} #{fieldClass}"> #{fieldValue}
          |]
           
             

