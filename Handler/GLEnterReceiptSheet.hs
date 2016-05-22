{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Handler.GLEnterReceiptSheet where

import Import hiding(InvalidHeader)
import GL.Receipt
import Handler.GLEnterReceiptSheet.ReceiptRow

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import qualified Data.Csv as Csv
import Data.Csv
import Data.Either
import Data.Char (ord)
import Data.Time(parseTimeM)
import Text.Blaze.Html(ToMarkup(toMarkup))
import Text.Printf(printf)
import Data.Conduit.List (consume)
import qualified Data.List.Split  as S
import Text.Printf (printf)
import Data.Ratio (approxRational)
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

       

getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = do
  (postTextFormW, postEncType) <- generateFormPost postTextForm
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  let widget =  [whamlet|
<h1>Enter a receipts spreadsheet
<ul>
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
  defaultLayout $ widget

postTextForm :: Form (Text, Textarea)
postTextForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField "Sheet name" Nothing
  <*> areq textareaField ("Receipts") Nothing


uploadFileForm = renderBootstrap3 BootstrapBasicForm $ (areq fileField ("upload") Nothing )
postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  ((textResp, postTextW), enctype) <- runFormPost postTextForm
  ((fileResp, postFileW), enctype) <- runFormPost uploadFileForm
  spreadSheet <- case (textResp, fileResp) of
                        (FormMissing, FormMissing) -> error "missing"
                        (FormSuccess (title, spreadsheet), _) -> return $ encodeUtf8 $ unTextarea spreadsheet
                        (_, FormSuccess fileInfo) -> do
                          c <- fileSource fileInfo $$ consume
                          return $ concat c
                          
                        _ -> error "Form failure"

  let responseE = case parseReceipts spreadSheet of
                            (Left (Left msg)) -> Left (Left msg)
                            (Right receipts) -> Right (responseW "parsed sucessfully"receipts)
                            (Left (Right receipts)) -> Left $ Right (responseW "parsed unsucessfully" receipts)
      types = spreadSheet :: ByteString
      responseW title receipts = do
        let ns = [1..] :: [Int]
        [whamlet| 
<h1> #title #{tshow title}
<table..table.table-bordered>
  <tr>
    <th>
    <th>Date
    <th>Counterparty
    <th>Bank Account
    <th>Total
    <th>GL Account
    <th>Amount
    <th>Tax Rate
  $forall (receipt, i) <- zip receipts ns
    ^{render (i, receipt)}
|]

  case responseE of
    Left (Left msg) -> setMessage (toHtml msg) >> redirect GLEnterReceiptSheetR
    Left (Right widget) -> do
         setMessage (toHtml $ t "Invalid file")
         sendResponseStatus (toEnum 422) =<< defaultLayout widget
    Right widget -> do
          defaultLayout widget

t = id :: Text -> Text

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
  render (Nothing) = [whamlet||]

instance (Renderable l, Renderable r) => Renderable (Either l r) where
  render (Left r) = [whamlet|<span.left>^{render r}|]
  render (Right r) = [whamlet|<span.right>^{render r}|]

instance Renderable InvalidField where
  render (ParsingError err value) = do
    toWidget [cassius|
.parsing-error
  .description
     display:none
|]
    [whamlet|
<span.parsing-error>
  <span.description>^{render err}
  <span.message.text-danger data-toggle="tooltip" title="^{render err}">^{render value}
|]
    toWidget [julius|
$('[data-toggle="tooltip"]').tooltip();
|]
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  
-- ** To move in app
-- Represents a row of the spreadsheet.
instance Csv.FromNamedRecord (ReceiptRow RawT)where
  parseNamedRecord m = pure ReceiptRow
    <*> (m `parse` "date") -- >>= parseDay)
    -- <*> (textToMaybe <$> m `parse` "counterparty")
    <*> (m `parse` "counterparty")
    <*> m `parse` "bank account"
    <*> (m `parse` "total")

    <*> m `parse` "gl account"
    <*> m `parse` "amount"
    <*> m `parse` "tax rate"
    where parse m colname = do
            t <- m Csv..:colname
            let types = t :: Text
            res <-  toError t <$>  m Csv..: colname
            return $ trace (show (colname, t, res )) res

parseDay bs = do
  str <- parseField bs
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


  
parseReceipts :: ByteString
              -> Either (Either Text
                                [( EitherRow InvalidHeaderT ValidHeaderT
                                , [EitherRow InvalidRowT ValidRowT]
                                )]
                        )
                        [( ReceiptRow ValidHeaderT
                        , [ReceiptRow ValidRowT]
                        )]
parseReceipts bytes = do
  rows <- either (Left . Left) (Right . map analyseReceiptRow) $ parseReceiptRow bytes

  -- split by header, valid or not
  let (orphans:groups) =  S.split (S.keepDelimsL $ S.whenElt  isRight) rows
      -- we know header are right and rows are left

      makeReceipt :: [(Either (EitherRow InvalidRowT ValidRowT)
                              (EitherRow InvalidHeaderT ValidHeaderT)
                      )
                     ]
                  -> ( EitherRow InvalidHeaderT ValidHeaderT
                     , [EitherRow InvalidRowT ValidRowT]
                     )
      makeReceipt (Right header:rows) = (header , headerToRowE header  : lefts rows)
      headerToRowE :: (EitherRow InvalidHeaderT ValidHeaderT) -> (EitherRow InvalidRowT ValidRowT)
      headerToRowE = either (Left . transformRow) (Right . transformRow)
      receipts = map makeReceipt groups

      toMaybe = either (const Nothing) Just
      validReceipt (header, rows) =
        liftA2 (,)
        (toMaybe header)
        (traverse toMaybe (headerToRowE header : rows))
        
        
  case traverse validReceipt receipts of
    Just valids -> Right valids
    Nothing -> Left  . Right $ receipts

      
-- | Parse a csv and return a list of receipt row if possible
parseReceiptRow :: ByteString -> Either Text [ReceiptRow RawT]
parseReceiptRow bytes = either (Left . pack)  (Right . toList)$ do
    (header, vector) <- try
    Right vector
  where
    try = Csv.decodeByNameWith (sep) bytes'
    (sep:_) = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- ",;\t" ]
    bytes' = fromStrict bytes

-- ** to move in general helper or better in App
-- formatAmount :: Amount -> Text
formatAmount = (\t -> t :: String) .  printf "%0.2f" . (\x -> x :: Double) .  fromRational
formatDouble = (\t -> t :: String) .  printf "%0.2f"

