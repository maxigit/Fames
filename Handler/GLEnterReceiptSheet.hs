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
  ((res, postW), enctype) <- runFormPost postTextForm
  let responseE = case res of
                        FormSuccess (title, spreadsheet) -> do -- Either
                          let receiptsE =  parseReceipts (encodeUtf8 $ unTextarea spreadsheet)
                          let ns = [1..]
                          case receiptsE of
                            (Left (Left msg)) -> Left msg
                            _ -> Right [whamlet|
<h1> #title parsed successfully
<ul>
$case receiptsE
  $of (Right receipts)
    <div> #{tshow receipts}
    $forall (receipt, i) <- zip receipts ns
      <ul>
        <li .valid #receipt#{tshow i}> ^{render receipt}
  $of (Left (Right receipts))
    $forall (receipt, i) <- zip receipts ns
      <ul .invalid>
        <li .invalid #receipt#{tshow i}> ^{render receipt}
|]
  case responseE of
    Left msg -> setMessage (toHtml msg) >> redirect GLEnterReceiptSheetR
    Right widget -> defaultLayout $ widget


-- ** Widgets
renderReceiptRow ReceiptRow{..} = [whamlet|
<span.glAccount>^{render rowGlAccount}
<span.amount>^{render rowAmount}
|]

instance Renderable (ReceiptRow ValidRowT) where render = renderReceiptRow
instance Renderable (ReceiptRow InvalidRowT) where render = renderReceiptRow

-- renderReceipt :: (ReceiptRow header, [ReceiptRow row]) -> Widget
renderReceiptHeader header= [whamlet|
<span.date>^{render $ rowDate header}
<span.counterparty>^{render $ rowCounterparty header}
<span.bankAccount>^{render $ rowBankAccount header}
<span.totalAmount>^{render $ rowTotal header}
|]

instance Renderable (ReceiptRow ValidHeaderT) where render = renderReceiptHeader
instance Renderable (ReceiptRow InvalidHeaderT) where render = renderReceiptHeader


instance (Renderable h, Renderable r, Show h, Show r) => Renderable (h, [r]) where
  render (header, rows) = [whamlet|
^{render header}
<ul>
  $forall (row, i) <- zip rows is
    <li .row id=row#{tshow i}> ^{render row}
|] where is = [1..]
-- <span.rowTax>#{render rowTax}

class Renderable r where
  render :: r -> Widget

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
  render (Left r) = [whamlet|<span.invalid>^{render r}|]
  render (Right r) = [whamlet|<span.valid>^{render r}|]

-- ** To move in app
-- Represents a row of the spreadsheet.
instance Csv.FromNamedRecord (ReceiptRow Raw)where
  parseNamedRecord m = pure ReceiptRow
    <*> (m `parse` "date") -- >>= parseDay)
    <*> m `parse` "counterparty"
    <*> m `parse` "bank account"
    <*> m `parse` "total"

    <*> m `parse` "gl account"
    <*> m `parse` "amount"
    <*> m `parse` "tax rate"
    where parse m colname = m Csv..: colname

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
      makeReceipt (Right header:rows) = (header , headerToRowE header  : lefts rows)
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
parseReceiptRow :: ByteString -> Either Text [ReceiptRow Raw]
parseReceiptRow bytes = either (Left . pack)  (Right . toList)$ do
    (header, vector) <- try
    Right vector
  where
    try = Csv.decodeByNameWith (sep) bytes'
    (sep:_) = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- ",;\t" ]
    bytes' = fromStrict bytes

-- ** to move in general helper or better in App
-- formatAmount :: Amount -> Text
formatAmount = tshow . (\t -> t :: String) .  printf "%0.2f" . (\x -> x :: Double) .  fromRational
formatDouble = tshow . (\t -> t :: String) .  printf "%0.2f"

