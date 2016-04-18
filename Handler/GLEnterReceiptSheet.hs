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
                          receipts <-  parseReceipts (encodeUtf8 $ unTextarea spreadsheet)
                          let ns = [1..]
                          Right [whamlet| |]
-- <h1> #title parsed successfully
-- <ul>
--   $forall receiptI <- zip receipts ns
--     $case receiptI
--       $of (Right receipt, i) 
--         <li .receipt id=receipt#{tshow i}>
--           <span.date>#{date receipt}
--           <span.counterparty>#{counterparty receipt}
--           <span.bankAccount>#{bankAccount receipt}
--           <span.totalAmount>#{formatAmount $ totalAmount receipt}
--           <ul>
--             $forall item <- items receipt
--               <li>
--                 <span.glAccount>#{glAccount item}
--                 <span.amount>#{formatAmount $ amount item}
--                 <span.taxTyple>#{tshow $ taxType item}
--       $of (Left (ReceiptRow'{..}), i) 
--         <li .invalidRow id=invalidRow#{tshow i}>
--                 <span.rowGlAccount>#{rowGlAccount}
--                 <span.rowAmount>#{tshow rowAmount}
--                 <span.rowTax>#{tshow rowTax}
--               |]
  case responseE of
    Left (Left msg) -> setMessage (toHtml msg) >> redirect GLEnterReceiptSheetR
    Right widget -> defaultLayout $ widget



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
      makeReceipt (Right header:rows) = (header, lefts rows)
      receipts = map makeReceipt groups

      toMaybe = either (const Nothing) Just
      validReceipt (header, rows) =
        liftA2 (,)
        (toMaybe header)
        (traverse toMaybe rows)
        
        
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

