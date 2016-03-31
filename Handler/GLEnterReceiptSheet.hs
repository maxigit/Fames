{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.GLEnterReceiptSheet where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import qualified Data.Csv as Csv
import Data.Csv
import Data.Char (ord)
import Data.Time(parseTimeM)
import Text.Blaze.Html(ToMarkup(toMarkup))
import Text.Printf(printf)
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
         <form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{postEncType}>
            ^{postTextFormW}
            <button type="submit" .btn .btn-default>Process
    <li> Or Upload a document
    Not implemented
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


uploadFileForm = renderBootstrap3 BootstrapBasicForm $ (areq fileField (withSmallInput "upload") Nothing )
postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  ((res, postW), enctype) <- runFormPost postTextForm
  case res of
    FormSuccess (title, text) -> processReceiptSheetR title (unTextarea text)
    _ -> setError "Form empty" >> redirect GLEnterReceiptSheetR

type Amount = Double

data RawReceiptRow = RawReceiptRow
  { rrDate :: Either Text (Maybe Day)
  , rrCompany :: Either Text (Maybe Text)
  , rrBankAccount :: Either Text (Maybe Text)
  , rrComment :: Either Text (Maybe Text)
  , rrTotalAmount :: Either Text (Maybe Amount)
  , rrItemPrice :: Either Text (Maybe Amount)
  , rrItemNet :: Either Text (Maybe Amount)
  , rrItemTaxAmount :: Either Text (Maybe Amount)
  , rrItem :: Either Text (Maybe Text)
  , rrGlAccount :: Either Text (Maybe Text)
  , rrGLDimension1 :: Either Text (Maybe Int)
  , rrGLDimension2 :: Either Text (Maybe Int)
  } deriving (Show, Read)

instance Csv.FromField Day where
  parseField "" = empty
  parseField str = case  concat [parseTimeM True defaultTimeLocale f ("str") | f <- formats] of
    [day] -> pure day
    _ -> mzero
    where
      formats = ["%d"]

instance ToMarkup Day where
  toMarkup d = "TODO"
instance Csv.FromNamedRecord RawReceiptRow where
  parseNamedRecord m = pure RawReceiptRow
    <*> m `parse` "date"
    <*> m `parse` "company"
    <*> m `parse` "bank account"
    <*> m `parse` "comment"
    <*> m `parse` "total price"
    <*> m `parse` "item price"
    <*> m `parse` "item net"
    <*> m `parse` "item tax"
    <*> m `parse` "item"
    <*> m `parse` "gl account"
    <*> m `parse` "dimension 1"
    <*> m `parse` "dimension 2"
    where parse m  colname = do
            e <- m Csv..: colname
            return (case e of 
                    Left bs -> let types = bs :: ByteString in  Left (decodeUtf8 bs)
                    Right r -> Right r
                    )

processReceiptSheetR title text  = do
  case parseReceiptSheet text of
    Left err -> setError (fromString $ "Error encountered" ++ show err) >> redirect GLEnterReceiptSheetR
    Right csv -> defaultLayout $ [whamlet|
<table>
  $forall row <- csv
    <tr>
      <td> ^{withError $ rrDate row}
      <td> ^{withError $ rrCompany row}
      <td> ^{withError $ rrBankAccount row}
      <td> ^{withError $ rrComment row}
      <td> ^{withError $ rrTotalAmount row}
      <td> ^{withError $ map formatF <$> rrItemPrice row}
      <td> ^{withError $ map formatF <$> rrItemNet row}
      <td> ^{withError $ map formatF <$> rrItemTaxAmount row}
      <td> ^{withError $ rrItem row}
      <td> ^{withError $ rrGlAccount row}
      <td> ^{withError $ rrGLDimension1 row}
      <td> ^{withError $ rrGLDimension2 row}
                                         |]
  
withError :: (ToMarkup a) => Either Text (Maybe a) -> Widget
withError (Left e) = toWidget [hamlet|<span style="background:lightred; color:darkred">#{e}|]
withError (Right (Just v)) = [whamlet|<span style="background:lightgreen; color:darkgreen">#{v}|]
withError (Right Nothing) = return () -- [whamlet|<span style="background:green; color:darkgreen">#{c}|]
  
-- formatF :: (Num a) => a -> Text
formatF :: Amount -> Text
formatF f = pack $ printf "%.02f" f

-- Transforms a text into a set of receipts
data RawRow 
-- validates that the text is a table with the correct columns
parseReceiptSheet :: Text -> Either Text [RawReceiptRow]
parseReceiptSheet text = case partitionEithers results  of
  (_, [(header, vector)]) -> Right (toList vector)
  (errs, []) -> Left $ pack ("Invalid format:" ++ show errs)
  (_,rights) -> Left "File ambiguous."
  where
    results = map (`Csv.decodeByNameWith` content) seps
    content = encodeUtf8 (fromStrict text)
    seps = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- ",;\t" ]


data RawReceipt
-- validates that each raw is valid
y :: RawRow -> Either Text [RawReceipt] 
y = undefined

data Receipt
-- groups rows into a set op receipt
z :: [RawReceipt] -> [Receipt]
z  = undefined

data Event
-- transforms a receipt into an Event
t :: Receipt -> Event
t = undefined
