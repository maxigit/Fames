{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.GLEnterReceiptSheet where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Data.Conduit.List (consume)
import qualified Data.Csv as Csv
import Data.Char (ord)
import Data.Time(parseTimeM)
import Data.Encoding
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
    _ -> setMessage "Form empty" >> redirect GLEnterReceiptSheetR

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
  }

instance Csv.FromField Day where
  parseField "" = empty
  parseField str = case  concat [parseTimeM True defaultTimeLocale f ("str") | f <- formats] of
    [day] -> pure day
    _ -> mzero
    where
      formats = ["%d"]
instance Csv.FromNamedRecord RawReceiptRow where
  parseNamedRecord m = pure RawReceiptRow
    <*> m Csv..: "Date"
    <*> m Csv..: "Company"
    <*> m Csv..: "Bank Account"
    <*> m Csv..: "Comment"
    <*> m Csv..: "Total Price"
    <*> m Csv..: "Item Price"
    <*> m Csv..: "Item Net"
    <*> m Csv..: "Item Tax"
    <*> m Csv..: "Item"
    <*> m Csv..: "GL Account"
    <*> m Csv..: "Dimension 1"
    <*> m Csv..: "Dimension 2"

processReceiptSheetR title text  = do
  case x text of
    Left err -> setMessage (fromString $ show err) >> redirect GLEnterReceiptSheetR
    Right _ -> defaultLayout $ [whamlet|hello|]
  
  
-- Transforms a text into a set of receipts
data RawRow 
-- validates that the text is a table with the correct columns
x :: Text -> Either Text [RawReceiptRow]
x text = case asum results  of
  Left msg -> Left (pack msg)
  Right (header, vector) -> Right (toList vector)
  where
    results = map (`Csv.decodeByNameWith` content) seps
    content = encode text
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
