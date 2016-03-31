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

-- Convenience functions to help decides of string-like types
s :: String -> String
s =  id

t :: Text -> Text  
t = id 

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
  parseField bs = parseField bs >>= \str ->  case  concat [parseTimeM True defaultTimeLocale f str | f <- formats] of
    [day] -> pure day
    _ -> mzero
    where
      formats = ["%d/%m/%y"] ++  [ "%" ++ show day ++ show sep ++ "%" ++ show month ++ show  sep ++ "%" ++ show year
                | day <- s"de"
                , month <- s"mb"
                , year <- s"yY"
                , sep <- s"-/ "
                ]


instance ToMarkup Day where
  toMarkup day = toMarkup $ formatTime defaultTimeLocale "%a %d %b %Y" day
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
    Right csv -> defaultLayout (renderRawReceiptRows csv)


renderRawReceiptRows rows = [whamlet|
<table .table .table-striped .table-hover>
  <tr>
    <th>date
    <th>company
    <th>bank account
    <th>comment
    <th>total price
    <th>item price
    <th>item net
    <th>item tax
    <th>item
    <th>gl account
    <th>dimension 1
    <th>dimension 2
  $forall row <- rows
    <tr class=#{rowStatus row} >
      ^{tdWithError $ rrDate row}
      ^{tdWithError $ rrCompany row}
      ^{tdWithError $ rrBankAccount row}
      ^{tdWithError $ rrComment row}
      ^{tdWithError $ rrTotalAmount row}
      ^{tdWithError $ map formatF <$> rrItemPrice row}
      ^{tdWithError $ map formatF <$> rrItemNet row}
      ^{tdWithError $ map formatF <$> rrItemTaxAmount row}
      ^{tdWithError $ rrItem row}
      ^{tdWithError $ rrGlAccount row}
      ^{tdWithError $ rrGLDimension1 row}
      ^{tdWithError $ rrGLDimension2 row}
|]
    
tdWithError :: (ToMarkup a) => Either Text (Maybe a) -> Widget
tdWithError (Left e) = toWidget [hamlet|
<td>
  <span .text-danger .bg-danger>#{e}
|]
tdWithError (Right (Just v)) = toWidget [hamlet|
<td>
  <span .bg-success>#{v}
|]
tdWithError (Right Nothing) = [whamlet|
<td>
 <  span .bg-warnig>
|]
  

rowStatus :: RawReceiptRow -> Text
rowStatus raw = case validateRawRow raw  of
  Left _ -> "danger"
  Right row -> case rowFilled row of
    Nothing -> "warning"
    Just _ ->  "succes"

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


data ReceiptRow = ReceiptRow
  { rDate :: (Maybe Day)
  , rCompany :: (Maybe Text)
  , rBankAccount :: (Maybe Text)
  , rComment :: (Maybe Text)
  , rTotalAmount :: (Maybe Amount)
  , rItemPrice :: (Maybe Amount)
  , rItemNet :: (Maybe Amount)
  , rItemTaxAmount :: (Maybe Amount)
  , rItem :: (Maybe Text)
  , rGLAccount :: (Maybe Text)
  , rGLDimension1 :: (Maybe Int)
  , rGLDimension2 :: (Maybe Int)
  } deriving (Show, Read)

-- validates that eack raw is valid
validateRawRow :: RawReceiptRow -> Either RawReceiptRow ReceiptRow
validateRawRow raw = let row = pure ReceiptRow
                              <*> rrDate raw
                              <*> rrCompany raw
                              <*> rrBankAccount raw
                              <*> rrComment raw
                              <*> rrTotalAmount raw
                              <*> rrItemPrice raw
                              <*> rrItemNet raw
                              <*> rrItemTaxAmount raw
                              <*> rrItem raw
                              <*> rrGlAccount raw
                              <*> rrGLDimension1 raw
                              <*> rrGLDimension2 raw
                    in case row of
                            Left _ -> Left raw
                            Right r -> Right r
 
validateRawRows :: [RawReceiptRow] -> Either [RawReceiptRow] [ReceiptRow]
validateRawRows raws = case traverse validateRawRow raws of
  Left _ -> Left raws
  Right  rows -> Right rows


  -- Check if enough fields are filled
rowFilled :: ReceiptRow -> Maybe ReceiptRow
rowFilled row = do
  asum [ rDate row >> return ()
       , rCompany row >> return ()
       , rBankAccount row >> return ()
       --- , rcomment row
       , rTotalAmount row >> return ()
       , rItemPrice row >> return ()
       , rItemNet row >> return ()
       , rItemTaxAmount row >> return ()
       , rItem row >> return ()
       , rGLAccount row >> return ()
       -- , rgldimension1 row
       -- , rgldimension2 row
       ]

  return row
                  
data Receipt
-- groups rows into a set op receipt
z :: [ReceiptRow] -> [Receipt]
z  = undefined

data Event
-- transforms a receipt into an Event
t' :: Receipt -> Event
t' = undefined
