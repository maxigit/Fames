{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Handler.GL.GLEnterReceiptSheet where

import Import hiding(InvalidHeader)
-- import GL.Receipt
import Handler.GL.GLEnterReceiptSheet.ReceiptRow

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.CsvUtils hiding(RawT)
import qualified Data.Csv as Csv
import Data.Either
import Text.Blaze.Html(ToMarkup())
import qualified Data.List.Split  as S
-- import Text.Printf (printf)
-- import qualified Data.Text as Text
-- import qualified Data.Text.Read as Text
-- import Data.Ratio (approxRational)
-- import qualified Data.ByteString as BL
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
  (uploadFileFormW, upEncType) <- generateFormPost $ uploadFileForm (pure ())
  setMessage (toHtml title)
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
<h1>Enter a receipts spreadsheet
<ul>
   <li #gl-enter-receipt-sheet-pre> ^{pre}
   <li>
     <form #text-form role=form method=post action=@{GLR GLEnterReceiptSheetR} enctype=#{postEncType}>
         ^{postTextFormW}
          <button type="submit" .btn .btn-default>Process
   <li> Or Upload a document
      <form #upload-form role=form method=post action=@{GLR GLEnterReceiptSheetR} enctype=#{upEncType}>
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
  ((textResp, __postTextW), __enctype) <- runFormPost postTextForm
  ((fileResp, __postFileW), ___enctype) <- runFormPost $ uploadFileForm (pure ())
  let showForm form = case form of
                      (FormFailure f) -> "Form Failure:" ++ show f
                      (FormMissing) -> "Form missing"
                      (FormSuccess _) -> "Form Success"

  spreadSheet <- case (textResp, fileResp) of
                        -- (FormMissing, FormMissing) -> error "missing"
                        (FormSuccess (__title, spreadsheet), _) -> return $ encodeUtf8 $ unTextarea spreadsheet
                        (_, FormSuccess (fileInfo, encoding, ())) -> do
                          fst <$> readUploadUTF8 fileInfo encoding
                        (formA, formB) -> error $ showForm formA ++ ", " ++ showForm formB
 
  either id defaultLayout $ do
    rawRows <- parseSpreadsheet columnMap Nothing spreadSheet <|&>  renderGLEnterReceiptSheet 422 "Invalid file or columns missing." . render
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

t :: Text -> Text
t = id

setMessage' :: (ToMarkup a, MonadHandler m) => a -> m ()
setMessage' msg = {-trace ("set message : " ++ show msg)-} (setMessage $ toHtml msg)
-- ** Widgets
instance Renderable (ReceiptRow 'ValidRowT) where render = renderReceiptRow ValidRowT
instance Renderable (ReceiptRow 'InvalidRowT) where render = renderReceiptRow InvalidRowT

renderReceiptRow :: (Renderable (HeaderFieldTF t Text),
                     Renderable (HeaderFieldTF t Double),
                     Renderable (RowFieldTF t (Maybe Int)),
                     Renderable (RowFieldTF t (Maybe Double)),
                     Renderable (RowFieldTF t (Maybe Text)))
                 => ReceiptRowType -> ReceiptRow t -> Widget
renderReceiptRow rowType_ ReceiptRow{..}= do
  let groupIndicator ValidHeaderT = ">" :: Text
      groupIndicator InvalidHeaderT = ">"
      groupIndicator _ = ""
  toWidget [cassius|
.receipt-header
  font-size: 1.2 em
  font-weight: bold
|]
  [whamlet|
<td.receiptGroup>#{groupIndicator rowType_}
<td.date>^{render rowDate}
<td.counterparty>^{render rowCounterparty}
<td.bankAccount>^{render rowBankAccount}
<td.totalAmount>^{render rowTotal}
<td.glAccount>^{render rowGlAccount}
<td.amount>^{render rowAmount}
<td.tax>^{render rowTax}
|]

instance Renderable (ReceiptRow 'ValidHeaderT) where render = renderReceiptRow ValidHeaderT
instance Renderable (ReceiptRow 'InvalidHeaderT) where render = renderReceiptRow InvalidHeaderT


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
         class_ RawT = error "Shouldn't happend"
-- <span.rowTax>#{render rowTax}

columnMap :: Map String [String]
columnMap = Map.fromList
  [ (col, concatMap expandColumnName (col:cols)  )
  | (col, cols) <-
    [ ("date", [])
    , ("counterparty", ["company"])
    , ("bank account", ["bank"])
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
instance Csv.FromNamedRecord (ReceiptRow 'RawT)where
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
    where parse = parseMulti columnMap

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

      go (Right header:rows__) = (header , lefts rows__)
      go _ = error "Shouldn't not happend"

      headerToRowE :: (Either InvalidHeader ValidHeader) -> (Either InvalidRow ValidRow)
      headerToRowE = either (Left . transformRow) (Right . transformRow)

      receipts = if null orphans then map go groups else error "orphans"

      toMaybe = either (const Nothing) Just
      -- to valid a 'receipt' we traverse all of its constituent rows
      validReceipt (header, rows2) =
        liftA2 (,)
        (toMaybe header)
        (traverse toMaybe (headerToRowE header : rows2))
        
        
  in case traverse validReceipt receipts of
    Just valids -> Right valids
    Nothing -> Left receipts
