{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Handler.GL.GLEnterReceiptSheet where

import Import hiding(InvalidHeader)
-- import GL.Receipt
import Handler.GL.GLEnterReceiptSheet.ReceiptRow

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.CsvUtils
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
import Lens.Micro.Extras (preview)
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

-- | process GET for GLEnterReceiptSheetR route
-- used to upload a receip sheet.
getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = renderGLEnterReceiptSheet Nothing 200 ""  (return ())

-- | Render a page with a form to upload a receipt sheet
-- via text or file. It can also display the content of the previous attempt if any.
renderGLEnterReceiptSheet :: Maybe Param -> Int -> Text -> Widget -> Handler Html
renderGLEnterReceiptSheet param status title pre = do
  (postTextFormW, postEncType) <- generateFormPost (postTextForm param)
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

type Param = (Text, Textarea)   
postTextForm :: Maybe Param -> Form Param
postTextForm paramM = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField "Sheet name" (fst <$> paramM )
  <*> areq textareaField "Receipts" (snd <$> paramM)


postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  ((textResp, __postTextW), __enctype) <- runFormPost (postTextForm Nothing)
  ((fileResp, __postFileW), ___enctype) <- runFormPost $ uploadFileForm (pure ())
  let showForm form = case form of
                      (FormFailure f) -> "Form Failure:" ++ show f
                      (FormMissing) -> "Form missing"
                      (FormSuccess _) -> "Form Success"

  (spreadSheet, param) <- case (textResp, fileResp) of
                        -- (FormMissing, FormMissing) -> error "missing"
                        (FormSuccess param@(__title, spreadsheet), _) -> return $ (encodeUtf8 $ unTextarea spreadsheet, Just param)
                        (_, FormSuccess (fileInfo, encoding, ())) -> do
                          (, Nothing) . fst <$> readUploadUTF8 fileInfo encoding
                        (formA, formB) -> error $ showForm formA ++ ", " ++ showForm formB
 
  let receipts = parseGL spreadSheet 
  renderParsingResult ((\_ pre -> setError "Invalid Spreadsheet" >> renderGLEnterReceiptSheet param 422 "" pre ) :: Handler () -> Widget -> Handler Html)
                      (defaultLayout  . renderReceiptSheet)
                      receipts

    -- rawRows <- parseSpreadsheet columnMap Nothing spreadSheet <|&>  renderGLEnterReceiptSheet 422 "Invalid file or columns missing." . render
    -- let receiptRowsE = map analyseReceiptRow rawRows

    -- case sequence receiptRowsE of
    --   Left _ -> renderGLEnterReceiptSheet 422 "Invalid cell format." (render receiptRowsE)
    -- receipts <- makeReceipt receiptRows <|&> renderGLEnterReceiptSheet  422 "Invalid cell format." . renderReceiptSheet
    -- return $ renderReceiptSheet receipts


parseGL :: ByteString -> ParsingResult RawRow [(PartialHeader, [PartialItem])]
parseGL spreadSheet = either id ParsingCorrect $ do
  rawRows <- parseSpreadsheet columnMap Nothing spreadSheet <|&>  WrongHeader
  partials <- getLefts (map analyseReceiptRow rawRows) <|&>  InvalidFormat 
  receipts <- makeReceipt partials <|&> InvalidData ["First row is missing header"] [] . map transformRow
  Right receipts

getLefts es = case partitionEithers es of
                      ([], rights ) -> Right rights
                      (lefts, _) -> Left lefts
   


renderReceiptSheet :: ( Renderable h
                      , Renderable r
                      , Renderable (Int, (h, [r]))
                      )
                      => [(h, [r] )] -> Widget
  
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
instance (
                     -- Renderable (HeaderFieldTF t Text),
                     -- Renderable (HeaderFieldTF t Double),
                     -- Renderable (RowFieldTF t (Maybe Int)),
                     -- Renderable (RowFieldTF t (Maybe Double)),
                     -- Renderable (RowFieldTF t (Maybe Text)) ,
                     Renderable (ReceiptHeader t) ,
                     Renderable (ReceiptItem t) 
           ) => Renderable (ReceiptRow t)
    where render r = renderReceiptRow  r

renderReceiptRow :: (
                     -- Renderable (HeaderFieldTF t Text),
                     -- Renderable (HeaderFieldTF t Double),
                     -- Renderable (RowFieldTF t (Maybe Int)),
                     -- Renderable (RowFieldTF t (Maybe Double)),
                     -- Renderable (RowFieldTF t (Maybe Text)) ,
                     Renderable (ReceiptHeader t)
                    ,Renderable (ReceiptItem t)
                    )
                 => ReceiptRow t -> Widget
renderReceiptRow row = do
  let groupIndicator | isThat row = ""  :: Text
                     | otherwise  = ">"
  toWidget [cassius|
.receipt-header
  font-size: 1.2 em
  font-weight: bold
|]
  [whamlet|
<tr>
  <td.receiptGroup>#{groupIndicator}
    $case preview here row
      $of Nothing
        <td>
        <td>
        <td>
        <td>
        <td>
      $of Just header
        ^{render header}
    $case preview there row
      $of Nothing
        <td>
        <td>
        <td>
        <td>
        <td>
        <td>
        <td>
      $of Just item
        ^{render item}
 |] 


instance ( Renderable (FieldTF t Text)
         , Renderable (FieldTF t (Maybe Text))
         , Renderable (FieldTF t Double)
         , Renderable (FieldTF t Day)
         ) => Renderable  (ReceiptHeader t) where
  render = renderReceiptHeader
renderReceiptHeader ReceiptHeader{..} = [whamlet|
<td.date>^{render rowDate}
<td.counterparty>^{render rowCounterparty}
<td.bankAccount>^{render rowBankAccount}
<td.comment>^{render rowComment}
<td.totalAmount>^{render rowTotal}
|]
instance ( Renderable (FieldTF t (Maybe Text))
         , Renderable (FieldTF t (Maybe Double))
         , Renderable (FieldTF t (Maybe Int))
         ) => Renderable  (ReceiptItem t) where
  render = renderReceiptItem
renderReceiptItem ReceiptItem{..} = [whamlet|
<td.glAccount>^{render rowGlAccount}
<td.amount>^{render rowAmount}
<td.amount>^{render rowNetAmount}
<td.amount>^{render rowMemo}
<td.tax>^{render rowTax}
<td.tax>^{render rowGLDimension1}
<td.tax>^{render rowGLDimension2}
|]

-- instance (Renderable a, Renderable b)  => Renderable (These a b) where
-- renderThese = these render render (\a b -> render a >> render b) 


instance ( Renderable h
         , Renderable r
         ) => Renderable (Int, (h, [r])) where
  render (i, (header, rows)) = [whamlet|
<tr class="#{class_ header}" id="receipt#{i}">
  ^{render header}
$forall (row, j) <- zip rows is
  <tr class="#{class_ row }" id="receipt#{i}-#{j}">
      ^{render row}
|] where is = [1..] :: [Int]
         class_ _ = "receipt-header valid bg-info" :: Text
         -- class_ ValidHeaderT = "receipt-header valid bg-info" :: Text
         -- class_ InvalidHeaderT = "receipt-header invalid bg-danger"
         -- class_ ValidRowT = "receipt-row valid"
         -- class_ InvalidRowT = "receipt-row invalid bg-warning"
         -- class_ RawT = error "Shouldn't happend"
-- <span.rowTax>#{render rowTax}

instance Renderable ([RawRow]) where
  render rows = [whamlet|
    <table.table.table-border.table-striped.table-hover>
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
      $forall row <- rows
        ^{renderReceiptRow row}
                        |]

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
  parseNamedRecord m = do
    header <- Csv.parseNamedRecord m
    item <- Csv.parseNamedRecord m
    case (header, item) of
      (EmptyHeader, EmptyItem) -> mzero
      (EmptyHeader, _) -> return $ That item
      (_, EmptyItem) -> return $ This header
      (_, _) -> return $ These header item
 
instance Csv.FromNamedRecord (ReceiptHeader 'RawT) where
  parseNamedRecord m = pure ReceiptHeader
    <*> (allFormatsDay <$$$$> m `parse` "date")
    <*> m `parse` "counterparty" 
    <*> m `parse` "bank account"
    <*> m `parse` "comment"
    <*> (unCurrency <$$$$> m  `parse` "total" )
    where parse = parseMulti columnMap

instance Csv.FromNamedRecord (ReceiptItem 'RawT) where
  parseNamedRecord m = pure ReceiptItem
    <*> m `parse` "gl account"
    <*> (unCurrency <$$$$> m `parse` "amount" )
    <*> (unCurrency <$$$$> m `parse` "net amount" )
    <*> m `parse` "memo" 
    <*> m `parse` "tax rate" 
    <*> m `parse` "dimension 1"
    <*> m `parse` "dimension 2"
    where parse = parseMulti columnMap

-- | Regroups receipts rows starting with a header (valid or invalid)
makeReceipt :: [PartialRow]
             -> Either [RawRow] -- [ReceiptRow 'RawT]
                       [(PartialHeader, [PartialItem])]
makeReceipt rows = reverse . map (map reverse) <$> r where
  r = case rows of
        [] -> Right []
        This header : rs -> Right $ go header [] rs 
        These header item :rs -> Right $ go header [item] rs
        That item : rs -> Left [That $ transformItem item]
  go :: PartialHeader -> [PartialItem] -> [PartialRow]  -> [(PartialHeader, [PartialItem])]
  go h is [] = [(h, is)]
  go h is (These header item : rows) = (h, is) : go header [item] rows
  go h is (This header : rows) = (h, is) : go header [] rows
  go h is (That item : rows) = go h (item:is) rows
  -- -- split by header, valid or not
  -- let (orphans:groups) =  S.split (S.keepDelimsL $ S.whenElt  isRight) rows
  --     -- we know header are right and rows are left]

  --     go (Right header:rows__) = (header , lefts rows__)
  --     go _ = error "Shouldn't not happend"

  --     headerToRowE :: (Either InvalidHeader ValidHeader) -> (Either RawRow ValidRow)
  --     headerToRowE = either (Left . transformRow) (Right . transformRow)

  --     receipts = if null orphans then map go groups else error "orphans"

  --     toMaybe = either (const Nothing) Just
  --     -- to valid a 'receipt' we traverse all of its constituent rows
  --     validReceipt (header, rows2) =
  --       liftA2 (,)
  --       (toMaybe header)
  --       (traverse toMaybe (headerToRowE header : rows2))
        
        
  -- in case traverse validReceipt receipts of
  --   Just valids -> Right valids
  --   Nothing -> Left receipts
