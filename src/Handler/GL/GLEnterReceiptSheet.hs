{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-} -- TODO remove
{-# OPTIONS_GHC -Wno-unused-do-bind #-} -- TODO remove
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- TODO remove
module Handler.GL.GLEnterReceiptSheet where

import Import hiding(InvalidHeader)
-- import GL.Receipt
import Handler.GL.GLEnterReceiptSheet.ReceiptRow
import Handler.GL.Payroll.Common(mkAccountH)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.CsvUtils
import qualified Data.Csv as Csv
import Control.Monad.Except
import Text.Blaze.Html(ToMarkup())
import Data.List(mapAccumL)
import GL.Receipt(ReceiptTemplateExpanded, expandTemplate, ReceiptTemplate'(..))
import GL.FA
import GL.Utils
import qualified FA as FA
import  qualified WH.FA.Types as WFA
import  qualified WH.FA.Curl as WFA
import Data.Maybe(fromJust)
import Data.Decimal
import Database.Persist.Sql(Single(..), rawSql, unSqlBackendKey)
import qualified Data.Map as Map
import Lens.Micro.Extras (preview)
import Data.These.Combinators
import Data.These.Lens
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

-- | process GET for GLEnterReceiptSheetR route
-- used to upload a receip sheet.
{-# NOINLINE getGLEnterReceiptSheetR #-}
getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = do
  renderGLEnterReceiptSheet Nothing 200 ""  (return ())

getGLReceiptTemplatesR :: Handler Html
getGLReceiptTemplatesR = do
  templateMap0 <- appReceiptTemplates <$> getsYesod appSettings
  refMap <- faReferenceMapH
  templateMap <- either (error . show) return $  traverse (expandTemplate refMap) templateMap0
  defaultLayout ( renderTemplates templateMap )

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
   <li> Spreadsheet template can be downloaded from the bank summary page.
   <li> List of available 
     <a href=@{GLR GLReceiptTemplatesR}> templates
|]

type Param = (Text, Textarea)   
postTextForm :: Maybe Param -> Form Param
postTextForm paramM = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField "Sheet name" (fst <$> paramM )
  <*> areq textareaField "Receipts" (snd <$> paramM)

{-# NOINLINE postGLEnterReceiptSheetR #-}
postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  ((textResp, __postTextW), __enctype) <- runFormPost (postTextForm Nothing)
  ((fileResp, __postFileW), ___enctype) <- runFormPost $ uploadFileForm (pure ())
  let showForm form = case form of
                      (FormFailure f) -> "Form Failure:" ++ show f
                      (FormMissing) -> "Form missing"
                      (FormSuccess _) -> "Form Success"

  (spreadSheet, param, path) <- case (textResp, fileResp) of
                        -- (FormMissing, FormMissing) -> error "missing"
                        (FormSuccess param@(title, spreadsheet), _) -> return $ (encodeUtf8 $ unTextarea spreadsheet, Just param, title)
                        (_, FormSuccess (fileInfo, encoding, ())) -> do
                          (, Nothing, fileName fileInfo) . fst <$> readUploadUTF8 fileInfo encoding
                        (formA, formB) -> error $ showForm formA ++ ", " ++ showForm formB
 
  let onSuccess  err'receipts = let
          rendered = renderReceiptSheet err'receipts
          in case concatMap fst err'receipts of
              [] -> do
                -- save text
                (key, _) <- cacheByteString Nothing spreadSheet
                -- check if the document already exists
                documentKey'msgM <- runDB $ loadAndCheckDocumentKey key
                forM documentKey'msgM $ setWarning . snd
                (form, encType) <- generateFormPost (hiddenFileForm $ Just (key, path))
                defaultLayout $ [whamlet|
                            <form method=POST action="@{GLR GLSaveReceiptSheetToFAR}" enctype=#{encType}>
                              ^{form}
                              ^{rendered}
                              <button type="submit" .btn .btn-danger>Save to Front Accounting
                                            |]
              _ -> renderGLEnterReceiptSheet param 422 "Some receipt are inconsistent " rendered

  today <- todayH
  receipts <- parseGLH spreadSheet 
  let minDay = calculateDate (AddMonths (-1)) . fst . previousVATQuarter $ calculateDate (AddDays 15) today
  -- -1 month, VATQuarter correspond to ECSL not HMRC
  
  setWarning [shamlet|Using #{tshow minDay} as the beginning of the VAT Return |]
  renderParsingResult ((\msg pre -> msg  >> renderGLEnterReceiptSheet param 422 "" pre ) :: Handler () -> Widget -> Handler Html)
                      (onSuccess . map (fanl (validateConsistency minDay)))
                      receipts
{-# NOINLINE postGLSaveReceiptSheetToFAR #-}
postGLSaveReceiptSheetToFAR :: Handler Html
postGLSaveReceiptSheetToFAR = do
  ((resp,_), __enctype) <- runFormPost (hiddenFileForm Nothing)
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  case resp of
    FormMissing -> error "missing"
    FormFailure msg ->  error $ "Form Failure:" ++ show msg
    FormSuccess (key, path) -> do
       let _types = path :: FilePath
       Just spreadsheet <- retrieveTextByKey Nothing key
       documentKey'msgM <- runDB $ loadAndCheckDocumentKey key
       receiptsE <- parseGLH $ encodeUtf8 spreadsheet
       let err = error "This file has already been validated but is not valid anymore"
       case receiptsE of
         ParsingCorrect receipts ->  do
            today <- todayH
            let minDay = calculateDate (AddMonths (-1)) . fst . previousVATQuarter $ calculateDate (AddDays 15) today
                err'receipts = map (fanl $ validateConsistency minDay) receipts
            case concatMap fst err'receipts of
              [] -> do
                settings <- getsYesod appSettings 
                docKey <- do
                    case documentKey'msgM of
                      Nothing -> do
                        runDB $ createDocumentKey  (DocumentType "glreceipt") key (pack path) ""
                      Just (k, _) -> return (entityKey k)
                res <- runExceptT $ do
                    saveReceiptsToFA docKey settings (map snd err'receipts)
                case res of
                  Left err -> setError (toHtml err) >> getGLEnterReceiptSheetR
                  Right header'type'ids -> do
                    setSuccess $ [shamlet| #{length header'type'ids} Transactions saved successfully|]
                    defaultLayout $ renderSavedTransactions (urlForFA faUrl) header'type'ids
              _ -> err
         _ -> err


parseGL :: ReferenceMap -> Map Text ReceiptTemplateExpanded -> ByteString -> ParsingResult RawRow [(ValidHeader, [ValidItem])]
parseGL refMap templateMap spreadSheet = either id ParsingCorrect $ do
  rawRows' <- parseSpreadsheet columnMap Nothing spreadSheet <|&>  WrongHeader
  let rawRows = snd $ mapAccumL fillRowDate Nothing rawRows'
  -- traceShowM ("I've been there")
  rows <- getLefts (map analyseReceiptRow rawRows) <|&>  InvalidFormat 
  -- traceShowM ("and there")
  partials <- makeReceipt rows <|&> flip (InvalidData ["First row is missing header"]) [] . map transformRow
  -- traceShowM ("there again")
  let template = findWithDefault mempty "default"  templateMap
      guessed = map (applyInnerTemplate templateMap . applyTemplate template) partials
      -- round to 2 decimals
      
  getLefts (map (validReceipt refMap) guessed) <|&> flip (InvalidData ["Missing or incorrect values"]) [] . concatMap flattenReceipt

getLefts es = case partitionEithers es of
                      ([], rights ) -> Right rights
                      (lefts, _) -> Left lefts
   

parseGLH spreadsheet = do
  templateMap0 <- appReceiptTemplates <$> getsYesod appSettings
  refMap <- faReferenceMapH
  templateMap <- either (error . show) return $  traverse (expandTemplate refMap) templateMap0
  return $ parseGL refMap templateMap spreadsheet 
  

renderReceiptSheet :: ( Renderable h
                      , Renderable r
                      , Renderable (Int, (h, [r]))
                      )
                      => [([Text], (h, [r] ))] -> Widget
  
renderReceiptSheet receipts =  do
  let ns = [1..] :: [Int]
      tooltip [] =  ""
      tooltip errors = mconcat (intersperse " - " errors) <> "\""
  toWidget [cassius|
tr.payment-header
  background: #{paleBlue}
tr.deposit-header
  background: #{amberBadgeBg}
tbody.invalid
  tr.payment-header
    background: #{paleRed}
                   |]
  [whamlet| 
<table.table.table-bordered>
  <tr>
    <th>
    <th>Template
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
  $forall ((errors, receipt), i) <- zip receipts ns
    $with error <- not (null errors)
      <tbody :error:.invalid data-toggle=tooltip title="#{tooltip errors}" >
        ^{render (i, receipt)}
|]

renderSavedTransactions :: (FATransType -> Int -> Text) -> [(ValidHeader, FATransType, Int)] -> Widget
renderSavedTransactions urlForFA header'type'ids = do
  [whamlet|
<table *{datatable} data-paging=false>
  <thead>
    <tr>
      <th>Type
      <th>Trans No
      <th>Date
      <th>Template
      <th>Counterparty 
      <th>Amount
      <th>Bank Account
      <th>Comment
 $forall (ReceiptHeader{..}, tType, pId) <- header'type'ids
   <tr>
     <td>#{transactionIcon tType}
     <td>#{transNoWithLink urlForFA "" tType pId}
     <td.date>^{render rowDate}
     <td>^{render rowTemplate}
     <td>^{render rowCounterparty}
     <td.text-right>^{render rowTotal}
     <td>^{render rowBankAccount}
     <td>^{render rowComment}
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
                     Renderable (FieldTF t (Maybe Text)) ,
                     Renderable (ReceiptHeader t) ,
                     Renderable (ReceiptItem t) 
           ) => Renderable (ReceiptRow t)
    where render r = renderReceiptRow  r

renderReceiptRow :: (
                     -- Renderable (HeaderFieldTF t Text),
                     -- Renderable (HeaderFieldTF t Double),
                     -- Renderable (RowFieldTF t (Maybe Int)),
                     -- Renderable (RowFieldTF t (Maybe Double)),
                     Renderable (FieldTF t (Maybe Text)) ,
                     Renderable (ReceiptHeader t)
                    ,Renderable (ReceiptItem t)
                    )
                 => ReceiptRow t -> Widget
renderReceiptRow row = do
  let groupIndicator | isThat row = ""  :: Text
                     | otherwise  = ">"
      template = these (rowTemplate)
                       (rowItemTemplate)
                       (\a _ -> rowTemplate a)
                       row
  toWidget [cassius|
.payment-header
  font-size: 1.2 em
  font-weight: bold
|]
  [whamlet|
<tr>
  <td.receiptGroup>#{groupIndicator}
    $case preview here row
      $of Nothing
        <td.template>^{render template}
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
         , Renderable (RefFieldTF t BankAccountRef)
         ) => Renderable  (ReceiptHeader t) where
  render = renderReceiptHeader
renderReceiptHeader ReceiptHeader{..} = [whamlet|
<td.template>^{render rowTemplate}
<td.date>^{render rowDate}
<td.counterparty>^{render rowCounterparty}
<td.bankAccount>^{render rowBankAccount}
<td.comment>^{render rowComment}
<td.totalAmount>^{render rowTotal}
|]
instance ( Renderable (FieldTF t (Maybe Text))
         , Renderable (FieldTF t (Maybe Double))
         , Renderable (FieldTF t (Maybe Int))
         , Renderable (FieldTF t Text)
         , Renderable (FieldTF t Double)
         , Renderable (FieldTF t Int)
         , Renderable (RefFieldTF t GLAccountRef)
         , Renderable (RefFieldTF t TaxRef)
         , Renderable (RefFieldTF t (Maybe Dimension1Ref))
         , Renderable (RefFieldTF t (Maybe Dimension2Ref))
         ) => Renderable  (ReceiptItem t) where
  render = renderReceiptItem
renderReceiptItem ReceiptItem{..} = [whamlet|
<td.glAccount>^{render rowGLAccount}
<td.amount>^{render rowAmount}
<td.amount>^{render rowNetAmount}
<td.amount>^{render rowMemo}
<td.tax>^{render rowTax}
<td.dimension.1>^{render rowGLDimension1}
<td.dimension.2>^{render rowGLDimension2}
|]

-- instance (Renderable a, Renderable b)  => Renderable (These a b) where
-- renderThese = these render render (\a b -> render a >> render b) 


instance (
          Renderable r
         ) => Renderable (Int, (ValidHeader, [r])) where
  render (i, (header, rows)) =
    -- let class_ = if maybe 0 validValue (rowTotal header) < 0
    let class_ = if validValue (rowTotal header) < 0
                 then "deposit-header" :: Text
                 else "payment-header"

    in [whamlet|
<tr class="#{class_}" id="receipt#{i}-1">
  <td> >
  ^{render header}
  $maybe row <- headMay rows
    ^{render row}
$forall (row, j) <- zip (drop 1 rows) is
  <tr id="receipt#{i}-#{j}">
     <td>
     <td>
     <td>
     <td>
     <td>
     <td>
     <td>
      ^{render row}
|] where is = [2..] :: [Int]

instance Renderable ([RawRow]) where
  render rows = [whamlet|
    <table.table.table-border.table-striped.table-hover>
      <tr>
        <th>
        <th>Template
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

instance Renderable (Reference s e) where
  render Reference{..} = [whamlet|
     <span.referenceId>(#{refId}) <span.referenceName>#{decodeHtmlEntities refName}
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
    , ("template", ["quick"])
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
    <*> m `parse` "template"
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
    <*> m `parse` "template"
    where parse = parseMulti columnMap

instance Csv.FromField (Either Text (Reference s e)) where
  parseField field = Left <$> Csv.parseField  field

-- | Regroups receipts rows starting with a header (valid or invalid)
makeReceipt :: -- (FieldTF 'PartialT (Maybe Text) ~ Either InvalidField (Maybe (ValidField Text)))
            [PartialRow]
             -> Either [RawRow] -- [ReceiptRow 'RawT]
                       [(PartialHeader, [PartialItem])]
makeReceipt rows = map (map reverse) <$> r where
  r = case rows of
        [] -> Right []
        This header : rs -> Right $ go header [] rs 
        These header item :rs -> Right $ go header [item] rs
        That item : __rs -> Left [These (ReceiptHeader (Left (MissingValueError "GL item without header") ) RNothing RNothing RNothing RNothing (transform $ rowItemTemplate item)) (transformItem item)]
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

faReferenceMapH :: Handler ReferenceMap
faReferenceMapH = runDB $ do
  bankAccounts <- selectList [] []
  glAccounts <- selectList [] []
  -- for some reason the type_field is a bool and can't be read properly by persistent
  dimension1s <- rawSql "SELECT id, name, closed FROM 0_dimensions WHERE type_ = 1" []
  dimension2s <- rawSql "SELECT id, name, closed FROM 0_dimensions WHERE type_ = 2" []
  vats <- selectList [] []

  let rmBankAccountMap = buildRefMap [ (FA.unBankAccountKey key, bankAccountBankAccountName, not bankAccountInactive, () )
                                     | (Entity key FA.BankAccount{..}) <- bankAccounts
                                     ]
      rmGLAccountMap = buildRefMap [ (fromJust $ readMay $ FA.unChartMasterKey key, chartMasterAccountName, not chartMasterInactive, ()  )
                                   | (Entity key FA.ChartMaster{..}) <- glAccounts
                                   ]
      rmDimension1Map = buildRefMap [(key, name, not closed, ())
                                   | (Single key, Single name, Single closed) <- dimension1s
                                   ]
      rmDimension2Map = buildRefMap [(key, name, closed, ())
                                   | (Single key, Single name, Single closed) <- dimension2s
                                   ]
      rmTaxMap = buildRefMap [ (FA.unTaxTypeKey key, tshow taxTypeRate <> "% " <> taxTypeName, not taxTypeInactive, (taxTypeRate/100, fromJust $ readMay taxTypeSalesGlCode))
                             | (Entity key FA.TaxType{..}) <- vats
                             ]
  return $ ReferenceMap{..}



  
  


-- * to Front Accounting 
saveReceiptsToFA :: Key DocumentKey -> AppSettings -> [(ValidHeader, [ValidItem])] -> ExceptT Text Handler [(ValidHeader, FATransType, Int)]
saveReceiptsToFA docKey settings receipts0 = do
  mkAccount <- lift mkAccountH
  let receipts = sortOn (rowDate . fst ) $ map roundReceipt receipts0
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      mkPayment :: (ReceiptHeader 'FinalT, [ReceiptItem 'ValidT])  -> Either WFA.BankDeposit WFA.BankPayment
      mkPayment (ReceiptHeader{..}, items)  = if rowTotal >= 0
        then Right $ WFA.BankPayment rowDate
             Nothing
             rowCounterparty
             (refId rowBankAccount)
             rowComment
             (mkItems mkAccount (map transformItem items))
        else Left $ WFA.BankDeposit rowDate
             Nothing
             rowCounterparty
             (refId rowBankAccount)
             rowComment
             (mkItems mkAccount (map transformItem items))
  forM receipts $ \receipt@(header, _items) -> do 
    let p = mkPayment . first transformHeader $  receipt
    (pId, transType) <- ExceptT . liftIO <$> WFA.postBankPaymentOrDeposit connectInfo  $ p
    hToHx $ runDB $  do
       insert_ $ TransactionMap transType pId GLReceiptE (fromIntegral $ unSqlBackendKey $ unDocumentKeyKey docKey) False
    return (header, either (const ST_BANKDEPOSIT) (const ST_BANKPAYMENT) p, pId)


mkItems :: (Int -> WFA.GLAccount) -> [ReceiptItem 'FinalT] -> [WFA.GLItemD]
mkItems mkAccount items = let
  byTax = groupAsMap (\ReceiptItem{..} -> (rowTax, refId <$> rowGLDimension1, refId <$> rowGLDimension2)) (:[]) items
  taxes = Map.mapWithKey mkTax byTax
  mkItem :: ReceiptItem 'FinalT -> WFA.GLItemD
  mkItem ReceiptItem{..} = WFA.GLItem (mkAccount $ refId rowGLAccount)
                                          (refId <$> rowGLDimension1)
                                          (refId <$> rowGLDimension2)
                                          (abs $ realFracToDecimal 2 $ rowNetAmount)
                                          Nothing
                                          rowMemo  
  mkTax :: (TaxRef, Maybe Int, Maybe Int) -> [ReceiptItem 'FinalT] -> [WFA.GLItemD]
  mkTax (taxRef, dim1, dim2 ) items =  let
    -- we need to make sure that the total amount matches
    -- net = realFracToDecimal 2 $ sum (map (rowNetAmount) items)
    total = abs $ realFracToDecimal 2 $ sum (map rowAmount items)
    glItems = map mkItem items
    netItem = sum (map WFA.gliAmount glItems)
    -- we need Sum of netAmount of each times + tax = initial net
    tax = total - netItem
    
    in glItems <> [WFA.GLItem (mkAccount $ snd $ refExtra taxRef)
                  dim1 
                  dim2 
                  tax
                  (Just netItem)
                  Nothing
                  ]
  in join $ toList taxes

renderTemplates :: Map Text ReceiptTemplateExpanded -> Widget
renderTemplates templateMap = infoPanel "Template" [whamlet|
<table *{datatable} data-page-length=200>
  <thead>
    <tr>
      <th> Name
      <th> Counterparty
      <th> Bank Account
      <th> GL Account
      <th> VAT
      <th> Memo
      <th> Dimension 1
      <th> Dimension 2
  <tbody>
    $forall (name, template) <- Map.toList templateMap
      <tr>
        <td> #{name}
        <td> #{get counterparty template}
        <td> #{get bankAccount template}
        <td> #{get glAccount template}
        <td> #{get vat template}
        <td> #{get memo template}
        <td> #{get dimension1 template}
        <td> #{get dimension2 template}
|]
  where get part t = fromMaybe "" $ case t of
                       CompoundTemplate ts -> headMay $ mapMaybe part ts
                       _ -> part t
        counterparty (CounterpartySetter t) = Just t
        counterparty _ = Nothing
        bankAccount (BankAccountSetter (Identity c)) = Just $ refName c
        bankAccount _ = Nothing
        glAccount (ItemGLAccountSetter (Identity c)) = Just $ tshow (refId c) <> " - " <> refName c
        glAccount _ = Nothing
        vat (ItemVATDeducer (Identity c)) = Just $ refName c
        vat _ = Nothing
        memo (ItemMemoSetter c) = Just $ c
        memo _ = Nothing
        dimension1 (ItemDimension1Setter (Identity c)) =Just $ tshow (refId c) <> " - " <> refName c 
        dimension1 _ = Nothing
        dimension2 (ItemDimension2Setter (Identity c)) =Just $ tshow (refId c) <> " - " <> refName c 
        dimension2 _ = Nothing
         
        
        


