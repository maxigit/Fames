{-# LANGUAGE OverloadedStrings #-}
module Handler.GL.Tools 
( getGLToolsR
, postGLSupplierMirroredR
) where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import qualified FA as FA
import qualified WH.FA.Types as WFA
import qualified WH.FA.Curl as WFA
import GL.FA (Amount)
import Database.Persist.Sql (fromSqlKey)

{-# NOINLINE getGLToolsR #-}
getGLToolsR :: Handler Html
getGLToolsR = do
  defaultLayout $ do
    setTitle "GL Tools"
    renderTools

renderTools :: Widget
renderTools =  do
  (smFormW, smEncType) <- handlerToWidget $ generateFormPost supplierMirroredForm
  let smPanel = infoPanel' (Just "supplier-mirrored")
                           "Enter Supplier Mirrored Invoice/nredit Note"
                           [whamlet|
                              <div.well>
                                <form role=form method=POST action="@{GLR GLSupplierMirroredR}" encType=#{smEncType}>
                                  ^{smFormW}
                                  <button type="submit" .btn.btn-danger>Submit
                           |]

  [whamlet|
    <h1.jumbotron> Tools
  |]
  smPanel


-- * Supplier Mirrored  {{{

postGLSupplierMirroredR :: Handler Html
postGLSupplierMirroredR = do
  ((resp, _formW), _encType) <- runFormPost supplierMirroredForm
  settings <- appSettings <$> getYesod
  let faUrl = pack $ appFAExternalURL settings
  entityNameM <- entityNameMH False
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      r <- saveSupplierMirror settings param
      wM <- case r of
        Left e -> setError (toHtml e) >> return Nothing
        Right trans -> do
          setSuccess "Invoice/credit note pair created successfully."
          return $ Just 
            [whamlet|
              <table *{datatable} data-paging=false>
                <thead>
                  <th>Type
                  <th>Trans No
                  <th>Date
                  <Supplier>
                <tbody>
                  $forall (tType, date, supplier, tId) <- trans
                    <tr>
                      <td>>#{transactionIcon tType}
                      <td>#{transNoWithLink (urlForFA faUrl) "" tType tId}
                      <td>#{tshow date}
                      <td>#{fromMaybe (tshow supplier) (entityNameM ST_SUPPINVOICE (Just $ fromIntegral $ fromSqlKey supplier))}
            |]
      defaultLayout $ do
        fromMaybe (return ()) wM
        renderTools

data SMParam = SMParam 
  { smDate :: !Day
  , smCreditDate :: !(Maybe Day)
  , smReference :: !Text
  , smSupplier :: !(Key FA.Supplier)
  , smCreditSupplier :: !(Maybe (Key FA.Supplier))
  , smAccount :: !(Key FA.ChartMaster)
  , smAmount :: !Amount
  , smMemo :: !(Maybe Text)
  }

supplierMirroredForm = renderBootstrap3 BootstrapBasicForm $ SMParam 
  <$> areq dayField "invoice date" Nothing
  <*> aopt dayField "credit note date" Nothing
  <*> areq textField "supplier ref" Nothing
  <*> areq (selectField $ optionsPersistKey [FA.SupplierInactive ==. False] [Asc FA.SupplierId] FA.supplierSuppName) "invoice supplier" Nothing
  <*> aopt (selectField $ optionsPersistKey [FA.SupplierInactive ==. False] [Asc FA.SupplierId] FA.supplierSuppName) "credit note supplier" Nothing
  <*> areq (selectField $ optionsPersistKey [FA.ChartMasterInactive ==. False] [Asc FA.ChartMasterId] showAccount) "account" Nothing
  <*> areq doubleField "amount" Nothing
  <*> aopt textField "memo" Nothing
  where showAccount ac = FA.chartMasterAccountName ac 

saveSupplierMirror :: AppSettings -> SMParam -> Handler (Either Text [(FATransType, Day, Key FA.Supplier, Int)])
saveSupplierMirror settings SMParam{..} = do
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
  -- invoice
      poiSupplier = fromEnum $ fromSqlKey smSupplier
      poiReference = Nothing
      poiSupplierReference = smReference <> "-PRE"
      poiDate = smDate
      poiDueDate = smDate
      poiMemo = fromMaybe smReference smMemo
      poiDeliveryIds = []
      poiGLItems = [item]
      -- credit
      pcnSupplier = maybe poiSupplier (fromIntegral . fromSqlKey) smCreditSupplier
      pcnReference = Nothing
      pcnSupplierReference = smReference <> "-REV"
      pcnDate = fromMaybe smDate smCreditDate
      pcnDueDate = smDate
      pcnInvoiceNo = Nothing
      pcnDeliveryIds = []
      pcnGLItems = [item]
      pcnMemo = poiMemo
      -- item
      gliAccount = WFA.GLAccount $ FA.unChartMasterKey smAccount
      gliDimension1 = Nothing
      gliDimension2 = Nothing
      gliTaxOutput = Nothing
      gliAmount = smAmount
      gliMemo = Just smReference
      item = WFA.GLItem{..}
  
  hxtoHe $ do
    invId <- ioeToHx $ WFA.postPurchaseInvoice connectInfo  WFA.PurchaseInvoice{..}
    creditId <- ioeToHx $ WFA.postPurchaseCreditNote connectInfo WFA.PurchaseCreditNote{..}
    let trans = [ (ST_SUPPINVOICE, poiDate, smSupplier, invId)
                , (ST_SUPPCREDIT, pcnDate, fromMaybe smSupplier smCreditSupplier,   creditId)]
        eventNo = invId -- no event exists. makes it unique
    lift $ runDB $
      forM trans $ \(tType,_,_, tId) -> do
        insert_ $ TransactionMap tType tId GLSupplierMirroredE eventNo False
    return $ trans






  

  



  
