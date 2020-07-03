{-# LANGUAGE OverloadedStrings #-}
module Handler.GL.CheckAIS
(getGLCheckAISR
,postGLCheckAISR
) where

import Import hiding(force)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Text(splitOn)
import FA
-- import GL.Utils

data CheckParam = CheckParam
  { invoiceIds :: Maybe Text
  , invoiceRefs :: Maybe Text
  , creditIds :: Maybe Text
  , creditRefs :: Maybe Text
  }

getGLCheckAISR :: Handler Html
getGLCheckAISR = renderCheck Nothing Nothing

postGLCheckAISR :: Handler Html
postGLCheckAISR = do
  ((resp, __formW), __enctype) <- runFormPost (checkForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      w <- processCheck param
      renderCheck (Just param) (Just w)

checkForm paramM = renderBootstrap3 BootstrapBasicForm form where
  form = CheckParam <$> aopt textField "invoice ids" (invoiceIds <$> paramM)
                    <*> aopt textField "invoice references " (invoiceRefs <$> paramM)
                    <*> aopt textField "credits ids " (creditIds <$> paramM)
                    <*> aopt textField "credits references  " (creditRefs <$> paramM)

-- | Common page between GET and POST
renderCheck :: Maybe CheckParam -> Maybe Widget -> Handler Html 
renderCheck param widgetM = do
  ((formW), encType) <- generateFormPost (checkForm param)
  defaultLayout $ [whamlet|
<form #render-check-form role=form method=post action=@{GLR GLCheckAISR} enctype=#{encType}>
  <div.well>
    ^{ formW }
    <button.btn.btn-primary type="submit" name="Check" value="Check">Check
  $maybe widget <- widgetM
      ^{widget}
                          |]



 -- | The real function where the checks are done and rendered to a Widget
processCheck :: CheckParam -> Handler Widget
processCheck CheckParam{..} = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  customerName_ <- entityNameMH True
  let iids = mapMaybe readMay (maybe [] (splitOn ",") invoiceIds)
      irefs = maybe [] (splitOn ",") invoiceRefs
      cids = mapMaybe readMay (maybe [] (splitOn ",") creditIds)
      crefs = maybe [] (splitOn ",") creditRefs
      urlFn = urlForFA faURL
      customerName tran = let debtor = debtorTranDebtorNo tran
                          in fromMaybe (tshow debtor) $ customerName_ (toEnum $ debtorTranType tran) (fromIntegral <$> debtor)
  invoices <- loadCustomerTransaction ST_SALESINVOICE iids
  credits <- loadCustomerTransaction ST_CUSTCREDIT cids
  invoicesByRef <- loadCustomerTransactionByRef ST_SALESINVOICE irefs
  creditsByRef <- loadCustomerTransactionByRef ST_CUSTCREDIT crefs
  let trans = invoices ++ credits ++ invoicesByRef ++ creditsByRef
      summary = [whamlet|
  <table *{datatable}>
    <thead>
      <tr>
        <th data-class-name="text-right"> Trans No
        <th> Type
        <th> Customer
        <th data-class-name="text-right"> Total Invoice
        <th data-class-name="text-right"> Net Goods
        <th data-class-name="text-right"> Discount 
        <th data-class-name="text-right"> Net Shipping
        <th data-class-name="text-right"> Tax Goods
        <th data-class-name="text-right"> Shipping Tax

    $forall (Entity _ tran) <- trans
      <tr>
        <td> #{transNoWithLink urlFn "" (toEnum $ debtorTranType tran) (debtorTranTransNo tran) }
        <td> #{transactionIcon (toEnum $ debtorTranType tran)}
        <td> #{customerName tran }
        <td> #{formatDouble (totalInvoice tran)}
        <td> #{formatDouble (debtorTranOvAmount tran)}
        <td> #{formatDouble (debtorTranOvDiscount tran)}
        <td> #{formatDouble (debtorTranOvFreight tran)}
        <td> #{formatDouble (debtorTranOvGst tran)}
        <td> #{formatDouble (debtorTranOvFreightTax tran)}
 |]

  return $ primaryPanel "Summary" summary >> primaryPanel "AIS" (aisRemittanceAdvice customerName trans)

-- | Display information to match AIS remittance advice
-- so we can easily spot the difference
aisRemittanceAdvice :: (DebtorTran -> Text) -> [Entity DebtorTran] -> Widget
aisRemittanceAdvice customerName trans =  
  let  tosum = map (aisBilling . entityVal) trans ++ map (totalInvoice . entityVal)  trans
       isPos = (>0)
       isNeg = (<0)
  in [whamlet|
  <table *{datatable}>
    <thead>
      <tr>
        <th> Tran Date
        <th> Reference
        <th> Customer
        <th> 
        <th data-class-name="text-right"> AIS Discount (+VAT)
        <th data-class-name="text-right"> Total Invoice
        <th data-class-name="text-right"> Net Goods
        <th data-class-name="text-right"> Net Shipping / Net Billing
        <th data-class-name="text-right"> Tax Goods + Shipping
        <th data-class-name="text-right"> Total Debit
        <th data-class-name="text-right"> Total Credit

    $forall (Entity _ tran) <- trans
      $# ais billing
      <tr>
        <td.c0> 
        <td.c1>
        <td.c2> AIS BILING
        <td.c3>
        <td.c4> -
        <td.c5> #{formatDouble (aisBilling tran)}
        <td.c6> 
        <td.c7> #{formatDouble (aisBillingNet tran)}
        <td.c8> #{formatDouble (aisBillingTax tran)}
        #{showDebitCredit (aisBilling tran)}
        $# customer invoice
      <tr>
        <td.c0> #{tshow (debtorTranTranDate tran)}
        <td.c1> #{debtorTranReference tran}
        <td.c2> #{customerName tran}
        <td.c3>
        <td.c4> #{formatDouble (aisDiscount tran)}
        <td.c5> #{formatDouble (totalInvoice tran)}
        <td.c6> #{formatDouble (debtorTranOvAmount tran)}
        <td.c7> #{formatDouble (debtorTranOvFreight tran)}
        <td.c8> #{formatDouble (liftA2 (+) debtorTranOvGst debtorTranOvFreightTax  tran)}
        #{showDebitCredit (totalInvoice tran)}
    $# total
    <tr>
      <td.c0>
      <td.c1>
      <td.c2>
      <td.c3>
      <td.c4> #{formatDouble (sum $ map (aisDiscount . entityVal) trans)}
      <td.c5> #{formatDouble (sum $ map (liftA2 (+) aisBilling totalInvoice . entityVal)  trans)}
      <th.c6> #{formatDouble (sum $ map (debtorTranOvAmount . entityVal)  trans)}
      <th.c7> #{formatDouble (sum $ map (liftA2 (+) aisBillingNet debtorTranOvFreight . entityVal)  trans)}
      <th.c8> #{formatDouble (sum $ map (liftA3 (add3) aisBillingTax debtorTranOvGst debtorTranOvFreightTax . entityVal)  trans)}
      <th> #{formatDouble (0 - sum (filter isNeg tosum))}
      <th> #{formatDouble (sum (filter isPos tosum))}
    <tr>
      <th colspan=9>
      <th> total Invoices
      <th> #{formatDouble (sum tosum)}
    <tr>
      <th colspan=9>
      <th> total discount
      <th> #{formatDouble (sum $ map (aisDiscount . entityVal) trans) }
    <tr>
      <th colspan=9>
      <th> to be paid
      <th.bg-success> #{formatDouble $ sum tosum - (sum $ map (aisDiscount . entityVal) trans) }
 |]

add3 a b c = a + b + c
aisDiscount :: DebtorTran -> Double
aisDiscount trans@DebtorTran{..} = (debtorTranOvAmount ) * 0.06  * (1 + taxRate trans)
taxRate DebtorTran{..} = debtorTranOvGst / debtorTranOvAmount 
totalInvoice DebtorTran{..} = debtorTranOvAmount + debtorTranOvFreight + debtorTranOvFreightTax + debtorTranOvGst - debtorTranOvDiscount
aisBillingNet DebtorTran{..} = - debtorTranOvAmount * 0.01
aisBillingTax DebtorTran{..} = - debtorTranOvAmount * 0.01 * 0.2
aisBilling DebtorTran{..} = - debtorTranOvAmount * 0.01 * 1.20 -- VAT even if the customer is Ireland : it is AIS which is invoicing



showDebitCredit amount =
  if amount < 0
  then
    [shamlet|
      <td> #{formatDouble (0 - amount)}
      <td>
      |]
  else
    [shamlet|
      <td>
      <td> #{formatDouble amount}
      |]
    

loadCustomerTransaction :: FATransType  -> [Int] -> Handler [(Entity DebtorTran)]
loadCustomerTransaction type_ ids = do
  runDB $ map (reverseIfCredit type_) <$> selectList [DebtorTranTransNo <-. ids, DebtorTranType ==. fromEnum type_ ] [Asc DebtorTranTranDate]
  
loadCustomerTransactionByRef :: FATransType  -> [Text] -> Handler [(Entity DebtorTran)]
loadCustomerTransactionByRef type_ refs = do
  runDB $ map (reverseIfCredit type_) <$> selectList [DebtorTranReference <-. refs, DebtorTranType ==. fromEnum type_ ] [Asc DebtorTranTranDate]

reverseIfCredit ST_CUSTCREDIT e@(Entity _ DebtorTran{..}) =
 e {entityVal = DebtorTran{debtorTranOvAmount= -debtorTranOvAmount
                      ,debtorTranOvFreight= -debtorTranOvFreight
                      ,debtorTranOvFreightTax= -debtorTranOvFreightTax
                      ,debtorTranOvGst= -debtorTranOvGst
                      ,..}
                      }
reverseIfCredit _ t = t 


