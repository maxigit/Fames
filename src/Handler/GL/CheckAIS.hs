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
  { groups :: Maybe Text
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
  form = CheckParam <$> aopt textField "transactions" (groups <$> paramM)

-- | Common page between GET and POST
renderCheck :: Maybe CheckParam -> Maybe Widget -> Handler Html 
renderCheck param widgetM = do
  ((formW), encType) <- generateFormPost (checkForm param)
  defaultLayout $ [whamlet|
$newline text
  ^{info}
<form #render-check-form role=form method=post action=@{GLR GLCheckAISR} enctype=#{encType}>
  <div.well>
    ^{ formW }
    <button.btn.btn-primary type="submit" name="Check" value="Check">Check
  $maybe widget <- widgetM
      ^{widget}
                          |]


info = infoPanel "Info"
  [whamlet|
  Please enter the transaction as a <verbatim>,</verbatim{ separated list
  of invoice or credit note (prefixed with <verbatim>-</verbatim>.
  A transaction will be looked by reference unless it start with a <verbatim>#</verbatim>.
  In that case it will be looked up by id.

  Example

      <code> 1 -#2  , 3

   Will load 2 groups. The first made of
   invoice with <b>reference</b> 1 and credit note with id 2
   and the second group invoice with reference 3
  |]

 -- | The real function where the checks are done and rendered to a Widget
processCheck :: CheckParam -> Handler Widget
processCheck CheckParam{..} = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  customerName_ <- entityNameMH True
  let urlFn = urlForFA faURL
      customerName tran = let debtor = debtorTranDebtorNo tran
                          in fromMaybe (tshow debtor) $ customerName_ (toEnum $ debtorTranType tran) (fromIntegral <$> debtor)
  transGroups <- loadTransactionGroups groups
  let trans  = concat transGroups
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

    $forall tran <- trans
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

  return $ primaryPanel "Summary" summary >> primaryPanel "AIS" (aisRemittanceAdvice customerName transGroups)

-- | Display information to match AIS remittance advice
-- so we can easily spot the difference
aisRemittanceAdvice :: (DebtorTran -> Text) -> [[DebtorTran]] -> Widget
aisRemittanceAdvice customerName groups =  
  let  tosum = map (aisBilling) groups ++ map (totalInvoice)  trans
       isPos = (>0)
       isNeg = (<0)
       trans = concat groups
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

    $forall group <- groups
      $# ais billing
      <tr>
        <td.c0> 
        <td.c1>
        <td.c2> AIS BILING
        <td.c3>
        <td.c4> -
        <td.c5> #{formatDouble (aisBilling group)}
        <td.c6> 
        <td.c7> #{formatDouble (aisBillingNet group)}
        <td.c8> #{formatDouble (aisBillingTax group)}
        #{showDebitCredit (aisBilling group)}
        $# customer invoice
      $forall tran <- group
        <tr>
          <td.c0> #{tshow (debtorTranTranDate tran)}
          <td.c1> #{debtorTranReference tran}
          <td.c2> #{customerName tran}
          <td.c3>
          <td.c4> #{formatDouble (aisDiscount' tran)}
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
      <td.c4> #{formatDouble (aisDiscount trans)}
      <td.c5> #{formatDouble (sum $ map (liftA2 (+) aisBilling' totalInvoice)  trans)}
      <th.c6> #{formatDouble (sum $ map (debtorTranOvAmount)  trans)}
      <th.c7> #{formatDouble (sum $ map (liftA2 (+) aisBillingNet' debtorTranOvFreight)  trans)}
      <th.c8> #{formatDouble (sum $ map (liftA3 (add3) aisBillingTax' debtorTranOvGst debtorTranOvFreightTax)  trans)}
      <th> #{formatDouble (0 - sum (filter isNeg tosum))}
      <th> #{formatDouble (sum (filter isPos tosum))}
    <tr>
      <th colspan=9>
      <th> total Invoices
      <th> #{formatDouble (sum tosum)}
    <tr>
      <th colspan=9>
      <th> total discount
      <th> #{formatDouble (aisDiscount trans) }
    <tr>
      <th colspan=9>
      <th> to be paid
      <th.bg-success> #{formatDouble $ sum tosum - (aisDiscount trans) }
 |]

add3 a b c = a + b + c
aisDiscount' :: DebtorTran -> Double
aisDiscount' trans@DebtorTran{..} = (debtorTranOvAmount ) * 0.06  * (1 + taxRate trans)
taxRate DebtorTran{..} = debtorTranOvGst / debtorTranOvAmount 
totalInvoice DebtorTran{..} = debtorTranOvAmount + debtorTranOvFreight + debtorTranOvFreightTax + debtorTranOvGst - debtorTranOvDiscount
aisBillingNet' DebtorTran{..} = - debtorTranOvAmount * 0.01
aisBillingTax' DebtorTran{..} = - debtorTranOvAmount * 0.01 * 0.2
aisBilling' DebtorTran{..} = - debtorTranOvAmount * 0.01 * 1.20 -- VAT even if the customer is Ireland : it is AIS which is invoicing


aisDiscount = sumMap aisDiscount'
aisBillingNet = sumMap aisBillingNet'
aisBillingTax = sumMap aisBillingTax'
aisBilling = sumMap aisBilling'
sumMap f = sum . map f



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
    

loadTransactionGroups :: Maybe Text -> Handler [[DebtorTran]]
loadTransactionGroups Nothing = return []
loadTransactionGroups (Just groups) = mapM loadTransactionGroup (splitOn (",") groups)

loadTransactionGroup :: Text -> Handler [DebtorTran]
loadTransactionGroup group = catMaybes <$> mapM loadTransaction (splitOn " " group)

loadTransaction :: Text -> Handler (Maybe DebtorTran)
loadTransaction tran = do
  let (type_, tran')  = case stripPrefix "-" tran of
                          Nothing -> (ST_SALESINVOICE, tran)
                          Just t -> (ST_CUSTCREDIT, t)
      selector = case stripPrefix "#" tran' >>= readMay of
                    Nothing -> DebtorTranReference ==. tran'
                    Just tid -> DebtorTranTransNo ==. tid
  fmap (reverseIfCredit type_ .  entityVal) . headMay
    <$> runDB (selectList [DebtorTranType ==. fromEnum type_, selector] [])
  
reverseIfCredit ST_CUSTCREDIT DebtorTran{..} =
   DebtorTran{debtorTranOvAmount= -debtorTranOvAmount
             ,debtorTranOvFreight= -debtorTranOvFreight
             ,debtorTranOvFreightTax= -debtorTranOvFreightTax
             ,debtorTranOvGst= -debtorTranOvGst
             ,..}
reverseIfCredit _ t = t 


