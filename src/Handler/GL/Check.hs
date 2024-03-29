{-# LANGUAGE ImplicitParams #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Handler.GL.Check
( getGLCheckR 
, postGLCheckR 
, getGLCheckDebtorTransR
, postGLFixDebtorTransR
) where

import Import hiding(showDouble, force)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import FA
import GL.Utils
-- import Handler.Util hiding(showDouble)
import Data.Align(align)
import qualified Data.Map as Map
import Util.Decimal

showDouble x = tshow $ toDecimalWithRounding (Round 6) x
-- * Types 
data TransactionSummary = TransactionSummary 
  { tsTransNo :: Int
  , tsTransType :: FATransType
  , tsDate :: Day
  , tsPersonId :: Int
  , tsBranchId :: Int
  , tsNet :: Double
  , tsTax :: Double
  , tsShippingNet :: Double
  , tsDetails :: [Entity DebtorTransDetail]
  , tsGl ::  [Entity GlTran]
  }

tsTotal t = sum $ map ($ t) [tsNet,  tsTax , tsShippingNet]
tsTotalDetails t = sum $ map (getAmount . entityVal) (tsDetails t) where
  getAmount = case tsTransType t of
                ST_CUSTDELIVERY -> debtorTransDetailCOGSAmount
                ST_CUSTCREDIT -> debtorTransDetailCOGSAmount
                _ -> \x -> debtorTransDetailSalesAmount  x -- + debtorTransDetailSalesTaxAmount x
  

tsTotalDebit t = sum [  glTranAmount 
                     | Entity _ gl@GlTran{..} <- tsGl t
                     , isDebit gl 
                     ]
tsTotalCredit t = sum [  - glTranAmount 
                     | Entity _ gl@GlTran{..} <- tsGl t
                     , not (isDebit gl)
                     ]

-- | Discount are entered separatily in invoice
-- example 10 with 10% discount generate 10 + -1 (instead of 9)
-- which add up to 11 credit and 11 debit
tsDetailsSplitDiscount t = roundWith $ sum $ map (getAmount . entityVal) (tsDetails t) where
  getAmount = case tsTransType t of
                ST_SALESINVOICE -> \x -> debtorTransDetailDiscountedAmount  x
                _ -> const 0
tsDetailsTax t = sum $ map (getAmount . entityVal) (tsDetails t) where
  getAmount = case tsTransType t of
                _ -> \x -> debtorTransDetailSalesTaxAmount x
-- | How much should the details adds too. It depends on the transaction types
tsExpectedDetails t = case tsTransType t of
  ST_CUSTDELIVERY -> tsTotalDetails t -- only COGS side of the sales
  ST_CUSTCREDIT -> tsTotalDetails t -- only COGS side of the sales
  ST_CUSTPAYMENT -> 0 -- no details
  _ -> if ?taxIncluded
       then tsNet t - tsDetailsTax t  -- the net is not a net
       else tsNet t

tsExpectedDebit t = case tsTransType t of
  ST_CUSTDELIVERY -> tsTotalDetails t
  ST_CUSTCREDIT -> tsTotalDetails t {- cogs -} + tsTotal t -- refund customer
  ST_SALESINVOICE -> tsTotal t + tsDetailsSplitDiscount t
  _ -> tsTotal t 

tsExpectedGeneratedDebit t =
  case tsTransType t of
    ST_CUSTCREDIT -> tsExpectedDetails t { tsTransType = ST_SALESINVOICE}
                   + tsExpectedDetails t { tsTransType = ST_CUSTDELIVERY}
    _ -> tsExpectedDebit t 

tsRoute TransactionSummary{..} = GLR (f (fromIntegral tsTransNo) (fromIntegral $ fromEnum tsTransType)) where
  f = case tsTransType of
        ST_CUSTPAYMENT -> GLCheckDebtorTransR 
        ST_SALESINVOICE -> GLCheckDebtorTransR 
        ST_CUSTDELIVERY -> GLCheckDebtorTransR 
        ST_CUSTCREDIT -> GLCheckDebtorTransR 
        _ -> GLCheckDebtorTransR 
  
tsFixRoute TransactionSummary{..} = GLR (f (fromIntegral tsTransNo) (fromIntegral $ fromEnum tsTransType)) where
  f = case tsTransType of
        ST_CUSTPAYMENT -> GLFixDebtorTransR
        ST_SALESINVOICE -> GLFixDebtorTransR 
        ST_CUSTDELIVERY -> GLFixDebtorTransR 
        ST_CUSTCREDIT -> GLFixDebtorTransR 
        _ -> GLFixDebtorTransR 

near a b = abs (a - b) < 1e-2
tsIsValid t = and [ tsExpectedDetails t `near` tsTotalDetails t
                  , tsExpectedDebit t `near` tsTotalDebit t
                  , tsExpectedDebit t `near` tsTotalCredit t
                  ]
  
-- | display an amount and add a status depending on the reference value
showWithStatus ref v = [shamlet|<span :bad:.bg-danger :bad:.text-danger :warning:.bg-warning :warning:.text-warning data-toggle=tooltip title="Expecting #{showDouble ref}" >#{showDouble v} |] where
  diff =  abs (ref - v)
  bad = diff > 1e-2
  warning = diff > 1e-3 && (not bad)

-- * Form 
data CheckParam = CheckParam
  { pFrom :: Maybe Day
  , pTo :: Maybe Day
  , pDisplayAll :: Bool
  } deriving (Show, Eq)

defaultParam :: CheckParam
defaultParam = CheckParam Nothing Nothing False

checkForm paramM = renderBootstrap3 BootstrapBasicForm form where
  form = CheckParam <$> aopt dayField "From" (pFrom <$> paramM)
                    <*> aopt dayField "to" (pTo <$> paramM)
                    <*> areq boolField "Display All" (pDisplayAll <$> paramM <|> Just False )

{-# NOINLINE getGLCheckR #-}
getGLCheckR :: Handler Html
getGLCheckR = renderCheck defaultParam Nothing

itext t = t :: Text
{-# NOINLINE postGLCheckR #-}
postGLCheckR :: Handler Html
postGLCheckR = do
  ((resp, __formW), __enctype) <- runFormPost (checkForm Nothing)
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  let ?taxIncluded = deduceTax
      ?roundMethod = roundfa
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      alls <- checkAll param
      let w = [whamlet|
<table#gl-check *{datatable} data-paging=false>
  <thead>
    <tr>
      <th> No
      <th> Type
      <th> Date
      <th> Net
      <th> Tax
      <th> Shipping
      <th> Total
      <th> Details
      <th> Debit
      <th> Credit
  $forall t <- alls
    $if pDisplayAll param || not (tsIsValid t)
      <tr>
        <td><a href=@{tsRoute t} > #{tsTransNo t}
        <td><a href="#{urlForFA faUrl (tsTransType t) (tsTransNo t)}">
            #{transactionIconSpan $ tsTransType t }
        <td> #{tshow $ tsDate t}
        <td> #{tshow $ tsNet t}
        <td> #{tshow $ tsTax t}
        <td> #{tshow $ tsShippingNet t}
        <td> #{showDouble $ tsTotal t}
        <td> #{showWithStatus (tsExpectedDetails t)  (tsTotalDetails t) }
        <td> #{showWithStatus (tsExpectedDebit t) (tsTotalDebit t) }
        <td> #{showWithStatus (tsExpectedDebit t) (tsTotalCredit t) }
                      |]
      renderCheck param (Just w)

  
defaultLayout' w = defaultLayout (w >> js) where
  js = toWidget [julius|
$('[data-toggle="tooltip"]').tooltip();
|]


renderCheck :: CheckParam -> Maybe Widget -> Handler Html
renderCheck param widgetM = do
  ((formW), encType) <- generateFormPost (checkForm $ Just param)
  defaultLayout' $ [whamlet|
<form #render-check-form role=form method=post action=@{GLR GLCheckR} enctype=#{encType}>
  <div.well>
    ^{ formW }
    <button.btn.btn-primary type="submit" name="Check" value="Check">Check
  $maybe widget <- widgetM
      ^{widget}
                          |]


-- ** DB 
checkAll :: CheckParam -> Handler [_]
checkAll param = runDB $ do
  customerTransactions <- loadCustomerTransactions param
  return customerTransactions


loadCustomerTransactions :: CheckParam -> SqlHandler [ TransactionSummary]
loadCustomerTransactions param = do
  let ws = catMaybes [ pFrom param <&> (DebtorTranTranDate >=. )
                     , pTo param <&> (DebtorTranTranDate <. )
                     ]
  trans <- selectList ws [Asc DebtorTranTranDate, Asc DebtorTranType, Asc DebtorTranTransNo]

  mapM loadCustomerSummary trans

loadCustomerSummary ts@(Entity _ DebtorTran{..}) = do
          details <- loadCustomerTransDetails ts
          gls <- loadGLFor debtorTranTransNo debtorTranType
          return $ tsFromDebtor ts details gls

tsFromDebtor (Entity _ DebtorTran{..}) details gls = let
  transType = toEnum debtorTranType
  in TransactionSummary debtorTranTransNo transType debtorTranTranDate
                     (fromMaybe (error $ "Debtor no should be set for " ++ show debtorTranType ++ " #" ++ show debtorTranTransNo)
                                debtorTranDebtorNo)
                     debtorTranBranchCode
                     (debtorTranOvAmount + if transType == ST_CUSTPAYMENT
                                           then debtorTranOvDiscount
                                           else - debtorTranOvDiscount)
                                           
                     (debtorTranOvFreightTax + debtorTranOvGst)
                     (debtorTranOvFreight)
                     details gls
  
 
loadCustomerTransDetails (Entity _ DebtorTran{..})  = do
  selectList [ DebtorTransDetailDebtorTransNo ==. Just debtorTranTransNo
             , DebtorTransDetailDebtorTransType ==. Just debtorTranType
             ]
             [Asc DebtorTransDetailId]
  
loadGLFor transNo transType = do
  selectList [ GlTranTypeNo ==. transNo
             , GlTranType ==. transType
             ]
             [Asc GlTranId]

-- ** Check 
{-# NOINLINE getGLCheckDebtorTransR #-}
getGLCheckDebtorTransR :: Int64 -> Int64 -> Handler Html
getGLCheckDebtorTransR no tType = do
  force <- isJust <$> lookupGetParam "force"
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  let ?taxIncluded = deduceTax
      ?roundMethod = roundfa
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  t <- runDB $ do
    tm <- selectFirst [DebtorTranTransNo ==. fromIntegral no, DebtorTranType ==. fromIntegral tType] []
    maybe (error $ "Transaction" <> show no <> " not found") loadCustomerSummary tm
  newGls <- runDB $ generateGLs t
  let (newDebits, newCredits) = partition isDebit newGls
      newDebit = sum (map glTranAmount newDebits) + tsTax t + tsShippingNet t
      newCredit = negate ( sum (map glTranAmount newCredits)) + tsTax t + tsShippingNet t
      glDiffs = alignGls (tsGl t) newGls
      glDiffStatus  = fixGls glDiffs
      newNonNull = filter ((/= 0) . glTranAmount) newGls
  defaultLayout' [whamlet|
<div.panel.panel-info>
  <div.panel-heading><h3>
    #{itext $ showTransType (tsTransType t)}
    <a href="#{urlForFA faUrl (tsTransType t) (tsTransNo t)}">
      ##{tshow (tsTransNo t)}
    <a href="#{glViewUrlForFA faUrl (tsTransType t) (tsTransNo t)}"> GL
  <div.panel-body>
    <table.table.table-hover>
      <tr>
        <th> Date
        <td> #{tshow $ tsDate t}
      <tr>
        <th> Net
        <td> #{tshow $ tsNet t}
      <tr>
        <th> Tax
        <td> #{tshow $ tsTax t}
      <tr>
        <th> Shipping
        <td> #{tshow $ tsShippingNet t}
      <tr>
        <th> Total
        <td> #{showDouble $ tsTotal t}
      <tr>
        <th> Details
        <td> #{showWithStatus (tsExpectedDetails t)  (tsTotalDetails t) }
      <tr>
        <th> Debit
        <td> #{showWithStatus (tsExpectedDebit t) (tsTotalDebit t) }
      <tr>
        <th> Credit
        <td> #{showWithStatus (tsExpectedDebit t) (tsTotalCredit t) }
      <tr>
        <th> Generated Debit
        <td> #{showWithStatus (tsExpectedGeneratedDebit t) (newDebit) }
      <tr>
        <th> Generated Credit
        <td> #{showWithStatus (tsExpectedGeneratedDebit t) (newCredit) }
      <tr>
        <th> Fixed Debit
        <td> #{showWithStatus (tsExpectedDebit t) (fixedDebit glDiffStatus) }
      <tr>
        <th> Fixed Credit
        <td> #{showWithStatus (tsExpectedDebit t) (fixedCredit glDiffStatus) }
      <tr>
        <td>
        $if force || (and [near (tsExpectedDebit t) (fixedDebit glDiffStatus), near (tsExpectedDebit t) (fixedCredit glDiffStatus) ])
          <td>
            <form role=form method=POST action=@{tsFixRoute t}>
              <button.btn.btn-danger type="submit" name="Fix" value="Fix">Fix
        $else
          <td>
            <form role=form method=POST action=@{tsFixRoute t}>
                <button.btn.btn-primary.disabled type="submit" name="Fix" value="Fix">Fix

<div.panel.panel-info>
  <div.panel-heading data-toggle=collapse data-target="#details-panel"><h3> Details
  <div.panel-body.collapse id=details-panel>
    ^{displayDetails t}
<div.panel.panel-info>
  <div.panel-heading data-toggle=collapse data-target="#gls-diff-panel"><h3> Gls Diff
  <div.panel-body.collapse.in id=gls-diff-panel>
    ^{displayGlsDiff glDiffs }
<div.panel.panel-info>
  <div.panel-heading data-toggle=collapse data-target="#gls-panel"><h3> Gls
  <div.panel-body.collapse id=gls-panel>
    ^{displayGls "gls-table" t}
<div.panel.panel-info>
  <div.panel-heading data-toggle=collapse data-target="#generated-panel"><h3> Generated Gls
  <div.panel-body.collapse id=generated-panel>
    ^{displayGls' "generated-table" newNonNull }
|]
  
displayDetails TransactionSummary{..}  = let
  __use_all_ = TransactionSummary{..}
 in  [whamlet|
<table id=#{tableId} *{forDatatable}>
  <thead>
    <tr>
      <th> StockId
      <th> Unit Price
      <th> Unit Tax
      <th> Quantity
      <th> Discount
      <th> Standard Cost
      <th> Sales Amount
      <th> COGS Amount
  <tfoot>
    <tr>
      <th>
      <th>
      <th>
      <th>
      <th>
      <th>
      <th>
      <th>
  <tbody>
    $forall (Entity _ d) <- tsDetails
      <tr>
        <td> #{debtorTransDetailStockId d}
        $# <td> #{debtorTransDetailDescription d}
        <td> #{showDouble $ debtorTransDetailUnitPrice d}
        <td> #{showDouble $ debtorTransDetailUnitTax d}
        <td> #{showDouble $ debtorTransDetailQuantity d}
        <td> #{showDouble $ debtorTransDetailDiscountPercent d}
        <td> #{showDouble $ debtorTransDetailStandardCost d}
        <td> #{showDouble $ debtorTransDetailSalesAmount d}
        <td> #{showDouble $ debtorTransDetailCOGSAmount d}
|] <> toWidget [julius|
  $(document).ready(function () {
  $(#{toJSON ("table#" <> tableId)}).dataTable( {
    "footerCallback": function(tfoot, data, start, end, display ) {
        var api = this.api();
        for (i of [6,7]) {
        $(api.column(i).footer()).html(
          api.column(i, {filter:"applied"}).data().reduce(function(a,b) { return (parseFloat(a) || 0)+(parseFloat(b)||0);}, 0)
        );
        }
      }
    });
  });
|] where tableId = "table-details" :: Text

displayGls tableId TransactionSummary{..} = displayGls' tableId (map entityVal tsGl)
displayGls' :: Text -> [GlTran] -> Widget
displayGls' tableId tsGl = [whamlet|
<table id=#{tableId} *{forDatatable} data-paging=false>
  <thead>
    <tr>
      <th> Account
      <th> Memo
      <th> Debit
      <th> Credit
      <th> Stock Id
      <th> Dim
      <th> Dim2
      <th> Person Type
      <th> Person
  <tbody>
    $forall gl <- tsGl
      <tr>
        ^{displayGl gl}
  <tfoot>
    <th> Total
    <th>
    <th>
    <th>
    <th>
    <th>
    <th>
    <th>
    <th>

|] <> toWidget [julius|
  $(document).ready(function () {
  $(#{toJSON ("table#" <> tableId)}).dataTable( {
    "footerCallback": function(tfoot, data, start, end, display ) {
        var api = this.api();
        var total = [];
        for (i of [2,3]) {
          total[i] = api.column(i, {filter:"applied"}).data().reduce(function(a,b) { return (parseFloat(a) || 0)+(parseFloat(b)||0);}, 0)
        $(api.column(i).footer()).html( total[i]);
        }
        // $(api.column(6).footer()).html("Balance");
        $(api.column(1).footer()).html(total[3]-total[2]);
      }
    });
  });
|]


displayGl (GlTran{..}) =  [whamlet|
      <td> #{glTranAccount}
        <span.hidden>account:#{glTranAccount}
      <td> #{glTranMemo}
      $if glTranAmount > 0
        <td> #{showDouble glTranAmount}
        <td>
          <span.hidden>debit:
      $else
        $if glTranAmount < 0
          <td>
            <span.hidden>credit:
          <td> #{showDouble $ negate glTranAmount}
        $else
          <td>
          <td>
      <td>
        $maybe stockId <- glTranStockId
          <span.hidden>stockId:
          #{stockId}
      <td> #{tshow glTranDimensionId}
      <td> #{tshow glTranDimension2Id}
      <td> #{maybe "" tshow glTranPersonTypeId}
      <td> #{maybe "" decodeUtf8 glTranPersonId}
|]

displayGlsDiff :: [These (Entity GlTran) GlTran] -> Widget
displayGlsDiff es = do
  [whamlet|
<table.table.table-border.table-hover>
  <tr>
    <th> Status
    <th> Account
    <th> Memo
    <th> Debit
    <th> Credit
    <th> Stock Id
    <th> Dim
    <th> Dim2
    <th> Person Type
    <th> Person
    $forall glt <-  es
       ^{displayGlDiff glt}
|]

displayGlDiff :: These (Entity GlTran) GlTran -> Widget
displayGlDiff glt = case glt of
  This (Entity _ gl) -> [whamlet|
 <tr.bg-warning.text-warning>
    <td> FA Only
    ^{displayGl gl}
    |]
  That gl | glTranAmount gl == 0 -> return ()
  That gl -> [whamlet|
 <tr.bg-warning.text-warning>
    <td> New Only
    ^{displayGl gl}
  |]
  These (Entity _ a) b | glTranAmount a == glTranAmount b ->  [whamlet|
 <tr.bg-success.text-success>
    <td> Ok
    ^{displayGl a}
    |]
  These (Entity _ a) b ->  [whamlet|
 <tr.bg-danger.text-danger>
    <td> FA
    ^{displayGl a}
 <tr.text-info>
    <td> New
    ^{displayGl b}
    |]


debtorTransDetailSalesAmount DebtorTransDetail{..} =
  debtorTransDetailQuantity
  * ( if ?taxIncluded
      then debtorTransDetailUnitPrice - debtorTransDetailUnitTax
      else debtorTransDetailUnitPrice)
  * (1 - debtorTransDetailDiscountPercent)

debtorTransDetailSalesTaxAmount DebtorTransDetail{..} =
  debtorTransDetailQuantity
  * debtorTransDetailUnitTax
  * (1 - debtorTransDetailDiscountPercent)

debtorTransDetailCOGSAmount DebtorTransDetail{..} =
  debtorTransDetailQuantity
  * debtorTransDetailStandardCost

debtorTransDetailDiscountedAmount DebtorTransDetail{..} =
  debtorTransDetailQuantity
  * ( if ?taxIncluded
      then debtorTransDetailUnitPrice - debtorTransDetailUnitTax
      else debtorTransDetailUnitPrice)
  * (debtorTransDetailDiscountPercent)
  
{-# NOINLINE postGLFixDebtorTransR #-}
postGLFixDebtorTransR :: Int64 -> Int64 -> Handler Html
postGLFixDebtorTransR no tType = do
  runDB $ fixDebtorTrans no tType
  -- process
  getGLCheckDebtorTransR no tType

fixDebtorTrans :: Int64 -> Int64 -> SqlHandler ()
fixDebtorTrans no tType =  do
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  let ?taxIncluded = deduceTax
      ?roundMethod = roundfa
  tm <- selectFirst [DebtorTranTransNo ==. fromIntegral no, DebtorTranType ==. fromIntegral tType] []
  t <-  maybe (error $ "Transaction" <> show no <> " not found") loadCustomerSummary tm

  newGls <- generateGLs t
  let glDiffs = alignGls (tsGl t) newGls
      glDiffStatus  = fixGls glDiffs

  -- delete to toDeletes and create toCreates
  mapM_ (delete . entityKey) (glToDelete glDiffStatus)
  insertMany_ (glToCreate glDiffStatus)

  setSuccess ("Transaction has been fixed")
  

-- | Generate expected GL from details
roundfa = RoundAbs (Round 2)
roundWith x = let r = fromRational . toRational $ toDecimalWithRounding ?roundMethod   x
                -- in traceShow("RoundWith", ?roundMethod, x, r) r
                  in r
generateGLs :: (?taxIncluded :: Bool, ?roundMethod :: RoundingMethod ) => TransactionSummary -> SqlHandler [GlTran]
generateGLs summary = roundGl <$$> generateGLs'  summary where
  roundGl GlTran{..} = GlTran{glTranAmount=roundWith glTranAmount,..} where
generateGLs' t@TransactionSummary{..} = do
  glss <- forM tsDetails (\d@(Entity _ DebtorTransDetail{..}) -> do
     stockMaster <- getJust (StockMasterKey debtorTransDetailStockId)
     branch <- getJust (CustBranchKey tsBranchId tsPersonId)
     debtor <- getJust (DebtorsMasterKey tsPersonId)
     return $ case tsTransType of
          ST_CUSTDELIVERY -> generateCustomerDeliveryGls t stockMaster d
          ST_CUSTCREDIT -> generateCustomerCreditGls t debtor branch stockMaster d
          ST_SALESINVOICE -> generateCustomerInvoiceGls t debtor branch stockMaster d
          _ ->  []
                 ) 
  return $ concat glss

generateCustomerDeliveryGls (TransactionSummary{..})
                           StockMaster{..}
                           (Entity _ d@DebtorTransDetail{..}) = let
  glTranTypeNo = tsTransNo
  glTranType = fromEnum tsTransType
  glTranTranDate = tsDate
  glTranMemo = ""
  glTranStockId = Just debtorTransDetailStockId
  glTranDimensionId = fromMaybe 0 stockMasterDimensionId
  glTranDimension2Id = fromMaybe 0 stockMasterDimension2Id
  glTranPersonTypeId = Just 2
  glTranPersonId = Just (fromString $ show tsPersonId)
  cogs = GlTran{..} where
         glTranAccount = stockMasterCogsAccount
         glTranAmount = debtorTransDetailCOGSAmount d
  stock = GlTran{..} where
         glTranAccount = stockMasterInventoryAccount
         glTranAmount = - (debtorTransDetailCOGSAmount d)
  in [cogs, stock]
  
generateCustomerCreditGls (TransactionSummary{..})
                           DebtorsMaster{..}
                           CustBranch{..}
                           StockMaster{..}
                           (Entity _ d@DebtorTransDetail{..}) = let
  glTranTypeNo = tsTransNo
  glTranType = fromEnum tsTransType
  glTranTranDate = tsDate
  glTranMemo = ""
  glTranStockId = Just debtorTransDetailStockId
  glTranDimensionId = fromMaybe 0 stockMasterDimensionId
  glTranDimension2Id = fromMaybe 0 stockMasterDimension2Id
  glTranPersonTypeId = Just 2
  glTranPersonId = Just (fromString $ show tsPersonId)
  sales = GlTran{ glTranDimensionId=debtorsMasterDimensionId
                , glTranDimension2Id=debtorsMasterDimension2Id
                ,..} where
         glTranAccount = stockMasterSalesAccount
         glTranAmount = (debtorTransDetailSalesAmount d)
  -- tax = GlTran{..} where
  --        glTranAccount = "2200"
  --        glTranAmount = (debtorsTransDetailSalesTaxAmount d)
  cogs = GlTran{..} where
         glTranAccount = stockMasterCogsAccount
         glTranAmount = - (debtorTransDetailCOGSAmount d)
  stock = GlTran{..} where
         glTranAccount = stockMasterInventoryAccount
         glTranAmount = (debtorTransDetailCOGSAmount d)
  debt = GlTran{ glTranDimensionId=debtorsMasterDimensionId
               , glTranDimension2Id=debtorsMasterDimension2Id
               , ..} where
         glTranAccount = custBranchReceivablesAccount
         glTranAmount = - (debtorTransDetailSalesAmount d)
  -- debtTax = GlTran{..} where
  --        glTranAccount = custBranchReceivablesAccount
  --        glTranAmount = - (debtorTransDetailSalesTaxAmount d)
  in  [sales, cogs, stock, debt] --- , {-tax , -} debtTax]
  
generateCustomerInvoiceGls (TransactionSummary{..})
                           DebtorsMaster{..}
                           CustBranch{..}
                           StockMaster{..}
                           (Entity _ d@DebtorTransDetail{..}) = let
  glTranTypeNo = tsTransNo
  glTranType = fromEnum tsTransType
  glTranTranDate = tsDate
  glTranMemo = ""
  glTranStockId = Just debtorTransDetailStockId
  glTranDimensionId = debtorsMasterDimensionId
  glTranDimension2Id = debtorsMasterDimension2Id
  glTranPersonTypeId = Just 2
  glTranPersonId = Just (fromString $ show tsPersonId)
  sales = let glTranAccount = stockMasterSalesAccount
          in if debtorTransDetailDiscountPercent /= 0
             then let noDiscount = debtorTransDetailSalesAmount d { debtorTransDetailDiscountPercent = 0}
                      discount =  noDiscount - debtorTransDetailSalesAmount d 
                  in [ GlTran {glTranAmount = - noDiscount, .. }
                     , GlTran {glTranAmount = discount, ..}
                     ]
             else [ GlTran{glTranAmount = - (debtorTransDetailSalesAmount d), .. }
                  ]
  -- we don't check the tax as it should be correct
  -- tax = GlTran{..} where
  --        glTranAccount = "2200"
  --        glTranAmount = - (debtorTransDetailSalesTaxAmount d)
  debt = GlTran{ glTranDimensionId=debtorsMasterDimensionId
               , glTranDimension2Id=debtorsMasterDimension2Id
               , ..} where
         glTranAccount = custBranchReceivablesAccount
         glTranAmount = (debtorTransDetailSalesAmount d)
  -- debtTax = GlTran{..} where
  --        glTranAccount = custBranchReceivablesAccount
  --        glTranAmount = (debtorTransDetailSalesTaxAmount d)
  in  sales ++ [debt]

alignGls :: [Entity GlTran] -> [GlTran] -> [(These (Entity GlTran) GlTran)]
alignGls as bs = let
  as' = rankGls entityVal as
  bs' = rankGls id bs
  toMap getGl xs = Map.fromList $ [ ((glTranStockId, rank, glTranAccount), x)
                            | (x, rank) <- xs
                            , let GlTran{..} = getGl x
                            ]
  am = toMap entityVal as'
  bm = toMap id bs'

  in toList (align am bm)
  
-- | Gives a rank to item with similar stock_id and gl account 
-- sorted by amount
rankGls :: (a -> GlTran) -> [a] -> [(a, Int)]
rankGls getGl gls = let
  grouped = groupAsMap (\a -> let GlTran{..}  = getGl a in  (glTranStockId, glTranAccount)) return gls
  rankGroup gs = zip (sortOn (glTranAmount . getGl) gs) [1..]
  in concat $ toList (rankGroup <$> grouped)


-- | Computes total amount if we were fixing the GL with the new ones
fixedDebit, fixedCredit :: GlDiffStatus -> Double
fixedDebit = sum . map glTranAmount . filter isDebit . glValids
fixedCredit = negate . sum . map glTranAmount . filter (not . isDebit) . glValids 
  
-- | 

-- | extract gl trans to delete, those to create and the on which stays
-- fixGls :: [These GlTran GlTran
data GlDiffStatus = GlDiffStatus
  { glToKeep :: [GlTran]
  , glToDelete :: [Entity GlTran]
  , glToCreate :: [GlTran]
  } deriving Show

deriving instance Show GlTran

glValids :: GlDiffStatus -> [GlTran]
glValids gld = glToKeep gld ++ glToCreate gld
  
fixGls :: [These (Entity GlTran) GlTran] -> GlDiffStatus
fixGls glts = let
  (oks, notOks ) = partitionEithers (map isOk glts)
  isOk (These a b) | glTranAmount (entityVal a) == glTranAmount b = Left b
  isOk t = Right t
  (faOnlys, newOnlys, wrongs) = partitionThese notOks
  (toDeletes, toCreates) =  unzip wrongs

  in GlDiffStatus  (oks ++ map entityVal faOnlys)
                   toDeletes
                   (toCreates ++ newOnlys)
