{-# LANGUAGE ImplicitParams #-} 
{-# LANGUAGE StandaloneDeriving #-}
module Handler.GL.Check
( getGLCheckR 
, postGLCheckR 
, getGLCheckDebtorTransR
, postGLFixDebtorTransR
) where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import FA
import Handler.Util
import Data.These
import Data.Align(align)
import qualified Data.Map as Map

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
                _ -> \x -> debtorTransDetailSalesAmount  x + debtorTransDetailSalesTaxAmount x
  

tsTotalDebit t = sum [  glTranAmount 
                     | Entity _ gl@GlTran{..} <- tsGl t
                     , isDebit gl 
                     ]
tsTotalCredit t = sum [  - glTranAmount 
                     | Entity _ gl@GlTran{..} <- tsGl t
                     , not (isDebit gl)
                     ]
isDebit GlTran{..} = glTranAmount > 0

-- | How much should the details adds too. It depends on the transaction types
tsExpectedDetail t = case tsTransType t of
  ST_CUSTDELIVERY -> tsTotalDetails t -- only COGS side of the sales
  ST_CUSTCREDIT -> tsTotalDetails t -- only COGS side of the sales
  ST_CUSTPAYMENT -> 0 -- no details
  _ -> tsNet t

tsExpectedDebit t = case tsTransType t of
  ST_CUSTDELIVERY -> tsTotalDetails t
  ST_CUSTCREDIT -> tsTotalDetails t {- cogs -} + tsTotal t -- refund customer
  _ -> tsTotal t 

tsRoute TransactionSummary{..} = GLR (f (fromIntegral tsTransNo) (fromIntegral $ fromEnum tsTransType)) where
  f = case tsTransType of
        ST_CUSTPAYMENT -> GLCheckDebtorTransR 
        ST_SALESINVOICE -> GLCheckDebtorTransR 
        ST_CUSTDELIVERY -> GLCheckDebtorTransR 
        ST_CUSTCREDIT -> GLCheckDebtorTransR 
        _ -> GLCheckDebtorTransR 
  
near a b = abs (a - b) < 1e-2
tsIsValid t = and [ tsExpectedDetail t `near` tsTotalDetails t
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

getGLCheckR :: Handler Html
getGLCheckR = renderCheck defaultParam Nothing

itext t = t :: Text
postGLCheckR :: Handler Html
postGLCheckR = do
  ((resp, formW), enctype) <- runFormPost (checkForm Nothing)
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  let ?taxIncluded = deduceTax
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      alls <- checkAll param
      let w = [whamlet|
<table.table.table-border.table-hover.table-striped>
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
            #{itext (showTransType $ tsTransType t) }
        <td> #{tshow $ tsDate t}
        <td> #{tshow $ tsNet t}
        <td> #{tshow $ tsTax t}
        <td> #{tshow $ tsShippingNet t}
        <td> #{showDouble $ tsTotal t}
        <td> #{showWithStatus (tsExpectedDetail t)  (tsTotalDetails t) }
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

tsFromDebtor (Entity _ DebtorTran{..}) details gls =
  TransactionSummary debtorTranTransNo (toEnum debtorTranType) debtorTranTranDate
                     (fromMaybe (error $ "Debtor no should be set for " ++ show debtorTranType ++ " #" ++ show debtorTranTransNo)
                                debtorTranDebtorNo)
                     debtorTranBranchCode
                     (debtorTranOvAmount - debtorTranOvDiscount)
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
getGLCheckDebtorTransR :: Int64 -> Int64 -> Handler Html
getGLCheckDebtorTransR no tType = do
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  let ?taxIncluded = deduceTax
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  t <- runDB $ do
    tm <- selectFirst [DebtorTranTransNo ==. fromIntegral no, DebtorTranType ==. fromIntegral tType] []
    maybe (error $ "Transaction" <> show no <> " not found") loadCustomerSummary tm
  newGls <- runDB $ generateGLs t
  let (newDebits, newCredits) = partition isDebit newGls
      newDebit = sum (map glTranAmount newDebits)
      newCredit = negate $ sum (map glTranAmount newCredits)
      glDiffs = alignGls (map entityVal (tsGl t)) newGls
      glDiffStatus  = fixGls glDiffs
  defaultLayout' [whamlet|
<div.panel.panel-info>
  <div.panel-heading><h3>
    #{itext $ showTransType (tsTransType t)}
    <a href="#{urlForFA faUrl (tsTransType t) (tsTransNo t)}">
      ##{tshow (tsTransNo t)}
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
        <td> #{showWithStatus (tsExpectedDetail t)  (tsTotalDetails t) }
      <tr>
        <th> Debit
        <td> #{showWithStatus (tsExpectedDebit t) (tsTotalDebit t) }
      <tr>
        <th> Credit
        <td> #{showWithStatus (tsExpectedDebit t) (tsTotalCredit t) }
      <tr>
        <th> Generated Debit
        <td> #{showWithStatus (tsExpectedDebit t) (newDebit) }
      <tr>
        <th> Generated Credit
        <td> #{showWithStatus (tsExpectedDebit t) (newCredit) }
      <tr>
        <th> Fixed Debit
        <td> #{showWithStatus (tsExpectedDebit t) (fixedDebit glDiffStatus) }
      <tr>
        <th> Fixed Credit
        <td> #{showWithStatus (tsExpectedDebit t) (fixedCredit glDiffStatus) }
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
    ^{displayGls t}
<div.panel.panel-info>
  <div.panel-heading data-toggle=collapse data-target="#generated-panel"><h3> Generated Gls
  <div.panel-body.collapse id=generated-panel>
    ^{displayGls' newGls}
|]
  
displayDetails TransactionSummary{..}  = [whamlet|
<table.table.table-border.table-hover.table-striped>
  <tr>
    <th> StockId
    <th> Unit Price
    <th> Unit Tax
    <th> Quantity
    <th> Discount
    <th> Standard Cost
    <th> Sales Amount
    <th> COGS Amount
  $forall (Entity _ d@DebtorTransDetail{..}) <- tsDetails
    <tr>
      <td> #{debtorTransDetailStockId}
      $# <td> #{debtorTransDetailDescription}
      <td> #{showDouble debtorTransDetailUnitPrice}
      <td> #{showDouble debtorTransDetailUnitTax}
      <td> #{showDouble debtorTransDetailQuantity}
      <td> #{showDouble debtorTransDetailDiscountPercent}
      <td> #{showDouble debtorTransDetailStandardCost}
      <td> #{showDouble $ debtorTransDetailSalesAmount d}
      <td> #{showDouble $ debtorTransDetailCOGSAmount d}
|]

displayGls TransactionSummary{..} = displayGls' (map entityVal tsGl)
displayGls' tsGl = [whamlet|
<table.table.table-border.table-hover.table-striped>
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
  $forall gl <- tsGl
    <tr>
      ^{displayGl gl}
|]


displayGl (GlTran{..}) =  [whamlet|
      <td> #{glTranAccount}
      <td> #{glTranMemo}
      $if glTranAmount > 0
        <td> #{showDouble glTranAmount}
        <td>
      $else
        $if glTranAmount < 0
          <td>
          <td> #{showDouble $ negate glTranAmount}
        $else
          <td>
          <td>
      <td> #{fromMaybe "" glTranStockId}
      <td> #{tshow glTranDimensionId}
      <td> #{tshow glTranDimension2Id}
      <td> #{maybe "" tshow glTranPersonTypeId}
      <td> #{maybe "" decodeUtf8 glTranPersonId}
|]

displayGlsDiff :: [These GlTran GlTran] -> Widget
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

displayGlDiff :: These GlTran GlTran -> Widget
displayGlDiff glt = case glt of
  This gl -> [whamlet|
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
  These a b | glTranAmount a == glTranAmount b ->  [whamlet|
 <tr.bg-success.text-success>
    <td> Ok
    ^{displayGl a}
    |]
  These a b ->  [whamlet|
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

postGLFixDebtorTransR :: Int64 -> Int64 -> Handler Html
postGLFixDebtorTransR no tType = do
  -- process
  getGLCheckDebtorTransR no tType

-- | Generate expected GL from details
generateGLs :: (?taxIncluded :: Bool) => TransactionSummary -> SqlHandler [GlTran]
generateGLs t@TransactionSummary{..} = do
  glss <- forM tsDetails (\d@(Entity _ detail@DebtorTransDetail{..}) -> do
     stockMaster <- getJust (StockMasterKey debtorTransDetailStockId)
     branch <- getJust (CustBranchKey tsPersonId tsBranchId)
     debtor <- getJust (DebtorsMasterKey tsPersonId)
     return $ case tsTransType of
          ST_CUSTDELIVERY -> generateCustomerDeliveryGls t stockMaster d
          ST_CUSTCREDIT -> generateCustomerCreditGls t debtor branch stockMaster d
          -- ST_SALESINVOICE -> generateSalesinvoiceGL stockMaster detail
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
  

alignGls :: [GlTran] -> [GlTran] -> [(These GlTran GlTran)]
alignGls as bs = let
  as' = rankGls as
  bs' = rankGls bs
  toMap xs = Map.fromList $ [ ((glTranStockId, rank, glTranAccount), gl)
                            | (gl@GlTran{..}, rank) <- xs
                            ]
  am = toMap as'
  bm = toMap bs'

  in toList (align am bm)
  
-- | Gives a rank to item with similar stock_id and gl account 
-- sorted by amount
rankGls :: [GlTran] -> [(GlTran, Int)]
rankGls gls = let
  grouped = groupAsMap (\GlTran{..} -> (glTranStockId, glTranAccount)) return gls
  rankGroup gs = zip (sortOn glTranAmount gs) [1..]
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
  , glToDelete :: [GlTran]
  , glToCreate :: [GlTran]
  } deriving Show

deriving instance Show GlTran

glValids :: GlDiffStatus -> [GlTran]
glValids gld = glToKeep gld ++ glToCreate gld
  
fixGls :: [These GlTran GlTran] -> GlDiffStatus
fixGls glts = let
  (oks, notOks ) = partitionEithers (map isOk glts)
  isOk (These a b) | glTranAmount a == glTranAmount b = Left a
  isOk t = Right t
  (wrongs, (faOnlys, newOnlys)) = partitionThese notOks
  (toDeletes, toCreates) = unzip wrongs

  in traceShowId $ GlDiffStatus  (oks ++ faOnlys)
                   toDeletes
                   (toCreates ++ newOnlys)



  
