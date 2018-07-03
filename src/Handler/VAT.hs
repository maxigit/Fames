module Handler.VAT
( getGLVATR
, getGLVATEcslR
, postGLVATEcslR
) where

import Import
import Database.Persist.MySQL     (Single(..), rawSql)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import GL.Utils
import GL.Payroll.Settings
                            
--------------------------------------------------------------------------------
-- * Form
data ECSLParam = ECSLParam
  { epStartDate :: Day
  , epEndDate :: Day
  } deriving Show

data ECSL = ECSL
  { eCountry :: Text
  , eCustomerGST :: Text
  , eCustomerName :: Text
  , eValue :: Int
  , eIndicator :: Int
  } deriving (Eq, Show)
--------------------------------------------------------------------------------
ecslForm paramM = renderBootstrap3 BootstrapBasicForm form where
    form = ECSLParam <$> areq dayField "start" (epStartDate <$> paramM)
                     <*> areq dayField "end" (epEndDate <$> paramM)
-- * Handler
--------------------------------------------------------------------------------
getGLVATR :: Handler Html
getGLVATR = getGLVATEcslR

--------------------------------------------------------------------------------
getGLVATEcslR :: Handler Html
getGLVATEcslR = do
  today <- todayH
  let (epStartDate, epEndDate) = previousVATQuarter today
      param = ECSLParam{..}

  renderGLVATEcslR param mempty

previousVATQuarter :: Day -> (Day, Day)          
previousVATQuarter day =  (start, end) where
  begMonth = calculateDate BeginningOfMonth day
  (__year, month, _day) = toGregorian begMonth
      -- we need to find the last full quarter before today
      -- VAT quarter at the beginning of the year
  monthOffset = (month- 1) `mod` 3
  start = calculateDate (AddMonths (-monthOffset-3))  begMonth
  end = foldr (calculateDate) start [EndOfMonth, AddMonths 2]


--------------------------------------------------------------------------------
postGLVATEcslR :: Handler Html
postGLVATEcslR = do
  ((resp, formW), encType) <- runFormPost (ecslForm Nothing)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
        ecsls <- loadEcsl param
        actionM <- lookupPostParam "action"
        case actionM of
          Just "download" -> error "TODO" -- param
          _ -> renderGLVATEcslR param (renderEcsl ecsls)
  
  
--------------------------------------------------------------------------------
-- * Render
renderGLVATEcslR :: ECSLParam -> Widget -> Handler Html
renderGLVATEcslR param result = do
  (form, encType) <- generateFormPost (ecslForm $ Just param)
  defaultLayout [whamlet|
   <div.well>
     <form.form.form-inline action="@{GLR GLVATEcslR}" method=POST enctype="#{encType}">
       ^{form}
       <button.btn.btn-primary>Submit
       <button.btn.btn-success name="action" value="download">Download
   <div.panel.panel-primary>
     <div.panel-heading>
       <h3> ECSL
     <div.panel-body>
      ^{result}
  |]

renderEcsl :: [ECSL] -> Widget
renderEcsl ecsls = [whamlet|
<table.table-border.table.hover.table-striped>
  <tr>
    <td.private> Name
    <td> Country
    <td> Customer
    <td> Value
    <td> Indicator
  $forall ecsl <- ecsls
    <tr>
      <td.private> #{eCustomerName ecsl}
      <td> #{eCountry ecsl}
      <td> #{eCustomerGST ecsl}
      <td> #{tshow $ eValue ecsl}
      <td> #{tshow $ eIndicator ecsl}
|] <> toWidget [cassius|
  td.private
    font-style: italic
    opacity: 0.5

               |]
-- * DB
loadEcsl :: ECSLParam -> Handler [ECSL]
loadEcsl ECSLParam{..} = do
  let sql = "SELECT d.debtor_no, d.name AS cust_name, d.tax_id, "
          <> " SUM(CASE WHEN dt.type=" <> (tshow $ fromEnum ST_CUSTCREDIT) <> " THEN (ov_amount+ov_freight+ov_discount)*-1 "
          <> "    ELSE (ov_amount+ov_freight+ov_discount) "
          <> " END *dt.rate) AS total"
          <> " FROM 0_debtor_trans dt"
          <> " LEFT JOIN 0_debtors_master d ON d.debtor_no=dt.debtor_no "
          <> "  WHERE (dt.type=" <> ( tshow $ fromEnum ST_SALESINVOICE) <> " OR dt.type=" <> (( tshow $ fromEnum ST_CUSTCREDIT)) <> ")" 
          <> "  AND tax_id <> '' AND dt.tran_date >= ? AND dt.tran_date <= ? "
          <> "  GROUP BY debtor_no"

      makeEcls :: (Single Int64, Single Text, Single Text, Single Double) -> ECSL
      makeEcls (Single _no, Single name, Single tId, Single total) = ECSL (take 2 tId) (drop 2 tId) (decodeHtmlEntities name) (floor total) 0

  rows <- runDB $ rawSql sql [PersistDay epStartDate, PersistDay epEndDate]
  return $ map makeEcls rows
 







