{-# LANGUAGE OverloadedStrings #-}
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
import Formatting
import Formatting.Time(year, month)
import qualified FA as FA
--------------------------------------------------------------------------------
-- * Form
data ECSLParam = ECSLParam
  { epStartDate :: Day
  , epEndDate :: Day
  , epContactName :: Text
  } deriving Show

data ECSL = ECSL
  { eCountry :: Text
  , eCustomerGST :: Text
  , eCustomerName :: Text
  , eAmount :: Double
  , eIndicator :: Int
  } deriving (Eq, Show)
eValue :: ECSL -> Int
eValue e = floor (eAmount e)
--------------------------------------------------------------------------------
ecslForm paramM = renderBootstrap3 BootstrapBasicForm form where
    form = ECSLParam <$> areq dayField "start" (epStartDate <$> paramM)
                     <*> areq dayField "end" (epEndDate <$> paramM)
                     <*> areq textField "Contact" (epContactName <$> paramM)
-- * Handler
--------------------------------------------------------------------------------
getGLVATR :: Handler Html
getGLVATR = getGLVATEcslR

--------------------------------------------------------------------------------
getGLVATEcslR :: Handler Html
getGLVATEcslR = do
  today <- todayH

  userm <- currentFAUser
  let (epStartDate, epEndDate) = previousVATQuarter today
      epContactName = maybe "" FA.userRealName userm
      param = ECSLParam{..}

  renderGLVATEcslR param mempty


--------------------------------------------------------------------------------
postGLVATEcslR :: Handler TypedContent
postGLVATEcslR = do
  ((resp, formW), encType) <- runFormPost (ecslForm Nothing)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
        ecsls <- loadEcsl param
        actionM <- lookupPostParam "action"
        case actionM of
                      Just "download" -> do
                          vatNumber <- vatNumberH
                          generateCSV vatNumber "000" param  ecsls
                      _ -> toTypedContent <$> renderGLVATEcslR param (renderEcsl ecsls)
  
  
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
    <td.private> (Amount)
    <td> Indicator
  $forall ecsl <- ecsls
    <tr>
      <td.private> #{eCustomerName ecsl}
      <td> #{eCountry ecsl}
      <td> #{eCustomerGST ecsl}
      <td> #{eValue ecsl}
      <td.private> #{formatDouble $ eAmount ecsl}
      <td> #{tshow $ eIndicator ecsl}
  <tr>
    <th.private> Total
    <th>
    <th>
    <th> #{sum (map eValue ecsls)}
    <th> #{formatDouble $ sum (map eAmount ecsls)}
    <th> 
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
      makeEcls (Single _no, Single name, Single tId, Single total) = ECSL (take 2 tId) (drop 2 tId) (decodeHtmlEntities name) total 0

  rows <- runDB $ rawSql sql [PersistDay epStartDate, PersistDay epEndDate]
  return $ map makeEcls rows
 


generateCSV :: Text -> Text -> ECSLParam -> [ECSL] -> Handler TypedContent
generateCSV vatNumber branch  param ecsls = do
  let start =  epStartDate param
  let csv = [ "HMRC_VAT_ESL_BULK_SUBMISSION_FILE"
            , format ("\n"%stext%","%stext%","%year%","%month%",GBP,"%stext%",0")
                         vatNumber branch start start (epContactName param)
            ] ++ map go ecsls
      go e@ECSL{..} = format ("\n"%stext%","%stext%","%int%","%int) eCountry eCustomerGST (eValue e) eIndicator
      source = yieldMany csv
  setAttachment $ format ("ecsl-"%year%"-"%month%".csv") start start
  respondSource ("text/csv") (source .| mapC toFlushBuilder)

       





-- | Retrieve the VAT number from FA settings
vatNumberH :: Handler Text
vatNumberH = do
  pref <- runDB $ get (FA.SysPrefKey "gst_no")
  case pref >>= FA.sysPrefValue of
     (Just vatNumber) -> return vatNumber
     _ -> error "VAT number not configured"
