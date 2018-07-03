module Handler.VAT
( getGLVATR
, getGLVATEcslR
, postGLVATEcslR
) where

import Import
import Database.Persist.MySQL     (MySQLConf (..))
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import GL.Utils
                            
--------------------------------------------------------------------------------
-- * Form
data ECSLParam = ECSLParam
  { epStartDate :: Day
  , epEndDate :: Day
  } deriving Show

data ECSL = ECSL
  { eCountry :: Text
  , eCustomer :: Text
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
  let epStartDate = today
      epEndDate = today 
      param = ECSLParam{..}

  renderGLVATEcslR param mempty


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
    <td> Country
    <td> Customer
    <td> Value
    <td> Indicator
  $forall ecsl <- ecsls
    <tr>
      <td> #{eCountry ecsl}
      <td> #{eCustomer ecsl}
      <td> #{tshow $ eValue ecsl}
      <td> #{tshow $ eIndicator ecsl}
|]
-- * DB
loadEcsl :: ECSLParam -> Handler [ECSL]
loadEcsl param = do
  return [ ECSL "AT" "U23456789" 8601 0
         , ECSL "BE" "1234567890" 617 2
         , ECSL "CY" "12345678X" 126 0
         , ECSL "CZ" "12345678" 567 0
         , ECSL "DE" "123456789" 750312 2
         ]







