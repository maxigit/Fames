module Handler.Login where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(..), renderBootstrap3)

-- | Login page
getLoginR' :: Handler Html
getLoginR' = do
  (login_form, postEncType) <- generateFormPost loginForm
  defaultLayout $ [whamlet|
<h1> Login
<form #login role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{postEncType}>
  ^{login_form}
  <button type="submint" .btn .btn-defaunt>Submit
|]

loginForm = renderBootstrap3 BootstrapBasicForm
  ( (,)
    <$> areq textField "username" Nothing
    <*> areq passwordField "password" Nothing
  )

postLoginR' :: Handler Html
postLoginR' = do
  ((loginResp, loginW), enctype) <- runFormPost loginForm
  case loginResp of
    FormMissing -> error "missing"
    FormFailure a -> error ("Form failure" ++ show a )
    (FormSuccess (login, password)) -> do
       verifyLogin login password
       redirect $ GLEnterReceiptSheetR


verifyLogin :: Text -> Text -> Handler Bool
verifyLogin login passwod = do
  return True