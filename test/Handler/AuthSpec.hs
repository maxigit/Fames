module Handler.AuthSpec (spec) where

import TestImport
import Yesod.Auth (Route(LoginR))

logAsAdmin = do
     get (AuthR LoginR)
     request $ do
       setMethod "POST"
       setUrl ("auth/page/fa/login" :: String)
       addToken_ "form#login-form"
       addPostParam "username" "admin"
       addPostParam "password" "wadmin"

spec :: Spec
spec = withAppNoDB $ do
  it "not logged" $ do
    get AdministratorR
    statusIs 303
    followRedirect
    bodyContains "username"

  it  "logging" $ do
     get (AuthR LoginR)
     printBody
     statusIs 200
     htmlCount "input[name=username]" 1
     htmlCount "input[name=password]" 1
     -- log detail

     logAsAdmin

     printBody
     statusIs 200
     bodyContains "You are logged in"

  it "logged as administrator" $ do
    logAsAdmin
   
    get AdministratorR
    statusIs 200

    bodyContains "You are logged as Administrator"


     

