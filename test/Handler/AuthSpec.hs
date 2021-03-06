module Handler.AuthSpec (spec) where

import TestImport
import Yesod.Auth (Route(LoginR))

spec :: Spec
spec = withAppNoDB CheckAuth $ do
  it "not logged" $ do
    get (AdministratorR AIndexR)
    statusIs 303
    followRedirect
    bodyContains "username"

  it "ask for logging details on login page" $ do
     get (AuthR LoginR)
     statusIs 200
     htmlCount "input[name=username]" 1
     htmlCount "input[name=password]" 1

  it  "logging" $ do
     logAsAdmin
     statusIs 303
     followRedirect
     statusIs 200
     bodyContains "You are now logged in"

  it "logged as administrator" $ do
    logAsAdmin

    get (AdministratorR AIndexR)
    statusIs 200
    bodyContains "You are logged as Administrator"

