module Handler.AuthSpec (spec) where

import TestImport

logAsAdmin = do
     get HomeR
     request $ do
       setMethod "POST"
       addToken_ "form#text-form"
       byLabel "Login" "admin"
       byLabel "password" "wadmin"

spec :: Spec
spec = withAppNoDB $ do
  it "not logged" $ do
    get AdministratorR
    statusIs 403

  it  "logging" $ do
     get LoginR'
     statusIs 200
     bodyContains "Please login"
     -- log detail

     request $ do
       setMethod "POST"
       addToken_ "form#text-form"
       byLabel "Login" "admin"
       byLabel "password" "wadmin"

     statusIs 200

  it "logged as administrator" $ do
    logAsAdmin
   
    get AdministratorR
    statusIs 200

    bodyContains "You are logged as Administrator"


     

