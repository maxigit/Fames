module Handler.FaSpec (spec) where

import TestImport

spec :: Spec
spec = appSpec

appSpec :: Spec
appSpec = withAppNoDB $ do
  describe "FA bindings" $ do
    it "displays list of bank accounts" $ do
      get FABankAccountsR
      statusIs 200

      bodyContains "Current account"

    it "displays list of users" $ do
      get FAUsersR 
      statusIs 200

      bodyContains "admin"
    
