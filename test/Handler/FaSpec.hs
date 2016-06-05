module Handler.FaSpec (spec) where

import TestImport

spec :: Spec
spec = appSpec

appSpec :: Spec
appSpec = withApp $ do
  describe "FA bindings" $ do
    it "displays list of bank accounts" $ do
      get FABankAccountsR
      statusIs 401

      bodyContains "current account"

    it "displays list of users" $ do
      get FAUsersR 
      statusIs 200

      bodyContains "admin"
    
