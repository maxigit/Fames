module Handler.FaSpec (spec) where

import TestImport

spec :: Spec
spec = appSpec

appSpec :: Spec
appSpec = withAppNoDB BypassAuth $ do
  describe "FA bindings" $ do
    it "displays list of bank accounts" $ do
      get (FA'R FABankAccountsR)
      statusIs 200

      bodyContains "Current account"

    it "displays list of users" $ do
      get (FA'R FAUsersR )
      statusIs 200

      bodyContains "admin"
    
