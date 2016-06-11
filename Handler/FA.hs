{-# LANGUAGE QuasiQuotes #-}
module Handler.FA
( module Handler.FA
, module Handler.FA.Def
) where

import Import
import FA
import Handler.FA.Def
{-
getFAUsersR :: Handler Html
getFAUsersR = do
  entities <- runDB $ selectList [] []
  let users = entities :: [Entity FA.User]
  
  defaultLayout $ toWidget (entitiesToTable getDBName entities)



getFABankAccountsR :: Handler Html
getFABankAccountsR = do
  entities <- runDB $ selectList [] []
  let accounts = entities :: [Entity FA.BankAccount]

  defaultLayout $ toWidget (entitiesToTable  getDBName entities)
-}
