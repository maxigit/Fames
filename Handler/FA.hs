{-# LANGUAGE QuasiQuotes #-}
module Handler.FA where

import Import
import FA

getFAUsersR :: Handler Html
getFAUsersR = do
  entities <- runDB $ selectList [] []
  let users = entities :: [Entity FA.User]
  
  defaultLayout $ toWidget (renderEntities getDBName entities)



getFABankAccountsR :: Handler Html
getFABankAccountsR = do
  entities <- runDB $ selectList [] []
  let accounts = entities :: [Entity FA.BankAccount]

  defaultLayout $ toWidget (renderEntities getDBName entities)
