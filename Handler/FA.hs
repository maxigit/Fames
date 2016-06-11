{-# LANGUAGE QuasiQuotes #-}
module Handler.FA where

import Import
import FA

getFAUsersR :: Handler Html
getFAUsersR = do
  users <- runDB $ selectList [] []
  let m = Nothing :: Maybe FA.User
  let u = users :: [Entity FA.User]
  let def = entityDef m
  

  defaultLayout [whamlet|
div #{tshow def}
<table>
  <tr>
    $forall field <- entityFields def
      <th> #{unDBName $ fieldDB field}
  $forall Entity uid user <- users
    <tr>
      $forall some <- toPersistFields user
        <td> #{show $ toPersistValue some}
      <td> #{tshow uid}
|]



getFABankAccountsR :: Handler Html
getFABankAccountsR = do
  entities <- runDB $ selectList [] []
  let m = Nothing :: Maybe FA.BankAccount
  let types = entities :: [Entity FA.BankAccount]
  let def = entityDef m
  
  defaultLayout [whamlet|
div #{tshow def}
<table>
  <tr>
    $forall field <- entityFields def
      <th> #{unDBName $ fieldDB field}
  $forall Entity rid record <- entities
    <tr>
      $forall some <- toPersistFields record
        <td> #{show $ toPersistValue some}
      <td> #{tshow rid}
|]
