{-# LANGUAGE QuasiQuotes #-}
module Handler.FA where

import Import
import FA

getFAUsersR :: Handler Html
getFAUsersR = do
  users <- runDB $ selectList [] []

  defaultLayout [whamlet|
<table>
  <tr>
    <th> Id
    <th> RealName
    <th> Email
  $forall Entity uid user <- users
    <tr>
      <td> #{tshow uid}
      <td> #{tshow $ userLogin user}
      <td> #{tshow $ userRealName user}
      <td> #{tshow $ userEmail user}
|]



getFABankAccountsR :: Handler ()
getFABankAccountsR = return ()
