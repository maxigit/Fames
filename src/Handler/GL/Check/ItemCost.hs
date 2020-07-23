module Handler.GL.Check.ItemCost
( getGLCheckItemCostR
, getGLCheckItemCostAccountViewR

)
where

import Import
import Handler.GL.Check.ItemCost.Common

getGLCheckItemCostR :: Handler Html
getGLCheckItemCostR = do
  accounts <- getStockAccounts
  summaries <- mapM getAccountSummary accounts
  defaultLayout 
    [whamlet|
      <table *{datatable}>
        <thead>
          <tr>
            <th> Account
            <th> GL Balance
            <th> Correct Balance
            <th> Stock Valuation
        <tbody>
          $forall AccountSummary{..} <- summaries
           <tr>
            <td> 
               <a href=@{GLR $ GLCheckItemCostAccountViewR (fromAccount asAccount)}>
                 #{fromAccount asAccount} - #{asAccountName}
            <td> #{formatDouble asGLAmount}
            <td> #{maybe "" formatDouble asCorrectAmount}
            <td> #{formatDouble asStockValuation}
    |]


getGLCheckItemCostAccountViewR :: Text -> Handler Html
getGLCheckItemCostAccountViewR account = do
  sku'counts <- loadPendingTransactionCountFor (Account account)
  defaultLayout 
    [whamlet|
     <table *{datatable}>
      <thead>
        <th> Stock Id
        <th> Unchecked moves
      <tbody>
        $forall (sku, count) <- sku'counts
          <tr>
            <td> #{fromMaybe "" sku}
            <td> #{tshow count}
    |]

