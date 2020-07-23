module Handler.GL.Check.InventoryCost
(
getGLCheckInventoryCostR

)
where

import Import
import Handler.GL.Check.InventoryCost.Common

getGLCheckInventoryCostR :: Handler Html
getGLCheckInventoryCostR = do
  accounts <- getStockAccounts
  summaries <- mapM getAccountSummary accounts
  defaultLayout 
    [whamlet|
      <table *{datatable}>
        <thead>
          <tr>
            <td> Account
            <td> GL Balance
            <td> Correct Balance
            <td> Stock Valuation
        <tbody>
          $forall AccountSummary{..} <- summaries
           <tr>
            <td> #{fromAccount asAccount}
            <td> #{formatDouble asGLAmount}
            <td> #{maybe "" formatDouble asCorrectAmount}
            <td> #{formatDouble asStockValuation}
    |]


