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
  defaultLayout 
    [whamlet|
      <ul>
        $forall account <- accounts
         <li> #{account}
    |]


