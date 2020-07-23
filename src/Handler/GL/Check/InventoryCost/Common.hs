module Handler.GL.Check.InventoryCost.Common
(
getStockAccounts
)
where

import Import

-- * Summaries
getStockAccounts :: Handler [Text]
getStockAccounts = do
  return ["1001", "1002", "Todo"]
