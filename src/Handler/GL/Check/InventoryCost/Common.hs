module Handler.GL.Check.InventoryCost.Common
( getStockAccounts
, Account(..)
, AccountSummary(..)
, getAccountSummary
)
where

import Import
import Database.Persist.Sql  (rawSql, Single(..))

-- * Types
data Account = Account { fromAccount:: Text } deriving (Eq, Show)

data AccountSummary = AccountSummary
  { asAccount :: Account
  , asGLAmount :: Double
  , asCorrectAmount :: Maybe Double
  , asStockValuation :: Double
  }
  deriving (Show)
-- * Summaries
getStockAccounts :: Handler [Account]
getStockAccounts = do
  let sql = "select distinct(inventory_account) from 0_stock_master"
  rows <- runDB $ rawSql sql []
  return $ map (Account . unSingle) rows

getAccountSummary :: Account -> Handler AccountSummary
getAccountSummary account = do
  asGLAmount <- glBalanceFor account
  asCorrectAmount <- return Nothing
  asStockValuation <- stockValuationFor account
  return AccountSummary{asAccount=account,..}


glBalanceFor :: Account -> Handler Double
glBalanceFor (Account account) = do
 let sql = "select sum(amount) from 0_gl_trans where account = ? "
 rows <- runDB $ rawSql sql [toPersistValue account]
 case rows of
   [] -> return 0
   [(Single total)] -> return $ fromMaybe 0 total
   _ -> error "glBalanceFor should only returns one row. Please contact your Admininstrator"


stockValuationFor :: Account -> Handler Double
stockValuationFor account = do
  sku'costs <- getItemFor account
  values <- mapM stockValuation sku'costs
  return $ sum values

getItemFor :: Account -> Handler [(Text, Double)]
getItemFor (Account account) = do
  let sql = "select stock_id, material_cost from 0_stock_master where inventory_account = ?"
  runDB $ map (bimap unSingle unSingle)  <$> rawSql sql [toPersistValue account]

-- | Stock valuation for 
stockValuation :: (Text, Double) -> Handler Double
stockValuation (sku, cost) = do
  -- defaultLocation <- appFADefaultLocation . appSettings <$> getYesod
  let sql = "select sum(qty) from 0_stock_moves where stock_id = ?"
  rows <- runDB $ rawSql sql [toPersistValue sku]
  case rows of
        [] -> return 0
        [(Single qty)] -> return $ maybe 0 (*cost) qty
        _ -> error "The unpexcted happend"


  

