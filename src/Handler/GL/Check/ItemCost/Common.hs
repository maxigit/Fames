module Handler.GL.Check.ItemCost.Common
( getStockAccounts
, Account(..)
, AccountSummary(..)
, getAccountSummary
, loadPendingTransactionCountFor
)
where

import Import
import Database.Persist.Sql  (rawSql, Single(..))
import qualified FA as FA

-- * Types
data Account = Account { fromAccount:: Text } deriving (Eq, Show)

data AccountSummary = AccountSummary
  { asAccount :: Account
  , asAccountName :: Text
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
  asAccountName <- getAccountName account
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
   _ -> error "glBalanceFor should only returns one row. Please contact your Admininstrator!"


getAccountName :: Account -> Handler Text
getAccountName (Account account) = do
  let sql = "select account_name from 0_chart_master where account_code = ?"
  rows <- runDB $ rawSql sql [toPersistValue account]
  case rows of
   [(Single name)] -> return $ decodeHtmlEntities name
   _ -> error "The unexpected happened. Contact your Administrator!"

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


  

-- * Account  details
-- Load And join stock moves & gl trans of a given account and sku
-- If the sku is not provided, load the gl trans which doesn't have the stock_id set.
-- ONly load transaction not saved in inventory_cost_transaction
loadMovesAndTransactions :: Account -> Maybe Text -> Handler [These (Entity FA.StockMove) (Entity FA.GlTran)]  
loadMovesAndTransactions (Account account) Nothing = do
  let sql = "SELECT ?? FROM 0_gl_trans "
         <> " LEFT JOIN check_item_cost_transaction i ON (0_gl_trans.counter = i.gl_detail) "
         <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id IS NULL"
         <> "   AND i.item_cost_transaction_id is NULL "
         <> " ORDER BY 0_gl_trans.tran_date, 0_gl_trans.counter"
         <> " LIMIT 100" -- TODO remove
      mkTrans = That
  rows <- runDB $ rawSql sql [toPersistValue account]
  return $ map mkTrans rows

loadMovesAndTransactions (Account account) (Just sku) = do
  let sql = "SELECT ??, ?? FROM 0_stock_moves "
         <> " LEFT JOIN 0_gl_trans ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
         <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
         <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
         <> "                            )"
         <> " LEFT JOIN check_item_cost_transaction i ON (0_stock_moves.trans_id = move_id OR 0_gl_trans.counter = gl_detail) "
         <> " WHERE 0_gl_trans.account = ? AND 0_stock_moves.stock_id =  ? "
         <> "   AND i.item_cost_transaction_id is NULL "
         <> " ORDER BY 0_stock_moves.tran_date, 0_stock_moves.trans_id"
         <> " LIMIT 100" -- TODO remove
      mkTrans m'g = case m'g of
                      (m, Nothing ) -> This m
                      (m, Just g ) -> These m g
  rows <- runDB $ rawSql sql [toPersistValue account, toPersistValue sku]
  return $ map mkTrans rows



{-
loadMovesAndTransactionsFor :: Account -> Handler()
loadMovesAndTransactionsFor account = do
  sku'_s <- getItemFor account
  let skums = Nothing : map Just sku'_s
  mapM_ computesTransactionCheck skums
  -}

loadPendingTransactionCountFor :: Account -> Handler [(Maybe Text, Int)]
loadPendingTransactionCountFor account = do
  sku'_s <- getItemFor account
  let skums = Nothing : map (Just . fst) sku'_s
  forM skums $ \skum -> do
    trans <- loadMovesAndTransactions account skum
    return (skum, length trans)






  

