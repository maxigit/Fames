module Handler.GL.Check.ItemCost.Common
( getStockAccounts
, Account(..)
, AccountSummary(..)
, getAccountSummary
, loadPendingTransactionCountFor
, loadMovesAndTransactions
, computeItemCostTransactions
, collectCostTransactions
, loadLastTransaction
)
where

import Import
import Database.Persist.Sql  (rawSql, Single(..))
import qualified FA as FA
import qualified Data.Map as Map
import Lens.Micro.Extras (preview)
import Data.Monoid(First(..))
import Data.List(scanl')

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
  asCorrectAmount <- getTotalCost account
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


getTotalCost :: Account -> Handler (Maybe Double)
getTotalCost (Account account) = do
  let sql = "select sum(stock_value) from check_item_cost_transaction "
         <> " WHERE account = ?"
         <> " AND is_last is not null"
  rows <- runDB $ rawSql sql [toPersistValue account]
  case rows of
    [] -> return Nothing
    [(Single total)] -> return $ total
    _ -> error "glTotalCost should only returns one row. Please contact your Admininstrator!"

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
loadMovesAndTransactions :: (Maybe ItemCostTransaction) -> Account -> Maybe Text -> Handler [These (Entity FA.StockMove) (Entity FA.GlTran)]  
loadMovesAndTransactions lastm (Account account) Nothing = do
  let maxGl = lastm >>= itemCostTransactionMoveId
  let sql = "SELECT ?? FROM 0_gl_trans "
         -- <> " LEFT JOIN check_item_cost_transaction i ON (0_gl_trans.counter = i.gl_detail) "
         <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id IS NULL"
         -- <> "   AND i.item_cost_transaction_id is NULL "
         <> ( if isJust maxGl
              then  "                      AND 0_gl_trans.counter > ?" 
              else "" )
         <> " ORDER BY 0_gl_trans.tran_date, 0_gl_trans.counter"
      mkTrans = That
  rows <- runDB $ rawSql sql $ [toPersistValue account]
                               ++ maybe [] (pure . toPersistValue) maxGl
  return $ map mkTrans rows

loadMovesAndTransactions lastm account (Just sku) = do
   withMoves <- loadMovesAndTransactions' lastm account sku
   glOnly <- loadTransactionsWithNoMoves' lastm account sku
   -- interleave two sort lists as sorted
   let interleave [] ys = ys
       interleave xs [] = xs
       interleave (x:xs) ys@(y:_) | x `before` y = x : interleave xs  ys
       interleave xs (y:ys) = y : interleave xs  ys
       -- compare date then gl counter
      
       get fa fb = mergeTheseWith (fa . entityVal) (fb . entityVal) const
       before a b = key a < key b
        where key x =  ( get FA.stockMoveTranDate FA.glTranTranDate x
                       , entityKey <$> preview there x
                       , entityKey <$> preview here x
                       )
   return $ interleave withMoves glOnly


loadMovesAndTransactions' lastm (Account account) sku = do
  -- instead of checking if there is already an item in check_item_cost_transaction
  -- we filter moves and gl trans by only using the one
  -- newer than the ids found in the last ItemCostTransaction
  -- ids in table are sequential, so it should work.
  -- This is much faster than the initial behavior which is slow
  -- even though it's is using index (BTree)
  let maxGl = lastm >>= itemCostTransactionMoveId
      maxMove = lastm >>= itemCostTransactionMoveId
      sql = "SELECT ??, ?? FROM 0_stock_moves "
         <> " LEFT JOIN 0_gl_trans ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
         <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
         <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
         <> ( if isJust maxGl
              then  "                      AND 0_gl_trans.counter > ?" 
              else "" )
         <> "                            )"
         -- <> " LEFT JOIN check_item_cost_transaction i ON (0_stock_moves.trans_id = move_id OR 0_gl_trans.counter = gl_detail) "
         <> " WHERE (0_gl_trans.account = ? OR 0_gl_trans.account is NULL) AND 0_stock_moves.stock_id =  ? "
         -- <> "   AND i.item_cost_transaction_id is NULL "
         <> "   AND 0_stock_moves.qty <> 0"
         <> (if isJust maxMove
            then  "                      AND 0_stock_moves.trans_id > ?" 
            else "")
         <> " ORDER BY 0_stock_moves.tran_date, 0_stock_moves.trans_id"
      mkTrans m'g = case m'g of
                      (m, Nothing ) -> This m
                      (m, Just g ) -> These m g
  rows <- runDB $ rawSql sql $ maybe [] (pure . toPersistValue) maxGl
                               ++ [ toPersistValue account
                                  , toPersistValue sku
                                  ]
                                ++ maybe [] (pure . toPersistValue) maxMove
  return $ map mkTrans rows

loadTransactionsWithNoMoves' lastm (Account account) sku = do
  let maxGl = lastm >>= itemCostTransactionMoveId
      sql = "SELECT ?? FROM 0_gl_trans "
         <> " LEFT JOIN 0_stock_moves ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
         <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
         <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
         <> "                            AND 0_stock_moves.qty <> 0"
         <> "                            )"
         -- <> " LEFT JOIN check_item_cost_transaction i ON (0_gl_trans.counter = i.gl_detail) "
         <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id = ? "
         -- <> "   AND i.item_cost_transaction_id IS NULL"
         <> "   AND 0_stock_moves.stock_id IS NULL "
         <> ( if isJust maxGl
              then  "                      AND 0_gl_trans.counter > ?" 
              else "" )
         <> " ORDER BY 0_gl_trans.tran_date, 0_gl_trans.counter"
      mkTrans = That
  rows <- runDB $ rawSql sql $ [toPersistValue account, toPersistValue sku]
                               ++ maybe [] (pure . toPersistValue) maxGl
                        
  return $ map mkTrans rows


{-
loadMovesAndTransactionsFor :: Account -> Handler()
loadMovesAndTransactionsFor account = do
  sku'_s <- getItemFor account
  let skums = Nothing : map Just sku'_s
  mapM_ computesTransactionCheck skums
  -}

loadPendingTransactionCountFor :: Account -> Handler [(Maybe Text, Int, Maybe ItemCostTransaction)]
loadPendingTransactionCountFor account = do
  sku'_s <- getItemFor account
  let skums = Nothing : map (Just . fst) sku'_s
  forM skums $ \skum -> do
    lastm <- loadLastTransaction account skum
    trans <- loadMovesAndTransactions (entityVal <$> lastm) account skum
    return (skum, length trans, entityVal <$> lastm)





-- | This is the core routine which recalcuate the correct standard cost 
-- and correct gl amount.
computeItemCostTransactions :: (Maybe ItemCostTransaction) -> Account -> [These (Entity FA.StockMove) (Entity FA.GlTran)] -> [ItemCostTransaction]
computeItemCostTransactions lastm (Account account0) sm'gls0 = let 
    -- first we need to make sure there is no duplicate 
    sm'gls = fixDuplicates sm'gls0
    mkTrans previous sm'gl = let
       smM = preview here sm'gl
       glM = preview there sm'gl
       get :: (FA.StockMove -> a) 
           -> (FA.GlTran -> a)
           -> a
       get fa fb = mergeTheseWith (fa . entityVal) (fb . entityVal) const sm'gl
       quantity = maybe 0 (FA.stockMoveQty . entityVal) (smM)
       date  = get FA.stockMoveTranDate FA.glTranTranDate
       moveId = FA.unStockMoveKey . entityKey <$> smM
       glDetail = FA.unGlTranKey . entityKey <$> glM
       faTransNo = get FA.stockMoveTransNo FA.glTranTypeNo
       faTransType = toEnum $ get FA.stockMoveType FA.glTranType
       sku = get (Just . FA.stockMoveStockId) FA.glTranStockId
       -- TODO : everything below
       account = maybe account0 (FA.glTranAccount . entityVal) (glM)
       faAmount = maybe 0 (FA.glTranAmount . entityVal) (glM)
       correctAmount = if faTransType `elem` [ST_COSTUPDATE , ST_SUPPINVOICE, ST_SUPPCREDIT]
                       then faAmount
                       else quantity * cost
       qohBefore = maybe 0 itemCostTransactionQohAfter previous
       qohAfter = qohBefore + quantity       
       -- Try in order (gl transaction cost), previous, move
       cost' =  case (glM) of
                  (Just (Entity _ gl)) | faTransType `elem` [ST_SUPPRECEIVE, ST_INVADJUST, ST_COSTUPDATE ]
                                       , quantity /= 0 -> FA.glTranAmount gl / quantity
                  _ -> 0
       cost = if faTransType `elem` [ST_SUPPRECEIVE, ST_LOCTRANSFER] && isNothing glM
              then 0 -- wait for corresponding invoice
              else fromMaybe 0 $ headMay $ filter (/=0) [cost', costBefore, moveCost]
       moveCost = maybe 0 (FA.stockMoveStandardCost . entityVal) (smM)
       costBefore = maybe 0 itemCostTransactionCostAfter previous
       costAfter =
         if qohAfter /= 0
         then stockValue / qohAfter
         else 0
       faStockValue = maybe 0 itemCostTransactionFaStockValue previous + faAmount
              
       stockValue = maybe 0 itemCostTransactionStockValue previous + correctAmount
       in Just $ ItemCostTransaction
              { itemCostTransactionDate = date
              , itemCostTransactionMoveId = moveId
              , itemCostTransactionGlDetail = glDetail
              , itemCostTransactionFaTransNo = faTransNo
              , itemCostTransactionFaTransType = faTransType
              , itemCostTransactionSku = sku
              , itemCostTransactionAccount = account
              , itemCostTransactionFaAmount = faAmount
              , itemCostTransactionCorrectAmount = correctAmount
              , itemCostTransactionQohBefore = qohBefore
              , itemCostTransactionQohAfter = qohAfter
              , itemCostTransactionQuantity = quantity
              , itemCostTransactionCostBefore = costBefore
              , itemCostTransactionCost = cost
              , itemCostTransactionMoveCost = moveCost
              , itemCostTransactionCostAfter = costAfter
              , itemCostTransactionStockValue = stockValue
              , itemCostTransactionFaStockValue = faStockValue
              , itemCostTransactionItemCostValidation = Nothing
              , itemCostTransactionIsLast = Inactive
             }
    in catMaybes $ scanl'  mkTrans lastm sm'gls

-- | If a transaction contains the same items many times, for example 2 moves and 2 gl_trans  
-- instead of having 2 element in the list we will have the 4 (the cross product resulting from the join)
fixDuplicates :: [These (Entity FA.StockMove) (Entity FA.GlTran)] -> [These (Entity FA.StockMove) (Entity FA.GlTran)]
fixDuplicates move'gls = let
  trans = groupBy ((==) `on` transKey)  (sortOn transKey move'gls)
  transKey :: These (Entity FA.StockMove) (Entity FA.GlTran) -> (Int, Int)
  transKey = mergeTheseWith (((,) <$> FA.stockMoveTransNo <*> FA.stockMoveType) . entityVal)
                            (((,) <$> FA.glTranTypeNo <*> FA.glTranType) . entityVal)
                            const
  unduplicate m'gs = case m'gs of
         [m'g] -> [m'g]
         (m'g:_)  -> let
            (moves, cancellingPairs) = removeCancellingMoves $ mapMaybe getFirst $ toList $ groupAsMap entityKey (First . Just) $ mapMaybe (preview here) m'gs
            gls = mapMaybe getFirst $ toList $ groupAsMap entityKey (First . Just) $ mapMaybe (preview there) m'gs
            in ( case (length moves , length gls) of
                 (0, 0 ) ->  []
                 (0, _ ) -> map That gls
                 (_, 0 ) -> map This moves
                 (moveLength, glLength) | moveLength == glLength && (moveLength + length cancellingPairs) * glLength == length m'gs -> zipWith These moves gls 
                 _ -> error $ "Not cartesian product for " ++ (show $ transKey m'g) 
                            ++ " moves: " ++ show (length moves)
                            ++ " gls: " ++ show (length gls) 
               ) ++ map This cancellingPairs

         _ -> error "unexpected"
  in concatMap unduplicate trans

-- | some transactions contains moves which cancelled each others
-- this is probably due to the way voiding some transaction is (was?) handled
removeCancellingMoves :: [Entity FA.StockMove] -> ([Entity FA.StockMove], [Entity FA.StockMove])
removeCancellingMoves moves = let
   -- group moves by cost and absolute quantity
   groups = Map.fromListWith (<>) [ ((abs (FA.stockMoveQty move), FA.stockMoveStandardCost move)
                           , [e]
                           )
                         | e@(Entity _ move)  <- moves
                         ]
   cleanGroup mvs = let
       (negs,pos) = partition ((<0) . FA.stockMoveQty . entityVal) mvs
       cancelling = zip negs pos
       leftover = concatMap (drop (length cancelling)) [negs, pos]
       in (leftover, concatMap (\(a,b) -> [a,b]) cancelling)
   (allLeftover, allCancelling) = mconcat $ map cleanGroup (toList groups)
   in (sortOn entityKey allLeftover, sortOn entityKey allCancelling)






collectCostTransactions :: Account -> (Maybe Text) -> Handler ()
collectCostTransactions account skum = do
  lastEm <- loadLastTransaction account skum
  let lastm = entityVal <$> lastEm
  trans0 <- loadMovesAndTransactions lastm account skum
  let trans = computeItemCostTransactions lastm account trans0
  runDB $ 
     case reverse trans of
        [] -> return ()
        (last: reversed) -> do
           forM lastEm $ \last -> update (entityKey last) [ItemCostTransactionIsLast =. Inactive]
           insertMany_ (reverse reversed)
           insert_ last {itemCostTransactionIsLast = Active}
  
  
loadLastTransaction :: Account -> (Maybe Text)  -> Handler (Maybe (Entity ItemCostTransaction))
loadLastTransaction (Account account) skum = do
  runDB $ getBy (UniqueASL account skum Active) 
 

