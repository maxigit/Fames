module Handler.GL.Check.ItemCost.Common
( getStockAccounts
, Account(..)
, AccountSummary(..)
, CheckInfo(..)
, getAccountSummary
, loadPendingTransactionCountFor
, loadMovesAndTransactions
, computeItemCostTransactions
, collectCostTransactions
, loadCostSummary
, Matched
, loadCheckInfo
, itemSettings
)
where

import Import
import GL.Check.ItemCostSettings
import Database.Persist.Sql  (rawSql, Single(..))
import qualified FA as FA
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro.Extras (preview)
import Data.Monoid(First(..))
import Data.List(scanl')


-- * Types

data AccountSummary = AccountSummary
  { asAccount :: Account
  , asAccountName :: Text
  , asGLAmount :: Double
  , asCorrectAmount :: Maybe Double
  , asStockValuation :: Double
  }
  deriving (Show)

-- | Result of a quick sanity check
data CheckInfo  = CheckInfo
  { icAccount :: Account
  , icSku :: Maybe Text
  , icCostDiscrepency :: Bool -- ^ 
  , icNegativeQOH :: Bool
  , icCostVariation :: Bool -- ^ 
  }
  deriving (Show)
type Matched = (These (Entity FA.StockMove) (Entity FA.GlTran), Int)
-- * Summaries
getStockAccounts :: Handler [Account]
getStockAccounts = do
  let sql0 = "select distinct(inventory_account) from 0_stock_master"
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let (sql, params) = case settingsm of
             Nothing -> (sql0, [])
             Just settings -> let (keyword, stockLike) = filterEKeyword $ readFilterExpression (stockFilter settings)
                              in (sql0 <> " WHERE stock_id " <> keyword <> " ? ", [toPersistValue stockLike])
  -- let sql = "select distinct(account) from 0_gl_trans where stock_id is not null and stock_id " <> filterEToSQL stockLike
  --        <> " and amount <> 0"
  --        ^ Find all accounts used by any stock transaction (filter by stock like)
  rows <- runDB $ rawSql sql  params
  -- add extra accounts
  let accountSet = Set.fromList $ map (Account . unSingle) rows <> fromMaybe [] (extraAccounts =<< settingsm)
  return $ toList accountSet

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
  let sql = "select sum(stock_value) from check_item_cost_summary "
         <> " WHERE account = ?"
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
   [] -> error . unpack $ "Account " <> account <> " doesn't not exist!"
   _ -> error "The unexpected happened. Contact your Administrator!"

stockValuationFor :: Account -> Handler Double
stockValuationFor account = do
  sku'costs <- getItemFor account
  values <- mapM stockValuation sku'costs
  return $ sum values

getItemFor :: Account -> Handler [(Text, Double)]
getItemFor (Account account) = do
  -- let sql = "select stock_id, material_cost from 0_stock_master where inventory_account = ?"
  let sql = "select stock_id, material_cost from 0_stock_master where stock_id IN (select distinct stock_id from 0_gl_trans where account = ?)"
  runDB $ map (bimap unSingle unSingle)  <$> rawSql sql [toPersistValue account]

-- | Stock valuation for 
stockValuation :: (Text, Double) -> Handler Double
stockValuation (sku, cost) = do
  let sql = "select sum(qty) from 0_stock_moves where stock_id = ?"
  rows <- runDB $ rawSql sql [toPersistValue sku]
  case rows of
        [] -> return 0
        [(Single qty)] -> return $ maybe 0 (*cost) qty
        _ -> error "The unpexcted happend"


  

-- * Account  details
-- ** Loading
-- Load And join stock moves & gl trans of a given account and sku
-- If the sku is not provided, load the gl trans which doesn't have the stock_id set.
-- ONly load transaction not saved in inventory_cost_transaction
loadMovesAndTransactions :: (Maybe ItemCostSummary) -> Maybe Day -> Account -> Maybe Text -> Handler [Matched]  
loadMovesAndTransactions lastm endDatem (Account account) Nothing = do
  let maxGl = lastm >>= itemCostSummaryGlDetail
  let startDatem = itemCostSummaryDate <$>  lastm
  let sql = "SELECT ??,seq FROM 0_gl_trans "
         -- <> " LEFT JOIN check_item_cost_transaction i ON (0_gl_trans.counter = i.gl_detail) "
         <> "LEFT JOIN (SELECT MIN(id) AS seq, trans_no, type FROM 0_audit_trail GROUP BY trans_no, type) as audit ON (audit.trans_no = 0_gl_trans.type_no AND  audit.type = 0_gl_trans.type) "
         <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id IS NULL"
         <> "   AND 0_gl_trans.amount <> 0 "
         -- <> "   AND i.item_cost_transaction_id is NULL "
         <> ( if isJust maxGl
              then  "                      AND 0_gl_trans.counter > ?" 
              else "" )
         <> ( if isJust startDatem
              then  "                      AND 0_gl_trans.tran_date > ?" 
              else "" )
         <> ( if isJust endDatem
              then  "                      AND 0_gl_trans.tran_date <= ?" 
              else "" )
         <> " ORDER BY 0_gl_trans.tran_date, 0_gl_trans.counter"
      mkTrans (gl, Single seq)= (That gl, seq)
  rows <- runDB $ rawSql sql $ [toPersistValue account]
                               ++ maybe [] (pure . toPersistValue) maxGl
                               ++ maybe [] (pure . toPersistValue) startDatem
                               ++ maybe [] (pure . toPersistValue) endDatem
  return $ map mkTrans rows

loadMovesAndTransactions lastm endDatem account (Just sku) = do
   withMoves <- loadMovesAndTransactions' lastm endDatem account sku
   glOnly <- loadTransactionsWithNoMoves' lastm endDatem account sku
   -- interleave two sort lists as sorted
   let interleave [] ys = ys
       interleave xs [] = xs
       interleave (x:xs) ys@(y:_) | x `before` y = x : interleave xs  ys
       interleave xs (y:ys) = y : interleave xs  ys
       -- compare date then gl counter
      
       get fa fb = mergeTheseWith (fa . entityVal) (fb . entityVal) const
       before a b = key a < key b
        where key (x, seq) =  ( get FA.stockMoveTranDate FA.glTranTranDate x
                       , seq
                       )
   return $ interleave withMoves glOnly


loadMovesAndTransactions' lastm endDatem (Account account) sku = do
  -- instead of checking if there is already an item in check_item_cost_transaction
  -- we filter moves and gl trans by only using the one
  -- newer than the ids found in the last ItemCostTransaction
  -- ids in table are sequential, so it should work.
  -- This is much faster than the initial behavior which is slow
  -- even though it's is using index (BTree)
  let maxGl = lastm >>= itemCostSummaryGlDetail
      maxMove = lastm >>= itemCostSummaryMoveId
      startDatem = itemCostSummaryDate <$>  lastm
      sql = "SELECT ??, ??, seq FROM 0_stock_moves "
         <> " LEFT JOIN 0_gl_trans ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
         <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
         <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
         <> "                            AND 0_gl_trans.amount <> 0 "
         <> ( if isJust maxGl
              then  "                      AND 0_gl_trans.counter > ?" 
              else "" )
         <> "                            )"
         <> "LEFT JOIN (SELECT MIN(id) as seq, trans_no, type FROM 0_audit_trail GROUP BY trans_no, type) as audit ON (0_stock_moves.trans_no = audit.trans_no AND 0_stock_moves.type = audit.type) "
         -- <> " LEFT JOIN check_item_cost_transaction i ON (0_stock_moves.trans_id = move_id OR 0_gl_trans.counter = gl_detail) "
         <> " WHERE (0_gl_trans.account = ? OR 0_gl_trans.account is NULL) AND 0_stock_moves.stock_id =  ? "
         -- <> "   AND i.item_cost_transaction_id is NULL "
         <> "   AND 0_stock_moves.qty <> 0"
         <> (if isJust maxMove
            then  "                      AND 0_stock_moves.trans_id > ?" 
            else "")
         <> ( if isJust startDatem
              then  "                      AND 0_stock_moves.tran_date > ?" 
              else "" )
         <> ( if isJust endDatem
              then  "                      AND 0_stock_moves.tran_date <= ?" 
              else "" )
         <> " ORDER BY 0_stock_moves.tran_date, seq, 0_stock_moves.trans_id"
      mkTrans m'g = case m'g of
                      (m, Nothing, Single seq ) -> (This m, seq)
                      (m, Just g, Single seq ) -> (These m g, seq)
  rows <- runDB $ rawSql sql $ maybe [] (pure . toPersistValue) maxGl
                               ++ [ toPersistValue account
                                  , toPersistValue sku
                                  ]
                                ++ maybe [] (pure . toPersistValue) maxMove
                               ++ maybe [] (pure . toPersistValue) startDatem
                               ++ maybe [] (pure . toPersistValue) endDatem
  return $ map mkTrans rows

loadTransactionsWithNoMoves' lastm endDatem (Account account) sku = do
  let maxGl = lastm >>= itemCostSummaryGlDetail
      startDatem = itemCostSummaryDate <$>  lastm
      sql = "SELECT ??, seq FROM 0_gl_trans "
         <> " LEFT JOIN 0_stock_moves ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
         <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
         <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
         <> "                            AND 0_stock_moves.qty <> 0"
         <> "                            )"
         -- <> " LEFT JOIN check_item_cost_transaction i ON (0_gl_trans.counter = i.gl_detail) "
         <> "LEFT JOIN (SELECT MIN(id) AS seq, trans_no, type FROM 0_audit_trail GROUP BY trans_no, type) as audit ON (audit.trans_no = 0_gl_trans.type_no AND  audit.type = 0_gl_trans.type) "
         <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id = ? "
         <> "   AND 0_gl_trans.amount <> 0 "
         -- <> "   AND i.item_cost_transaction_id IS NULL"
         <> "   AND 0_stock_moves.stock_id IS NULL "
         <> ( if isJust maxGl
              then  "                      AND 0_gl_trans.counter > ?" 
              else "" )
         <> ( if isJust startDatem
              then  "                      AND 0_gl_trans.tran_date > ?" 
              else "" )
         <> ( if isJust endDatem
              then  "                      AND 0_gl_trans.tran_date <= ?" 
              else "" )
         <> " ORDER BY 0_gl_trans.tran_date, 0_gl_trans.counter"
      mkTrans (gl, Single seq) = (That gl, seq)
  rows <- runDB $ rawSql sql $ [toPersistValue account, toPersistValue sku]
                               ++ maybe [] (pure . toPersistValue) maxGl
                               ++ maybe [] (pure . toPersistValue) startDatem
                               ++ maybe [] (pure . toPersistValue) endDatem
                        
  return $ map mkTrans rows


{-
loadMovesAndTransactionsFor :: Account -> Handler()
loadMovesAndTransactionsFor account = do
  sku'_s <- getItemFor account
  let skums = Nothing : map Just sku'_s
  mapM_ computesTransactionCheck skums
  -}

loadPendingTransactionCountFor :: Account -> Handler [(Maybe Text, Int, Maybe ItemCostSummary)]
loadPendingTransactionCountFor account = do
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  sku'_s <- getItemFor account
  let skums = Nothing : map (Just . fst) sku'_s
  forM skums $ \skum -> do
    let endDatem = skum >>= itemSettings settingsm account >>= closingDate
    lastm <- loadCostSummary account skum
    trans <- loadMovesAndTransactions (entityVal <$> lastm) endDatem account skum
    return (skum, length trans, entityVal <$> lastm)


loadCostSummary :: Account -> (Maybe Text)  -> Handler (Maybe (Entity ItemCostSummary))
loadCostSummary (Account account) skum = do
  runDB $ selectFirst [ItemCostSummaryAccount ==. account, ItemCostSummarySku ==. skum] []

-- ** Computing cost transaction
-- | This is the core routine which recalcuate the correct standard cost 
-- and correct gl amount.
computeItemCostTransactions :: (Maybe ItemCostSummary) -> Account -> [Matched] -> Either (Text, [Matched])  [ItemCostTransaction]
computeItemCostTransactions summarym (Account account0) sm'gls0 = let 
    -- first we need to make sure there is no duplicate 
    lastm = case summarym of
      Nothing -> Nothing
      Just ItemCostSummary{..} -> let
            itemCostTransactionDate = itemCostSummaryDate
            itemCostTransactionMoveId = itemCostSummaryMoveId
            itemCostTransactionGlDetail = itemCostSummaryGlDetail
            itemCostTransactionFaTransNo = 0
            itemCostTransactionFaTransType = ST_INVADJUST
            itemCostTransactionSku = itemCostSummarySku
            itemCostTransactionAccount = itemCostSummaryAccount
            itemCostTransactionFaAmount = 0
            itemCostTransactionCorrectAmount = 0
            itemCostTransactionQohBefore = 0
            itemCostTransactionQohAfter = itemCostSummaryQohAfter
            itemCostTransactionQuantity = 0
            itemCostTransactionCostBefore = 0
            itemCostTransactionCost = 0
            itemCostTransactionCostAfter = itemCostSummaryCostAfter
            itemCostTransactionStockValue = itemCostSummaryStockValue
            itemCostTransactionFaStockValue = itemCostSummaryFaStockValue
            itemCostTransactionMoveCost = 0
            itemCostTransactionItemCostValidation = Nothing
            in Just ItemCostTransaction{..}
          
    sm'gls = fixDuplicates sm'gls0
    mkTrans previous (sm'gl, _seq) = let
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
             }
    in fmap (catMaybes . scanl'  mkTrans lastm) sm'gls

-- | If a transaction contains the same items many times, for example 2 moves and 2 gl_trans  
-- instead of having 2 element in the list we will have the 4 (the cross product resulting from the join)
-- In case duplicates can't be fixed returns (Left) the faulty transactions
fixDuplicates :: [Matched] -> Either (Text, [Matched]) [Matched]
fixDuplicates move'gls = let
  trans = groupBy ((==) `on` snd)  (move'gls)
  transKey = mergeTheseWith (((,) <$> FA.stockMoveTransNo <*> FA.stockMoveType) . entityVal)
                            (((,) <$> FA.glTranTypeNo <*> FA.glTranType) . entityVal)
                            const
  --  ^ already sorted by date and audit.id (sequence)
  unduplicate :: [Matched] -> Either (Text, [Matched]) [Matched]
  unduplicate m'gs = case m'gs of
         [m'g] -> Right [m'g]
         ((m'g,seqN):_)  -> let
            (moves, cancellingPairs) = removeCancellingMoves $ mapMaybe getFirst $ toList $ groupAsMap entityKey (First . Just) $ mapMaybe (preview here . fst) m'gs
            gls = mapMaybe getFirst $ toList $ groupAsMap entityKey (First . Just) $ mapMaybe (preview there . fst) m'gs
            in ( case (length moves , length gls) of
                 (0, 0 ) ->  Right []
                 (0, _ ) -> Right $ map (\g -> (That g, seqN)) gls
                 (_, 0 ) -> Right $ map (\m -> (This m, seqN)) moves
                 (moveLength, glLength) | moveLength == glLength && (moveLength + length cancellingPairs) * glLength == length m'gs -> Right $ zipWith (\m g -> (These m g, seqN)) moves gls 
                 (1, 2) -- | (FA.stockMoveType . entityVal <$> preview here m'g) == Just (fromEnum ST_INVADJUST)
                        | abs (sum (map (FA.glTranAmount . entityVal) gls)) < 1e-4
                        ->
                          -- ^ Inventory adjustment uses by mistake the same GL ACcount as the stock account and adjustment account
                          -- In that case the stock account should be matched with the moves
                          let direction = maybe False ((>0) . FA.stockMoveQty . entityVal) (preview here m'g)
                          in case  (moves, partition ((== direction) . ((>0) . FA.glTranAmount . entityVal)) gls) of
                               ([move], ([conv], [div])) -> Right $ [(These move conv, seqN), (That div, seqN)]
                               _ -> error ( "Unexpected happend") -- ^ we know we have 2 gl transactions
                 _ -> Left ( "Not cartesian product for " ++ (tshow $ transKey m'g) 
                            ++ " moves: " ++ tshow (length moves)
                            ++ " gls: " ++ tshow (length gls) 
                            , m'gs)
               ) <&> (++ (map (\m -> (This m, seqN)) cancellingPairs))

         _ -> Left ( "unexpected", m'gs)
  in concat <$> mapM unduplicate trans

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



-- ** Collect and Save

-- | Load the last summary or the last summary of another account
-- if the item used a previous account
loadInitialSummary :: Account -> (Maybe Text) -> Handler (Maybe (Either ItemCostSummary
                                                                        (Entity ItemCostSummary) ))
loadInitialSummary account@(Account acc) skum = do
  summarym <- loadCostSummary account skum
  case (summarym, skum) of
    (Just _, _) -> return $ fmap Right summarym
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just sku) -> do
      settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
      case  itemSettings settingsm account sku >>= initial of
        Just (FromAccount oldAccount) -> do
                   sumEM <- loadCostSummary oldAccount skum
                   case sumEM of
                      Just (Entity _ summary) -> return . Just $ Left summary { itemCostSummaryAccount = acc}
                      _ -> error . unpack $ "Can't load old account summary for " <> acc <> " " <>  sku
        Just (InitialData{..}) -> return . Just $ Left 
                 ItemCostSummary{ itemCostSummaryDate = startDate
                                , itemCostSummaryAccount = acc
                                , itemCostSummaryMoveId = Nothing
                                , itemCostSummaryGlDetail = Nothing
                                , itemCostSummarySku = Just sku
                                , itemCostSummaryQohAfter = initialQoh
                                , itemCostSummaryCostAfter = if initialQoh /= 0
                                                             then initialBalance / initialQoh  
                                                             else 0
                                , itemCostSummaryStockValue = initialBalance
                                , itemCostSummaryFaStockValue = initialBalance
                                }

        _ -> return Nothing

      


collectCostTransactions :: Account -> (Maybe Text) -> Handler (Either (Text, [Matched]) ())
collectCostTransactions account skum = do
  lastEm <- loadInitialSummary account skum
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let endDatem = skum >>= itemSettings settingsm account >>= closingDate
      lastm = either id entityVal <$> lastEm
  trans0 <- loadMovesAndTransactions lastm endDatem account skum
  let transE = computeItemCostTransactions lastm account trans0
  forM transE $ \trans -> 
    runDB $ 
       case lastMay trans of
          Nothing -> return ()
          Just summary  -> do
             insertMany_ trans
             let itemCostSummaryDate = itemCostTransactionDate  summary
                 itemCostSummaryMoveId = maximumMay $ mapMaybe itemCostTransactionMoveId trans :: Maybe Int
                 itemCostSummaryGlDetail = maximumMay $ mapMaybe itemCostTransactionGlDetail trans
                 itemCostSummarySku = itemCostTransactionSku summary
                 itemCostSummaryAccount = itemCostTransactionAccount summary
                 itemCostSummaryQohAfter = itemCostTransactionQohAfter summary
                 itemCostSummaryCostAfter = itemCostTransactionCostAfter summary
                 itemCostSummaryStockValue = itemCostTransactionStockValue summary
                 itemCostSummaryFaStockValue = itemCostTransactionFaStockValue summary
             case lastEm of
               Just (Right (Entity key _)) -> repsert key ItemCostSummary{..}
               _ -> insert_ ItemCostSummary{..}
     
 

-- * sanity check
-- | Detect all Sku/Account which look suspect
-- (not much variation in cost price, too much discrepency between FA and calculated etc ...
loadCheckInfo :: Handler [CheckInfo]
loadCheckInfo = do
  let sql = "SELECT  account, sku "
         ++ " , MAX(COALESCE(correct_amount != 0 AND fa_amount != 0 AND abs(1-LEAST(correct_amount, fa_amount)/GREATEST(correct_amount, fa_amount))>0.1,False)) as cost_discrepency"
          ++ ", MAX(qoh_after < 0) as negative_qoh "
         ++ " , MAX(COALESCE(cost_before != 0 AND cost_after != 0 AND abs(1-LEAST(cost_before, cost_after)/GREATEST(cost_before, cost_after))>0.25,False)) as cost_variation"
          ++ " FROM check_item_cost_transaction "
          ++ " WHERE fa_trans_type NOT IN (16)" -- filter location transfer
          ++ " AND item_cost_validation IS NULL "
          ++ " GROUP BY account, sku "
          ++ " HAVING cost_discrepency > 0 OR negative_qoh > 0 OR cost_variation > 0"
      mkCheck (Single account
              , Single icSku
              , Single icCostDiscrepency
              , Single icNegativeQOH
              , Single icCostVariation
              ) = CheckInfo{icAccount=Account account ,..}

  rows <- runDB $ rawSql sql []
  return $ map mkCheck rows

-- *  Utils
itemSettings :: Maybe Settings -> Account -> Text -> Maybe ItemSettings
itemSettings settingsm account sku = 
  settingsm >>= lookup account . accounts >>= lookup sku . items 
