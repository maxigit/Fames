module Handler.GL.Check.ItemCost.Common
( getStockAccounts
, Account(..)
, AccountSummary(..)
, CheckInfo(..)
, getAccountSummary
, itemTodayInfo
, loadPendingTransactionCountFor
, loadMovesAndTransactions
, computeItemCostTransactions
, collectCostTransactions
, loadCostSummary
, Matched
, loadCheckInfo
, itemSettings
, fixGLBalance
, updateCosts
, voidValidation
, itemCostTransactionStockValueRounded
, itemCostSummaryStockValueRounded
, round2
)
where

import Import
import GL.Check.ItemCostSettings
import Database.Persist.Sql  (rawSql, Single(..), rawExecute, fromSqlKey)
import qualified FA as FA
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro.Extras (preview)
import Data.Monoid(First(..))
import  qualified WH.FA.Types as WFA
import  qualified WH.FA.Curl as WFA
import Util.Decimal
import Control.Monad.Except (runExceptT, ExceptT(..))
import Data.List(nub, unfoldr)


-- * Types

data AccountSummary = AccountSummary
  { asAccount :: Account
  , asAccountName :: Text
  , asGLAmount :: Double
  , asCorrectAmount :: Maybe Double
  , asCorrectQoh :: Maybe Double
  , asStockValuation :: Double
  , asQuantity :: Double
  , asCollectDate :: Maybe Day
  }
  deriving (Show)

-- | Result of a quick sanity check
data CheckInfo  = CheckInfo
  { icAccount :: Account
  , icSku :: Maybe Text
  , icAmountDiscrepency :: Double -- ^ 
  , icCostDiscrepency :: Double -- ^ 
  , icNegativeQOH :: Bool
  , icCostVariation :: Double -- ^ 
  , icNullFAStockDiscrepency :: Double -- ^ qoh 0 but GL balance not null
  , icNullStockDiscrepency :: Double -- ^ qoh 0 but GL balance not null
  }
  deriving (Show)
type Matched = (These (Entity FA.StockMove) (Entity FA.GlTran), Int)
-- * Summaries
getStockAccounts :: Handler [Account]
getStockAccounts = do
  let sql0 = "SELECT DISTINCT(inventory_account) FROM 0_stock_master WHERE mb_flag <> 'D' "
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let (sql, params) = case settingsm of
             Nothing -> (sql0, [])
             Just settings -> let (keyword, stockLike) = filterEKeyword $ readFilterExpression (stockFilter settings)
                              in (sql0 <> " AND stock_id " <> keyword <> " ? ", [toPersistValue stockLike])
  -- let sql = "select distinct(account) from 0_gl_trans where stock_id is not null and stock_id " <> filterEToSQL stockLike
  --        <> " and amount <> 0"
  --        ^ Find all accounts used by any stock transaction (filter by stock like)
  rows <- runDB $ rawSql sql  params
  -- add extra accounts
  let accountSet = Set.fromList $ map (Account . unSingle) rows <> fromMaybe [] (extraAccounts =<< settingsm)
  return $ toList accountSet

getAccountSummary :: Day -> Account -> Handler AccountSummary
getAccountSummary date account = do
  asGLAmount <- glBalanceFor date account
  asAccountName <- getAccountName account
  (asCorrectAmount, asCorrectQoh, asCollectDate) <- getTotalCost date account
  (asStockValuation, asQuantity) <- stockValuationFor date account
  return AccountSummary{asAccount=account,..}


glBalanceFor :: Day -> Account -> Handler Double
glBalanceFor date (Account account) = do
 let sql = "select sum(amount) from 0_gl_trans where account = ? AND tran_date <= ? "
 rows <- runDB $ rawSql sql [toPersistValue account, toPersistValue date]
 case rows of
   [] -> return 0
   [(Single total)] -> return $ fromMaybe 0 total
   _ -> error "glBalanceFor should only returns one row. Please contact your Admininstrator!"


getTotalCost :: Day -> Account -> Handler (Maybe Double, Maybe Double, Maybe Day)
getTotalCost date (Account account) = do
  let sql = "SELECT sum(round(stock_value, 2)), sum(qoh_after), max(date) "
         <> " FROM check_item_cost_summary  "
         <> " WHERE account = ? " --  and date <= ? "
  rows <- runDB $ rawSql sql [toPersistValue account] -- , toPersistValue date]
  case rows of
    [] -> return (Nothing, Nothing, Nothing)
    [(Single total, Single qoh, Single collectDate)] -> 
      if collectDate > Just date
      then return (Nothing, Nothing, collectDate)
      else return (total, qoh, collectDate)
    _ -> error "glTotalCost should only returns one row. Please contact your Admininstrator!"

getAccountName :: Account -> Handler Text
getAccountName (Account account) = do
  let sql = "select account_name from 0_chart_master where account_code = ?"
  rows <- runDB $ rawSql sql [toPersistValue account]
  case rows of
   [(Single name)] -> return $ decodeHtmlEntities name
   [] -> error . unpack $ "Account " <> account <> " doesn't not exist!"
   _ -> error "The unexpected happened. Contact your Administrator!"

stockValuationFor :: Day -> Account -> Handler (Double, Double)
stockValuationFor date account = do
  sku'costs <- getItemFor account
  values <- mapM (stockValuation date) sku'costs
  return $ (sum (map (round2 . fst) values), sum (map snd values))

-- | Get Sku and (actual) cost price 
getItemFor :: Account -> Handler [(Text, Double)]
getItemFor (Account account) = do
  -- let sql = "select stock_id, material_cost from 0_stock_master where inventory_account = ?"
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let sql0 = "SELECT stock_id, material_cost "
          <> " FROM 0_stock_master "
          <> " WHERE stock_id IN (SELECT distinct stock_id  "
          <> "                    FROM 0_gl_trans where account = ?)"
          <> " AND mb_flag <> 'D'"                    
      (sql, params) = case settingsm of
             Nothing -> (sql0, [])
             Just settings -> let (keyword, stockLike) = filterEKeyword $ readFilterExpression (stockFilter settings)
                              in (sql0 <> " AND stock_id " <> keyword <> " ? ", [toPersistValue stockLike])
  runDB $ map (bimap unSingle unSingle)  <$> rawSql sql ([toPersistValue account] ++ params)

-- | Stock valuation for return stock valuation + qoh for given stock price
stockValuation :: Day -> (Text, Double) -> Handler (Double, Double)
stockValuation date (sku, cost) = do
  let sql = "select sum(qty) from 0_stock_moves where stock_id = ? and tran_date <= ? "
  rows <- runDB $ rawSql sql [toPersistValue sku, toPersistValue date]
  case rows of
        [] -> return (0,0)
        [(Single qtym)] -> return $ maybe (0,0) (\q -> (q*cost, q)) qtym
        _ -> error "The unpexcted happend"


  
-- | Return qoh stock valuation and current cost price
itemTodayInfo :: Account -> Handler (Map Text (Double, Double))
itemTodayInfo (Account account) = do
  let sql = "select stock_id, sum(qty), material_cost from 0_stock_moves "
          <>  " join 0_stock_master USING(stock_id) "
          <> " WHERE  inventory_account = ? "
          <> " AND mb_flag <> 'D'"
          <> " GROUP BY stock_id "
  rows <- runDB $ rawSql sql [toPersistValue account]
  return . mapFromList $ map (\(Single sku, Single qoh, Single cost) -> (sku, (cost, qoh))) rows

-- * Account  details
-- ** Loading
-- Load And join stock moves & gl trans of a given account and sku
-- If the sku is not provided, load the gl trans which doesn't have the stock_id set.
-- ONly load transaction not saved in inventory_cost_transaction
loadMovesAndTransactions :: (Maybe ItemCostSummary) -> Maybe Day -> Account -> Maybe Text -> Handler [Matched]  
loadMovesAndTransactions lastm endDatem (Account account) Nothing = do
  let maxGl = lastm >>= itemCostSummaryGlDetail
  let startDatem = itemCostSummaryDate <$>  lastm
  let (sqls, paramss) = unzip 
        [ ( "SELECT ??,seq FROM 0_gl_trans "
            <> "LEFT JOIN (SELECT MIN(id) AS seq, trans_no, type FROM 0_audit_trail GROUP BY trans_no, type) as audit ON (audit.trans_no = 0_gl_trans.type_no AND  audit.type = 0_gl_trans.type) "
            <> " LEFT JOIN 0_voided ON (0_gl_trans.type = 0_voided.type AND 0_gl_trans.type_no = 0_voided.id ) "
            <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id IS NULL"
            <> "   AND 0_voided.id IS NULL "
          , [toPersistValue account])
        , filterTransactionFromSummary maxGl startDatem "0_gl_trans.counter" "0_gl_trans.tran_date"
        , case endDatem of
             Just endDate  -> (" AND 0_gl_trans.tran_date <= ?" , [toPersistValue endDate] )
             _ -> ("", [])
        , ( " ORDER BY 0_gl_trans.tran_date, seq", [])
        ]
      mkTrans (gl, Single seq)= (That gl, seq)
  rows <- runDB $ rawSql (concat sqls) (concat paramss)
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
      
       before a b = key a < key b
        where key (x, seq) =  ( gett FA.stockMoveTranDate FA.glTranTranDate x
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
      (sqls, paramss) = unzip
        [ ("SELECT ??, ??, seq FROM 0_stock_moves "
           <> " LEFT JOIN 0_gl_trans ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
           <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
           <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
           <> "                            AND 0_gl_trans.amount <> 0 "
           <> "                            AND (0_gl_trans.account = ? OR 0_gl_trans.account is NULL) "
         <> "                            )"
           , [toPersistValue account] )
         , ( "LEFT JOIN (SELECT MIN(id) as seq, trans_no, type FROM 0_audit_trail GROUP BY trans_no, type) as audit "
           <> " ON (0_stock_moves.trans_no = audit.trans_no AND 0_stock_moves.type = audit.type) "
           <> " LEFT JOIN 0_voided ON (0_stock_moves.type = 0_voided.type AND 0_stock_moves.trans_no = 0_voided.id ) "
           <> " WHERE  0_stock_moves.stock_id =  ? "
           , [toPersistValue sku] )

         , ( "   AND 0_voided.id IS NULL "
             <> "   AND 0_stock_moves.qty <> 0"
             , [])
        , filterTransactionFromSummary maxGl startDatem "0_gl_trans.counter" "0_gl_trans.tran_date"
        , filterTransactionFromSummary maxMove startDatem "0_stock_moves.trans_id" "0_stock_moves.tran_date"
        , case endDatem of
            Just endDate -> (" AND 0_stock_moves.tran_date <= ?",  [toPersistValue endDate])
            Nothing -> ("", [])
        , (" ORDER BY 0_stock_moves.tran_date, seq, 0_stock_moves.trans_id", [])
        ]
      mkTrans m'g = case m'g of
                      (m, Nothing, Single seq ) -> (This m, seq)
                      (m, Just g, Single seq ) -> (These m g, seq)
  rows <- runDB $ rawSql (concat sqls)  (concat paramss)
  return $ map mkTrans rows

filterTransactionFromSummary maxId startDatem idField dateField =
        case (maxId, startDatem) of
            (Just id_, Just startDate) -> ( " AND ((" <> idField <> "> ? AND " <> dateField <> " = ?) OR " <> dateField <> " > ?)  " 
                                     , [toPersistValue id_, toPersistValue startDate, toPersistValue startDate] )
             -- ^ we use gl.counter only the day of the summary, to know what transaction haven't been collected
             -- but created since the collection.
             -- It only works on the day because we can have transaction < counter with date >= summary.date.
             -- It happens when transaction are not entered in chronological order.
            (_, Just startDate) ->  ( " AND " <> dateField <> " > ?"
                                    ,  [toPersistValue startDate])
            _ -> ("", [])

loadTransactionsWithNoMoves' lastm endDatem (Account account) sku = do
  let maxGl = lastm >>= itemCostSummaryGlDetail
      startDatem = itemCostSummaryDate <$>  lastm
      (sqls, paramss) = unzip
        [ ( "SELECT ??, seq FROM 0_gl_trans "
           <> " LEFT JOIN 0_stock_moves ON ( 0_stock_moves.stock_id = 0_gl_trans.stock_id"
           <> "                            AND 0_stock_moves.type = 0_gl_trans.type"
           <> "                            AND 0_stock_moves.trans_no = 0_gl_trans.type_no"
           <> "                            AND 0_stock_moves.qty <> 0"
           <> "                            )"
           -- <> " LEFT JOIN check_item_cost_transaction i ON (0_gl_trans.counter = i.gl_detail) "
           <> "LEFT JOIN (SELECT MIN(id) AS seq, trans_no, type FROM 0_audit_trail GROUP BY trans_no, type) as audit ON (audit.trans_no = 0_gl_trans.type_no AND  audit.type = 0_gl_trans.type) "
           <> " LEFT JOIN 0_voided ON (0_gl_trans.type = 0_voided.type AND 0_gl_trans.type_no = 0_voided.id ) "
           <> " WHERE 0_gl_trans.account = ? AND 0_gl_trans.stock_id = ? "
          ,  [toPersistValue account, toPersistValue sku])
        ,  ( "   AND 0_gl_trans.amount <> 0 "
           <> "   AND 0_voided.id IS NULL "
           <> "   AND 0_stock_moves.stock_id IS NULL "
         , [])
        , filterTransactionFromSummary maxGl startDatem "0_gl_trans.counter" "0_gl_trans.tran_date"
        , case endDatem of
             Just endDate  -> (" AND 0_gl_trans.tran_date <= ?" , [toPersistValue endDate] )
             _ -> ("", [])
        , ( " ORDER BY 0_gl_trans.tran_date, seq", [])
        ]
      mkTrans (gl, Single seq) = (That gl, seq)
  rows <- runDB $ rawSql (concat sqls) (concat paramss)
  return $ map mkTrans rows


{-
loadMovesAndTransactionsFor :: Account -> Handler()
loadMovesAndTransactionsFor account = do
  sku'_s <- getItemFor account
  let skums = Nothing : map Just sku'_s
  mapM_ computesTransactionCheck skums
  -}

loadPendingTransactionCountFor :: Day -> Account -> Handler [(Maybe Text, Int, Maybe ItemCostSummary)]
loadPendingTransactionCountFor date account = do
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  sku'_s <- getItemFor account
  let skums = Nothing : map (Just . fst) sku'_s
  forM skums $ \skum -> do
    let endDatem = Just . maybe date (min date) $ skum >>= itemSettings settingsm account >>= closingDate
    lastm <- loadCostSummary account skum
    trans <- loadMovesAndTransactions (entityVal <$> lastm) endDatem account skum
    return (skum, length trans, entityVal <$> lastm)


loadCostSummary :: Account -> (Maybe Text)  -> Handler (Maybe (Entity ItemCostSummary))
loadCostSummary (Account account) skum = do
  runDB $ selectFirst [ItemCostSummaryAccount ==. account, ItemCostSummarySku ==. skum] []

-- ** Computing cost transaction
-- | This is the core routine which recalcuate the correct standard cost 
-- and correct gl amount.
computeItemCostTransactions :: BehaviorMap -> (Maybe ItemCostSummary) -> Account -> [Matched] -> Either (Text, [Matched])  [ItemCostTransaction]
computeItemCostTransactions behaviors_ summarym account0 sm'gls0 = let 
    -- first we need to make sure there is no duplicate 
    lastm = case summarym of
      Nothing -> Initial
      Just ItemCostSummary{..} -> 
            WithPrevious PreventNegative
              $ RunningState itemCostSummaryQohAfter
                             itemCostSummaryCostAfter
                             itemCostSummaryStockValue
                             itemCostSummaryStockValue
                             itemCostSummaryFaStockValue
    sm'glsE = fixDuplicates sm'gls0
    in either Left
              (\sm'gls -> computeItemHistory behaviors_ account0 lastm sm'gls <|&> (,sm'gls))
              sm'glsE

-- | Main function
data AllowNegative = AllowNegative | PreventNegative
  deriving (Eq, Show)
data HistoryState
  = Initial
  | WaitingForStock RunningState [Matched]
  | WithPrevious AllowNegative RunningState
  | SupplierInvoiceWaitingForGRN RunningState Matched [Matched]
  | SupplierGRNWaitingForInvoice RunningState Matched [Matched]
instance Show HistoryState where
  show state = case state of
    Initial -> "I"
    WaitingForStock _ _ -> "WaitingForStock"
    WithPrevious _ _ -> "WithPrevious"
    SupplierGRNWaitingForInvoice _ _ _ -> "SupplierGRNWaitingForInvoice"
    SupplierInvoiceWaitingForGRN _ inv _ -> "SupplierInvoiceWaitingForGRN" <> show (snd inv)


data RunningState = RunningState
  { qoh :: Double
  , standardCost :: Double
  , stockValue :: Double
  , expectedBalance :: Double
  , faBalance :: Double
  } deriving Show

data Transaction = Transaction
  { tQuantity :: Double
  , tCost :: Double
  , tAmount :: Double
  , tComment :: Text
  } deriving Show

instance Semigroup RunningState where
  a <> b = let totalQoh = qoh a + qoh b
               totalValue = stockValue a + stockValue b 
               cost = case (totalQoh, standardCost a,standardCost b) of
                                  (0, ca, 0) -> ca
                                  (0, _, cb) -> cb
                                  (_, 0, cb) -> cb
                                  _ -> totalValue / totalQoh
               stock = totalQoh * cost
           in RunningState totalQoh
                           cost
                           stock
                           (expectedBalance a + expectedBalance b)
                           (faBalance a + faBalance b)

instance Monoid RunningState where
  mempty = RunningState 0 0 0 0 0


computeItemHistory :: BehaviorMap -> Account -> HistoryState -> [Matched] -> Either Text [ItemCostTransaction]
computeItemHistory behaviors_ account0 previousState [] =  
  case previousState of
    WaitingForStock previous toprocess -> 
    -- start again but allow negative.
    -- We also need to use the first know cost price
      case reverse toprocess of
        sm'gls@((sm'gl,_):_) | Just sm <- entityVal <$> preview here sm'gl -> 
            computeItemHistory behaviors_ account0 (WithPrevious AllowNegative previous { standardCost = FA.stockMoveStandardCost sm}) sm'gls
        sm'gls ->  computeItemHistory behaviors_ account0 (WithPrevious AllowNegative previous) sm'gls
    SupplierGRNWaitingForInvoice previous grn toprocess ->
      let behavior = behaviorFor behaviors_ [ForGrnWithoutInvoice] grn
      in case preview here (fst grn) of 
          Nothing -> Left $ "Unexpected happend.Shoudl be a GRN : Invoice id :" <> showForError grn
          Just (Entity _  move) -> 
             ( case (behavior, standardCost previous, FA.stockMoveStandardCost move ) of 
                    (Just Skip, _, _) -> Right $ (previous, Transaction 0 0 0 "skipped (behavior)")
                    (Just UseMoveCost, _0, moveCost) -> Right $ updateSummaryFromCost "Move" previous (FA.stockMoveQty move) moveCost 0
                    (Just UsePreviousCost, previousCost, _) -> Right $ updateSummaryFromCost "Previous" previous (FA.stockMoveQty move) previousCost 0
                    _ -> Left $ "Unspecified behavior for GRN without invoice #" <> showForError grn
            ) >>= (\(newSummary, newTrans) ->
            ((makeItemCostTransaction account0 previous grn newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious PreventNegative newSummary)  (reverse toprocess)
            )
    SupplierInvoiceWaitingForGRN previous inv toprocess -> 
      case preview there (fst inv) of
        Nothing -> Left "Unexpected happend.Shoudl be a Invoice"
        Just (Entity _ gl) ->
          let (newSummary, newTrans) = updateSummaryFromAmount previous 0 0 (FA.glTranAmount gl)
          in ((makeItemCostTransaction account0 previous inv newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious PreventNegative newSummary)  (reverse toprocess)
    _ -> Right []

computeItemHistory behaviors_ account0 previousState (sm'gl'seq:sm'gls) 
  | Just Skip <- behaviorFor behaviors_ [] sm'gl'seq  =
      ((makeItemCostTransaction account0 mempty sm'gl'seq mempty (Transaction 0 0 0 "skipped (behavior)")) :) <$> computeItemHistory behaviors_ account0 previousState  sm'gls
computeItemHistory behaviors_ account0 previousState all_@(sm'gl'seq@(sm'gl, _seq):sm'gls) = let
  behavior = behaviorFor behaviors_ [] sm'gl'seq 
  faTransType = toEnum $ mergeTheseWith (FA.stockMoveType . entityVal)  (FA.glTranType . entityVal) const sm'gl
  smeM = preview here sm'gl
  gleM = preview there sm'gl
  smM = entityVal <$> smeM
  glM = entityVal <$> gleM
  moveQuantityM = FA.stockMoveQty <$> smM
  moveCostM = FA.stockMoveStandardCost  <$> smM
  faAmountM = FA.glTranAmount <$> glM
  in case (faTransType, previousState) of
    ------------------- Initial state, --------------------------------------------
    (_             , Initial) ->
        computeItemHistory behaviors_ account0 (WaitingForStock mempty []) all_
    (ST_SUPPINVOICE, WithPrevious allowN previous) | Just faAmount <- faAmountM, fmap FA.glTranMemo glM == Just "GRN Provision"  ->
      --- ^^^ dont' see this invoice as a real one. It can happend when a GRN as an invoice
      -- but there is a different of price when the invoice is processed (exchange rate differnt or price updated manually)
      -- Waiting For Stock
      let (newSummary, newTrans) = if qoh previous == 0
                                   -- ^ the adjustement is updating a stock which should be null and stay null (0 items on hand, 0 comming)
                                   -- therefore processing it do adjust the cost price doesn't make sense.
                                   then let (newSummary, trans) = updateSummaryFromAmount previous 0 0 faAmount
                                        in (newSummary {stockValue = 0, expectedBalance =  0} , trans { tComment = "Z - GRN provision"})
                                   else updateSummaryFromAmount previous 0 0 faAmount
      in ((makeItemCostTransaction account0 previous sm'gl'seq newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious allowN newSummary)  sm'gls
    (ST_SUPPRECEIVE, (WaitingForStock previous toprocess)) | Just _ <- faAmountM -> 
        historyForGrnInvoice behaviors_ account0 previous sm'gl'seq [] toprocess sm'gls
    (ST_SUPPRECEIVE, (WaitingForStock previous toprocess)) | behavior `elem` map Just [UseMoveCost, UsePreviousCost] -> 
        historyForGrnInvoice behaviors_ account0 previous sm'gl'seq [] toprocess sm'gls
    (ST_SUPPRECEIVE, (WaitingForStock previous toprocess)) -> 
        computeItemHistory behaviors_ account0 (SupplierGRNWaitingForInvoice previous sm'gl'seq toprocess) sm'gls
    (ST_INVADJUST, WaitingForStock previous toprocess) | Just qty <- moveQuantityM
                                                       , isJust faAmountM || qty > 0 -> 
        historyForGrnInvoice behaviors_ account0 previous sm'gl'seq [] toprocess sm'gls
    (ST_WORKORDER, WaitingForStock previous toprocess) | Just _ <- moveQuantityM
                                                       , Just _ <- faAmountM  ->
        historyForGrnInvoice behaviors_ account0 previous sm'gl'seq [] toprocess sm'gls
    (ST_SUPPINVOICE, (WaitingForStock previous toprocess))->
        computeItemHistory behaviors_ account0 (SupplierInvoiceWaitingForGRN previous sm'gl'seq toprocess) sm'gls
    (_, (WaitingForStock previous toprocess)) ->
        computeItemHistory behaviors_ account0 (WaitingForStock previous (sm'gl'seq: toprocess)) sm'gls
    ------------------- Waiting for Supplier invoice ------------------------------
    (ST_SUPPINVOICE, SupplierGRNWaitingForInvoice previous grn toprocess)  ->
        historyForGrnInvoice behaviors_ account0 previous grn [sm'gl'seq] toprocess sm'gls
    -- GRN which have been "removed" can match their matching counterpart
    (ST_SUPPRECEIVE, SupplierGRNWaitingForInvoice previous grn toprocess) | Just qty <- moveQuantityM
                                                                          , Just (Entity _ grnMove) <- preview here (fst grn)
                                                                          , FA.stockMoveQty grnMove == -qty   ->
        historyForGrnInvoice behaviors_ account0 previous (nullifyQuantity grn) [nullifyQuantity sm'gl'seq] toprocess (sm'gls)
    (_              , SupplierGRNWaitingForInvoice previous grn toprocess)  ->
        computeItemHistory behaviors_ account0 (SupplierGRNWaitingForInvoice previous grn (sm'gl'seq:toprocess)) sm'gls
    -------------------------------- Waiting for Grn ------------------------------
    (ST_SUPPRECEIVE, SupplierInvoiceWaitingForGRN previous inv toprocess)  ->
        historyForGrnInvoice behaviors_ account0 previous sm'gl'seq [inv] toprocess sm'gls 
    (_             , SupplierInvoiceWaitingForGRN previous inv toprocess)  ->
        computeItemHistory behaviors_ account0 (SupplierInvoiceWaitingForGRN previous inv (sm'gl'seq:toprocess)) sm'gls
    -------------------------------- Supplier Invoice------------------------------
    (ST_SUPPINVOICE, WithPrevious allowN previous) | Just quantity <- moveQuantityM
                                            , Just moveCost <- moveCostM
                                            , Just faAmount <- faAmountM  ->
      let (newSummary, newTrans) = updateSummaryFromAmount previous quantity moveCost faAmount
      in ((makeItemCostTransaction account0 previous sm'gl'seq newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious allowN newSummary)  sm'gls
    (ST_SUPPINVOICE, WithPrevious _ previous) ->
      computeItemHistory behaviors_ account0 (SupplierInvoiceWaitingForGRN previous sm'gl'seq []) sm'gls
    -------------------------------- Supplier GRN------------------------------
    (ST_SUPPRECEIVE, WithPrevious allowN previous) | Just quantity <- moveQuantityM 
                                            , Just moveCost <- moveCostM
                                            , Just faAmount <- faAmountM  ->
      let (newSummary, newTrans) = updateSummaryFromAmount previous quantity moveCost faAmount
      in ((makeItemCostTransaction account0 previous sm'gl'seq newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious allowN newSummary)  sm'gls

    (ST_SUPPRECEIVE, WithPrevious _ previous) | behavior `elem` map Just [UseMoveCost, UsePreviousCost], False -> 
      historyForGrnInvoice behaviors_ account0 previous sm'gl'seq [] [] sm'gls 
    (ST_SUPPRECEIVE, WithPrevious _ previous) -> 
      computeItemHistory behaviors_ account0 (SupplierGRNWaitingForInvoice previous sm'gl'seq []) sm'gls
    --  Inventory Adjustment
    --  We should use the cost move when it is a genuine adjustment
    --  but use  the fa amount when it is a rename : so that both items transaction matches
    --  we do that by checking the person_id 
    (ST_INVADJUST, WithPrevious allowN previous) | Just quantity <- moveQuantityM
                                            , Just moveCost <- moveCostM
                                            -- , quantity + qoh previous /= 0
                                             -- if 0 adjustment to set the stock to 0 therefore the gl balance to 0 to
                                             ,    (smM >>= FA.stockMovePersonId) == Just 2
                                             -- or rename
                                            , Just faAmount <- faAmountM  ->
      let (newSummary, newTrans) = updateSummaryFromAmount previous quantity moveCost faAmount
      in ((makeItemCostTransaction account0 previous sm'gl'seq newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious allowN newSummary)  sm'gls
    -------------------------------- Location Tranfer ------------------------
    (ST_LOCTRANSFER, WithPrevious allowN previous ) -> -- skip
      ((makeItemCostTransaction account0 previous sm'gl'seq previous (Transaction 0 0 0 "skipped")) :) <$> computeItemHistory behaviors_ account0 (WithPrevious allowN previous)  sm'gls
    -------------------------------- Transaction not affecting the cost price
    (_,             WithPrevious PreventNegative previous)              | Just quantity <-  moveQuantityM 
                                                        , quantity /= 0
                                                        , qoh previous + quantity < 0 -> -- negative quantities
        computeItemHistory behaviors_ account0 (WaitingForStock previous [sm'gl'seq]) sm'gls
    (_            , WithPrevious allowN previous)              | Just quantity <-  moveQuantityM  ->
      let (newSummary, newTrans) = updateSummaryQoh previous quantity  (fromMaybe 0 faAmountM)
      in ((makeItemCostTransaction account0 previous sm'gl'seq newSummary ( newTrans {tComment = tComment newTrans <> " " <> tshow allowN})) :)
         <$> computeItemHistory behaviors_ account0 (WithPrevious allowN newSummary)  sm'gls
    (_            , WithPrevious allowN previous)              | amount <- fromMaybe 0 faAmountM ->
      let newSummary  = previous <> (mempty {faBalance = amount}) 
          newTrans = Transaction 0 0 amount ("Fallback " <> tshow allowN)
      in ((makeItemCostTransaction account0 previous sm'gl'seq newSummary newTrans) :) <$> computeItemHistory behaviors_ account0 (WithPrevious allowN newSummary)  sm'gls


-- | combine the GRN and invoices and process all the pending transaction (in reverse order)
-- check beforehand if the next transaction is not a invoice as well (from the same transaction)
historyForGrnInvoice :: BehaviorMap -> Account -> RunningState -> Matched -> [Matched] ->  [Matched] -> [Matched] -> Either Text [ItemCostTransaction]
historyForGrnInvoice behaviors_ account0 previous grn (invs@(inv:_)) toprocess (sm'gls)
  | Just (Entity _ gl) <- preview there (fst inv)
  , (similars@(_:_), leftover) <- partition (\sm'gl -> case preview there (fst sm'gl) of
                                               Just (Entity _ gl1) -> FA.glTranType gl1 == FA.glTranType gl && FA.glTranTypeNo gl1 == FA.glTranTypeNo gl
                                                               && FA.glTranMemo gl1 == "GRN Provision"
                                               _ -> False
                                          )
                                          (reverse toprocess ++ take 1 sm'gls)
  = historyForGrnInvoice behaviors_ account0 previous grn (reverse similars ++ invs) [] (leftover ++ drop 1 sm'gls)
historyForGrnInvoice behaviors_ account0 previous grn invs toprocess sm'gls = let
  smeM = preview here (fst grn)
  smM = entityVal <$> smeM
  behavior = behaviorFor behaviors_ [ForGrnWithoutInvoice] grn
  allInvoices = reverse invs
  in case (smM ) of
    (Just sm) -> 
      (case (mapMaybe (preview there .fst) (grn: allInvoices), behavior) of
        ([], Just UsePreviousCost) -> Right $ updateSummaryFromCost "Previous'" previous (FA.stockMoveQty sm) (standardCost previous) 0
        ([], Just UseMoveCost) -> Right $ updateSummaryFromCost "Move'" previous (FA.stockMoveQty sm) (FA.stockMoveStandardCost sm) 0
        ([], Nothing) | FA.stockMoveQty sm == 0    -> Right $ updateSummaryFromCost "Voided" previous (FA.stockMoveQty sm) 0 0
        ([], _) -> Left $ "No grn-without-invoice behavior defined for " <> showForError grn 
        (gls, _)       -> let glAmount = sum [ FA.glTranAmount inv 
                               | (Entity _ inv) <- gls
                               ]
                   in Right $ updateSummaryFromAmount previous (FA.stockMoveQty sm) (FA.stockMoveStandardCost sm) glAmount
      ) >>= (\(newSummary, newTrans) -> 
        ((makeItemCostTransaction account0 previous grn newSummary newTrans
          : [ makeItemCostTransaction account0 newSummary inv newSummary (Transaction 0 0 0 " |  balance updated with GRN")
            | inv <- allInvoices
            ]) 
          ++) <$> computeItemHistory behaviors_ account0 (WithPrevious PreventNegative newSummary) (reverse toprocess ++ sm'gls)
          )
    _ -> Left $ "Unexpected happended. Grn should be a GRN and inv a Supplier Invoice " <> tshow (entityKey <$> preview there (fst grn))


-- *** Update Running State
-- | Update the running state given a quantity and cost (and check overall amount)
updateSummaryFromAmount :: RunningState -> Double -> Double -> Double -> (RunningState, Transaction)
updateSummaryFromAmount previous 0 givenCost amount =  
    ( previous <> RunningState  0
                              givenCost
                              amount
                              amount
                              amount
    , Transaction 0 givenCost amount $ "Q0" )
updateSummaryFromAmount previous quantity givenCost amount =  let
  -- check if the original cost matches the given one (modulo rounding error)
  -- if so use the original 
  in if round (givenCost * quantity * 100) == round (amount * 100)
  then 
    ( previous <> RunningState  quantity
                              givenCost
                              (quantity*givenCost)
                              amount
                              amount
    , Transaction quantity givenCost amount $ "STICKY " <> tshow (amount/quantity) <> " -> " <> tshow givenCost )
  else
    let cost = amount / quantity
    in ( previous <> RunningState  quantity
                              cost
                              (quantity*cost)
                              (round2 $ quantity*cost)
                              amount
    , Transaction quantity cost amount $ "UNSTICKY " <> tshow (amount/quantity) <> " <> " <> tshow givenCost )


updateSummaryFromCost :: Text -> RunningState -> Double -> Double -> Double -> (RunningState, Transaction)
updateSummaryFromCost source previous quantity givenCost faAmount = let
  amount = givenCost * quantity
  rounded = round2 amount
  in  ( previous <> RunningState  quantity
                              givenCost
                              amount
                              rounded
                              faAmount
     , Transaction quantity givenCost amount $ "Q*" <> source <> "Cost")


updateSummaryQoh :: RunningState -> Double -> Double -> (RunningState, Transaction)
updateSummaryQoh previous quantity faAmount | standardCost previous == 0, quantity /= 0 = let
  -- use amount
  cost = amount / quantity
  amount = faAmount
  rounded = round2 amount
  in  ( previous <> RunningState  quantity
                              cost
                              amount
                              rounded
                              faAmount
     , Transaction quantity cost amount $ "Q*0" )
updateSummaryQoh previous quantity faAmount = let
  cost = standardCost previous
  amount = cost * quantity
  rounded = round2 amount
  in  ( previous <> RunningState  quantity
                              cost
                              amount
                              rounded
                              faAmount
     , Transaction quantity cost amount $ "Q*previousCost" )


makeItemCostTransaction :: Account -> RunningState-> Matched -> RunningState -> Transaction -> ItemCostTransaction
makeItemCostTransaction (Account account0) previous (sm'gl, _) new trans =  let
       smM = preview here sm'gl
       glM = preview there sm'gl
       date  = gett FA.stockMoveTranDate FA.glTranTranDate sm'gl
       moveId = FA.unStockMoveKey . entityKey <$> smM
       glDetail = FA.unGlTranKey . entityKey <$> glM
       faTransNo = gett FA.stockMoveTransNo FA.glTranTypeNo sm'gl
       faTransType = toEnum $ gett FA.stockMoveType FA.glTranType sm'gl
       sku = gett (Just . FA.stockMoveStockId) FA.glTranStockId sm'gl
       -- TODO : everything below
       account = maybe account0 (FA.glTranAccount . entityVal) (glM)
       faAmount = maybe 0 (FA.glTranAmount . entityVal) (glM)
       moveCost = maybe 0 (FA.stockMoveStandardCost . entityVal) (smM)
       in ItemCostTransaction
              { itemCostTransactionDate = date
              , itemCostTransactionMoveId = moveId
              , itemCostTransactionGlDetail = glDetail
              , itemCostTransactionFaTransNo = faTransNo
              , itemCostTransactionFaTransType = faTransType
              , itemCostTransactionSku = sku
              , itemCostTransactionAccount = account
              , itemCostTransactionFaAmount = faAmount
              , itemCostTransactionCorrectAmount = round2 $ tAmount trans
              , itemCostTransactionQohBefore = qoh previous
              , itemCostTransactionQohAfter = qoh new
              , itemCostTransactionQuantity = tQuantity trans
              , itemCostTransactionCostBefore = standardCost previous
              , itemCostTransactionCost = tCost trans
              , itemCostTransactionMoveCost = moveCost
              , itemCostTransactionCostAfter = standardCost new
              , itemCostTransactionStockValue = stockValue new
              , itemCostTransactionFaStockValue = faBalance new
              , itemCostTransactionItemCostValidation = Nothing
              , itemCostTransactionComment = tComment trans
             }
  
  
  

-- ***    PRocess transactions

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
            (moves, cancellingPairs) = case removeCancellingMoves $ mapMaybe getFirst $ toList $ groupAsMap entityKey (First . Just) $ mapMaybe (preview here . fst) m'gs of
                ([], cancellings) |  length cancellings == glLength && length cancellings  * glLength == length m'gs -> (cancellings, [])
                 -- ^ ignore cancelling pairs in case the gls cancel each other as well
                 --  happened when "removing" a GRN
                 --  the GRN is modified to have a negative stock + corresponding gl
                -- ([], _) |  glLength == 0 ->  ([], [])
                -- ^ filter out totally cancelling moves. Appears when GRN have been removed or inventory transfer
                m'c -> m'c
            glLength = length gls
            gls = mapMaybe getFirst $ toList $ groupAsMap entityKey (First . Just) $ mapMaybe (preview there . fst) m'gs
            in ( case (length moves , length gls) of
                 (0, 0 ) ->  Right []
                 (0, _ ) -> Right $ map (\g -> (That g, seqN)) gls
                 (_, 0 ) -> Right $ map (\m -> (This m, seqN)) moves
                 (moveLength, _) | moveLength == glLength && (moveLength + length cancellingPairs) * glLength == length m'gs -> Right $ zipWith (\m g -> (These m g, seqN)) moves gls 
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
                      Just (Entity _ summary) -> return . Just $ Left summary { itemCostSummaryAccount = acc, itemCostSummaryFaStockValue = 0}
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
                                , itemCostSummaryFaStockValue = 0
                                , itemCostSummaryValidated = False
                                }

        _ -> return Nothing

      


collectCostTransactions :: Day -> Account -> (Maybe Text) -> Handler (Either (Text, [Matched]) ())
collectCostTransactions date account skum = do
  lastEm <- loadInitialSummary account skum
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let endDatem = Just . maybe date (min date)  $ skum >>= itemSettings settingsm account >>= closingDate
      lastm = either id entityVal <$> lastEm
      behaviors_ = fromMaybe mempty (settingsm >>= behaviors)
  trans0 <- loadMovesAndTransactions lastm endDatem account skum
  let transE = computeItemCostTransactions behaviors_ lastm account trans0
  forM transE $ \trans -> 
    runDB $ 
       case lastMay trans of
          Nothing -> return ()
          Just summary  -> do
             insertMany_ trans
             let itemCostSummaryDate = fromMaybe (error "Unexpected happen") $ maximumMay $ map itemCostTransactionDate  trans
                 itemCostSummaryMoveId = maximumMay $ mapMaybe itemCostTransactionMoveId trans :: Maybe Int
                 itemCostSummaryGlDetail = maximumMay $ mapMaybe itemCostTransactionGlDetail trans
                 itemCostSummarySku = itemCostTransactionSku summary
                 itemCostSummaryAccount = itemCostTransactionAccount summary
                 itemCostSummaryQohAfter = itemCostTransactionQohAfter summary
                 itemCostSummaryCostAfter = itemCostTransactionCostAfter summary
                 itemCostSummaryStockValue = itemCostTransactionStockValue summary
                 itemCostSummaryFaStockValue = itemCostTransactionFaStockValue summary
                 itemCostSummaryValidated = abs (itemCostSummaryStockValue - itemCostSummaryFaStockValue) < 1e-2 && all (isJust . itemCostTransactionItemCostValidation) trans
             case lastEm of
               Just (Right (Entity key _)) -> repsert key ItemCostSummary{..}
               _ -> insert_ ItemCostSummary{..}
     
 

-- * Fixing
-- ** Balance
-- Generates a journal entry to balance all summary
fixGLBalance :: Day -> [Entity ItemCostSummary] -> Handler (Maybe (Entity ItemCostValidation))
fixGLBalance date summaries = do
 -- today <- todayH
  settings <- getsYesod appSettings
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      ref = skuRange summaries
      (journals, total) = generateJournals chunkSize  date sum'accounts
      sum'accounts = [ ( e
                       ,  fromMaybe (error $ "No fixing account set up for Account : " <> unpack itemCostSummaryAccount)
                                    (accountm <|> defaultAccount)
                       )
                     | e@(Entity _ ItemCostSummary{..}) <- summaries
                     , let accountm = accountMap >>= lookup (Account itemCostSummaryAccount)  >>= fixAccount
                     ]
      defaultAccount = appCheckItemCostSetting settings >>= defaultFixAccount
      chunkSize = fromMaybe 200 (appCheckItemCostSetting settings >>= batchSize)
      accountMap = accounts <$> appCheckItemCostSetting settings
      validate faIds =  do
            let comment = "Fix GL Balance " <> ref --  (on " <> tshow today  <> ")"
            _ <- mapM (refreshSummary date) summaries
            validationm <- validateFromSummary date comment summaries total
            forM validationm $ \v@(Entity validation _)  -> do
                let mkTransMap journalId=  TransactionMap ST_JOURNAL journalId ItemCostValidationE (fromIntegral $ fromSqlKey validation) False
                runDB $ insertMany_  $ map mkTransMap  faIds
                return v
  case (journals, summaries) of
    ([], []) ->
      setInfo "No summary to post" >> return Nothing
    ([], _) -> do
      setInfo "Nothing to post : all GL line are 0, or fixing date before summary date."
      validate []
    (_, _) -> do
        faIdsE <- liftIO $  mapM (WFA.postJournalEntry connectInfo) journals
        case sequence faIdsE of
          Left e -> setError (toHtml e) >> return Nothing
          Right faIds -> do
            mapM setStockIdFromMemo faIds
            validate faIds

skuRange :: [(Entity ItemCostSummary)] ->  Text
skuRange summaries = 
  let account'skus = [ itemCostSummaryAccount <> maybe "" ("/"<>) itemCostSummarySku
                     | (Entity _ ItemCostSummary{..}) <- summaries
                     ]
  in intercalate " - " $ ( nub $ mapMaybe ($ account'skus) [minimumMay, maximumMay] ) <>   [tshow $ length summaries]
-- | Generates a JournalEntry ready to be posted to FA
-- returns Nothing if there is nothing to do (no line nonnull)
generateJournals :: Int -> Day -> [(Entity ItemCostSummary, Account)] -> ([WFA.JournalEntry], Double)
-- generateJournals :: Day -> [(Entity ItemCostSummary, Account)] -> Maybe (WFA.JournalEntry)
generateJournals chunkSize date sum'accounts = 
  let (glss''es'total ) =  
            [ (([mkItem itemCostSummaryAccount itemCostSummarySku amount memo
              ,mkItem (fromAccount adjAccount) itemCostSummarySku (-amount) memo
              ]
              ,  e) -- we need to know which entity have been filtered or not
              , amount
              )
              -- so that we can call sku range to set the journal memo
            |   (e@(Entity _ s@ItemCostSummary{..}), adjAccount) <-  sum'accounts
            , let stockValue = if abs itemCostSummaryQohAfter < 1e-2
                               then 0
                               else itemCostSummaryStockValueRounded s
            , let amount =  toDecimalWithRounding (Round 2) $ stockValue - itemCostSummaryFaStockValue
            , abs amount  > 0
            , let memo = pack $ "Clear GL Balance from " <>  formatDouble itemCostSummaryFaStockValue <> " to " <> formatDouble stockValue :: Text
            , date >= itemCostSummaryDate -- only fix balance after the current summary
            ]
      mkItem account skum gliAmount memo   =
                WFA.GLItem { gliDimension1 = Nothing
                           , gliDimension2 = Nothing
                           , gliTaxOutput = Nothing
                           , gliMemo = Just $ memo <> maybe "" (":stock_id=" <>) skum
                           , gliAccount = WFA.GLAccount account
                           -- ^ hack to set the stock_id in the gl_trans table
                           -- updated afterward with a SQL query
                           , ..
                           }

      mkJournal gls'total = case splitAt chunkSize gls'total of
                      ([], _ ) -> Nothing
                      (s1''e'totals, s2) -> let (ss1, es) = unzip s1'e
                                                (s1'e, sum -> total) =  unzip s1''e'totals
                                                s1 = concat ss1
                                             in Just ( WFA.JournalEntry date Nothing s1 (Just $ "Stock balance adjustment : " <> tshow total <> " " <> skuRange es  <> " (fames)")
                                                , s2)
  in (unfoldr mkJournal glss''es'total, fromRational $ toRational $ sum (map snd glss''es'total))
  

setStockIdFromMemo :: Int -> Handler ()
setStockIdFromMemo fa_trans_no =  do
  let sql = "UPDATE 0_gl_trans "
          <> "SET stock_id = REGEXP_REPLACE(memo_, \".*:stock_id=(.*)$\", \"\\\\1\") "
          <> ", memo_ = REGEXP_REPLACE(memo_, \"(.*):stock_id=.*\", \"\\\\1\") "
          <> " WHERE memo_ like '%:stock_id=%' AND type_no = ? AND type = ? "
  runDB $ rawExecute sql [toPersistValue fa_trans_no, toPersistValue (fromEnum ST_JOURNAL)]

-- | collect new transaction on summary' day and update it.
-- Used after fixing the GL balance
refreshSummary :: Day -> Entity ItemCostSummary -> Handler (Either (Text, [Matched]) ())
refreshSummary day e = do
  let summary = entityVal e
  collectCostTransactions day (Account $ itemCostSummaryAccount summary) (itemCostSummarySku summary) 


-- ** Cost update
-- Update the standard cost using FA
-- on the given date, but only on the style
-- where the last transaction is before that date
updateCosts :: [Entity ItemCostSummary] -> Handler (Maybe (Entity ItemCostValidation))
updateCosts summaries = do
  settings <- getsYesod appSettings
  today <- todayH
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
  faIdms  <-  mapM (updateCost connectInfo today) summaries
  case catMaybes faIdms of
    [] -> setInfo "Nothing to update" >> return Nothing
    faId'totals -> do
     setInfo "Cost updates" 
     userId <- requireAuthId
     today <- todayH
     let comment = "Cost update - " <> skuRange summaries
         mkTransMap vid faId = TransactionMap ST_COSTUPDATE faId ItemCostValidationE (fromIntegral $ fromSqlKey vid) False
         total = sum $ map snd faId'totals
     runDB $ do
       let validation = ItemCostValidation comment userId today today False total
       vId <- insert validation
       insertMany_ (map (mkTransMap vId . fst) faId'totals)
       return $ Just (Entity vId validation)
  
updateCost :: WFA.FAConnectInfo -> Day ->  Entity ItemCostSummary -> Handler (Maybe (Int, Double))
updateCost connectInfo today (Entity _ summary@ItemCostSummary{..}) | Just sku <- itemCostSummarySku   = do
  let sql = "select material_cost from 0_stock_master "
            <> " where stock_id = ? " 
      
  pendings <- loadMovesAndTransactions (Just summary) Nothing (Account itemCostSummaryAccount) itemCostSummarySku
  if null pendings
  then do
    rows <- runDB $ rawSql sql [toPersistValue itemCostSummarySku ]
    case rows of
      [Single currentCost] | abs (currentCost - itemCostSummaryCostAfter) > 1e-6 -> do
            faIdm <- liftIO $ WFA.postCostUpdate connectInfo (WFA.CostUpdate sku itemCostSummaryCostAfter)
            case faIdm of
              Right (Just faId) -> do
                runDB $ insert $ FA.Comment (fromEnum ST_COSTUPDATE) faId (Just today) (Just $ sku  <> " Fames Cost Adjustement")
                return $ Just (faId, round2 $ itemCostSummaryStockValue - currentCost * itemCostSummaryQohAfter)
              Right Nothing -> return Nothing
              Left err  -> error $ unpack err
      [_] -> return $ Nothing
      _ -> error $ "Item " <> unpack sku <> " doesn't exist in 0_stock_master "
  else do
    logInfoN ("Skip cost update for " <> sku) 
    return Nothing -- nothing should be pending, refuse to update the cost
updateCost _ _ _ = return $ Nothing
    
    




  

-- * Validation
validateFromSummary :: Day -> Text -> [Entity ItemCostSummary] -> Double -> Handler (Maybe (Entity ItemCostValidation))
-- validateFromSummary _date _comment [] = return Nothing
validateFromSummary date comment summaries total = do
  userId <- requireAuthId
  let validation = ItemCostValidation comment userId date lastTransaction False total
      lastTransaction = fromMaybe date $ maximumMay $ map (itemCostSummaryDate . entityVal) summaries
  runDB $ do
      key <- insert validation
      forM summaries $ \(Entity sumkey ItemCostSummary{..}) -> do
        updateWhere [ ItemCostTransactionSku ==. itemCostSummarySku
                    , ItemCostTransactionAccount ==. itemCostSummaryAccount
                    , ItemCostTransactionItemCostValidation ==. Nothing
                    ]
                    [ ItemCostTransactionItemCostValidation =. Just key ]
                    -- set summary to validated, if balance matches
        updateWhere [ItemCostSummaryId ==. sumkey]
                    [ItemCostSummaryValidated =. True ]
      return . Just $ Entity key validation


voidValidation :: Key ItemCostValidation -> Handler Int
voidValidation key = do
  today <- todayH
  settings <- getsYesod appSettings
  let vId = fromSqlKey key
      connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      commentFn = const . Just $ "Item cost Validation # " <> tshow vId <> " voided"
  trans <- runDB $ selectList [ TransactionMapEventType ==. ItemCostValidationE
                               , TransactionMapEventNo ==. fromIntegral vId
                               , TransactionMapVoided ==. False
                               ] []
  e <- runExceptT $ mapM (\tran -> voidFATransaction connectInfo today (commentFn . entityVal $ tran) tran) $ trans
  case e of
    Left err -> error (unpack err)
    Right _ -> do
      runDB $ updateWhere  [ItemCostValidationId ==. key] [ItemCostValidationVoided =. True, ItemCostValidationTotal =. 0]
      return (length trans)

-- TODO factorize with Handler.GL.Payroll.Common
voidFATransaction :: WFA.FAConnectInfo -> Day -> Maybe Text -> Entity TransactionMap -> ExceptT Text Handler ()
voidFATransaction connectInfo vtDate comment (Entity __tId TransactionMap{..}) = do
  let vtTransNo = transactionMapFaTransNo
      vtTransType = transactionMapFaTransType
      vtComment = Just $ fromMaybe "Voided by Fames" comment
  ExceptT $ liftIO $ WFA.postVoid connectInfo WFA.VoidTransaction{..}
  -- mark the transaction as voided (all the rows)  not just the one matching the curren entity
  lift $  runDB $ updateWhere [ TransactionMapFaTransType ==. transactionMapFaTransType
                              , TransactionMapFaTransNo ==. transactionMapFaTransNo
                              ] [TransactionMapVoided =. True]
  return ()
    
  

--
-- * sanity check
-- | Detect all Sku/Account which look suspect
-- (not much variation in cost price, too much discrepency between FA and calculated etc ...
loadCheckInfo :: Handler [CheckInfo]
loadCheckInfo = do
  let sql0 = "SELECT  account, sku "
         ++ " , MAX(IF(fa_trans_type IN (20,25), 0, abs(fa_amount - correct_amount))) as amount_discrepency "
         ++ " , MAX(IF(fa_trans_type IN (20,25) OR move_cost = 0, 0, abs(move_cost - cost))) as cost_discrepency "
          ++ ", MAX(qoh_after < 0) as negative_qoh "
         ++ " , MAX(IF(fa_trans_type IN (20,25) OR abs(cost_before*cost_after) < 1e-2, 0, abs(cost_after - cost_before) )) as cost_variation"
         --             ^ the cost can change between a GRN and its invoice. We only case if the dodgy cost price has an impact (ie has been used instead of being calculated)
          ++ " FROM check_item_cost_transaction "
          ++ " WHERE fa_trans_type NOT IN (16)" -- filter location transfer
          ++ " AND item_cost_validation IS NULL "
          ++ " GROUP BY account, sku "
      sql = "SELECT check_info.* "
          ++ ", IF(abs(qoh_after) < 1e-2 AND abs(fa_stock_value) > 0.1,fa_stock_value,0) as null_stock_fa "
          ++ ", IF(abs(qoh_after) < 1e-2 AND abs(stock_value) > 0.1,stock_value,0) as null_stock "
          ++ " FROM  ( " ++ sql0++ ") AS check_info  "
          ++ " LEFT JOIN check_item_cost_summary USING(sku, account) "
          ++ " HAVING abs(amount_discrepency) >1 OR abs(cost_discrepency) > 0.5 OR negative_qoh > 0 OR abs(cost_variation) > 0.5 OR abs(null_stock) > 1e-2 OR abs(null_stock_fa) > 1e-2"
           
      mkCheck (Single account
              , Single icSku
              , Single icAmountDiscrepency
              , Single icCostDiscrepency
              , Single icNegativeQOH
              , Single icCostVariation
              , Single icNullFAStockDiscrepency
              , Single icNullStockDiscrepency
              ) = CheckInfo{icAccount=Account account ,..}

  rows <- runDB $ rawSql sql []
  return $ map mkCheck rows

-- *  Utils
itemSettings :: Maybe Settings -> Account -> Text -> Maybe ItemSettings
itemSettings settingsm account sku = 
  settingsm >>= lookup account . accounts >>= lookup sku . items 

-- We want "normal" rounding (sames as the one used by Mysql and php)
-- opposed to Haskell rounding (bankers rounding) 
-- round2 = (/100) . fromIntegral . round . (*100) 
round2 :: (RealFrac a, Fractional b) => a -> b
round2 =  fromRational . toRational . toDecimalWithRounding' 20 (Round  2) 

itemCostSummaryStockValueRounded :: ItemCostSummary -> Double
itemCostSummaryStockValueRounded = round2 . itemCostSummaryStockValue
itemCostTransactionStockValueRounded :: ItemCostTransaction -> Double
itemCostTransactionStockValueRounded = round2 . itemCostTransactionStockValue


nullifyQuantity :: Matched -> Matched
nullifyQuantity (This (Entity moveId move), seq) = (This (Entity moveId move { FA.stockMoveQty = 0, FA.stockMoveStandardCost = 0}) , seq)
nullifyQuantity (These (Entity moveId  move) gl, seq) = (These (Entity moveId move { FA.stockMoveQty = 0, FA.stockMoveStandardCost = 0}) gl , seq)
nullifyQuantity m = m

gett ::  (FA.StockMove -> a) -> (FA.GlTran -> a) -> These (Entity FA.StockMove) (Entity FA.GlTran) -> a
gett fa fb = mergeTheseWith (fa . entityVal) (fb . entityVal) const

-- | Show reference in error message
showForError :: Matched -> Text
showForError (This (Entity moveId FA.StockMove{..}), _) =
  tshow (toEnum stockMoveType :: FATransType) <> " " <> tshow stockMoveType <> "/" <> tshow  stockMoveTransNo
  <> " " <> tshow (FA.unStockMoveKey moveId)
  <> " " <> stockMoveStockId
showForError (These (Entity moveId FA.StockMove{..}) (Entity glId _), _ ) =
  tshow (toEnum stockMoveType :: FATransType) <> " " <> tshow stockMoveType <> "/" <> tshow  stockMoveTransNo
  <> " " <> tshow (FA.unStockMoveKey moveId) <> "," <> tshow (FA.unGlTranKey glId )
  <> " " <> stockMoveStockId
showForError (That (Entity glId FA.GlTran{..}), _ ) =
  tshow (toEnum glTranType :: FATransType) <> " " <> tshow glTranType <> "/" <> tshow  glTranTypeNo
  <>  tshow (FA.unGlTranKey glId )
  <> " " <> fromMaybe "" glTranStockId
-- * Behavior

behaviorFor :: BehaviorMap -> [BehaviorSubject] -> Matched -> Maybe Behavior
behaviorFor behaviors_ subjects (sm'gl, _) = let
  faTransNo = gett FA.stockMoveTransNo FA.glTranTypeNo sm'gl
  faTransType = gett FA.stockMoveType FA.glTranType sm'gl
  other = ForSku (gett (Just . FA.stockMoveStockId) FA.glTranStockId sm'gl)
        : if (gett FA.stockMoveStandardCost FA.glTranAmount sm'gl) == 0
           then [ForNullCost]
           else []

  in behaviorFor' behaviors_ (ForTransaction faTransType faTransNo : other ++ subjects)

behaviorFor' :: BehaviorMap -> [BehaviorSubject] -> Maybe Behavior
behaviorFor' behaviors_ subjects = 
  case asum $ map (flip lookup behaviors_)  subjects of
    Just (BehaveIf condition behavior) -> behaviorFor' (mapFromList  [(condition, behavior)]) subjects 
    b -> b
      
  
  
  
  



