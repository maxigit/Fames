{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This script perform the reconciliation between
-- bank statements from HSBC (UK) and  FrontAccounting,
-- ie it detects and displays the discrepencies between the two.
-- At the moment only amounts are taken into account, even though
-- dates are used to try to isolate the oddest transaction(s) in
-- in case there is many transaction mathcing the same amount.
-- It takes 3 types of inputs, HSBC statements, HSBC transactions
-- and FA statement reports.
-- Also, the report could be run on just a given period (corresponding
-- to one statement). It's probably easier to get as much transactions as possible
-- (.i.e from the beginning of time). This solves the problem of transaction pair
-- not appearing in the same "period" because the processing date (bank)
-- differs from the real transaction date (entered) in FA.

module BankReconciliate
where
  
import Control.Applicative
import Data.Maybe
import Control.Monad.State(State,evalState, get, put)

import Lens.Micro hiding(filtered)
import Lens.Micro.TH
import Data.Csv hiding(Options, (.:), lookup, record)
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.These
import Data.Align(align)
import Data.Map(Map)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import Data.Text (Text,strip, toLower)
import Data.Text.Encoding (decodeUtf8', decodeLatin1)

import System.FilePath.Glob (glob)
import System.Directory(getModificationTime)
import Data.Decimal
import Data.List (sortBy, sortOn, minimumBy, mapAccumL, dropWhileEnd, lookup)
import Data.Ord (comparing)
import Data.String
import Data.Time(Day, parseTimeM, formatTime, diffDays, addDays, UTCTime)
import Data.Time.Format(defaultTimeLocale)
import Data.Char(isSpace, isAscii)
import Util.ValidField

import GHC.Generics

import Data.Monoid
import Data.Generator(reduceWith)

import qualified Database.MySQL.Simple as SQL
-- import qualified Database.MySQL.Simple.QueryResults as SQL
import Prelude hiding(read)
import Text.Read(readMaybe)
import qualified Text.Parsec as P

import Text.Regex.TDFA() -- (=~))
import qualified Text.Regex.TDFA.ByteString as Rg
import qualified Text.Regex.Base as R
-- import qualified Text.Parsec.Char as P

read :: Read p => String -> p
read s = case readMaybe s of
  Nothing -> error $ "can't read ["   ++ s ++ "]"
  Just r -> r
-- * Inputs
-- ** Field converters

-- | Compare key ignoring the case and stripping
newtype CleanedRecord = CleanedRecord [(Text,  BS.ByteString)] deriving Show
cleanRecord :: NamedRecord  -> CleanedRecord
cleanRecord record =  CleanedRecord [ (clean k, v) | (k, v) <- H.toList record]


(.:) :: FromField a => CleanedRecord -> BS.ByteString -> Parser a
(CleanedRecord cleaned) .: key = maybe mempty parseField $ clean key `lookup` cleaned where
clean :: BS.ByteString -> Text
clean bs = strip . toLower . either (const $  decodeLatin1 bs) id $ decodeUtf8' bs
type Amount = Decimal

instance {-# OVERLAPPING #-} FromField (Maybe Decimal) where
    parseField f = do
        t <- parseField f
        case strip t of
            "" -> return Nothing
            _  -> Just <$> parseField f

instance FromField Decimal where
    parseField f = do
        s <- parseField f
        let _type = s :: String
        return (read s)


instance ToField Decimal where
    toField d = fromString (show d)

instance ToField Day where
    toField d = fromString $ formatTime defaultTimeLocale "%d %b %Y" d


parseTime :: [String ]-> String -> Parser Day
parseTime formats s = either fail return (readTimeE formats s)
readTimeE :: [String ]-> String -> Either String Day
readTimeE formats str= case concat $ map (parseTimeM True defaultTimeLocale) formats <*> [str] of
  [] -> Left $ "can't parse time : " ++ str
  [date] -> Right date
  _ -> Left $ "ambiguous parsing for time : " ++ str

parseDebit :: (FromField (Maybe b), Num b, Show b)
           => BS.ByteString -> BS.ByteString -> CleanedRecord -> Parser b
parseDebit debit credit r = do
    dAmount <- r .: debit
    cAmount <- r .: credit
    return $ case (dAmount, cAmount) of
        (Just a, Nothing) -> a
        (Nothing, Just a) -> -a
        (Nothing, Nothing) -> error $ "no debit or credit for " ++ show r
        (Just d, Just c)   -> error $ "debit and credit can't be set: "
                                  ++ show r
                                  ++ show d
                                  ++ "/"
                                  ++ show c




-- ** Basic types
-- Each type corresponds to a different input format.
-- | Transaction from FA statement report.
data FATransaction = FATransaction
       { _fType        :: !String
       , _fNumber      :: !Int
       , _fReference   :: !String
       , _fDate        :: !Day
       , _fObject      :: !String   --   ^ Can be Person, Item etc   ...
       , _fAmount      :: !Amount
       , _fBalance     :: !Amount
       , _fRecDate      :: !(Maybe Day)
       , _fPosition    :: !Int -- ^ position
       } deriving (Show, Read, Eq, Ord)

makeClassy ''FATransaction

instance FromNamedRecord (Int -> FATransaction) where
    parseNamedRecord record = pure FATransaction
                <*> r .: "Type"
                <*> r .: "#"
                <*> r .: "Reference"
                <*> (parseTime ["%Y/%m/%d"] =<<  r .: "Date")
                <*> r .: "Person/Item"
                <*> parseDebit "Debit" "Credit" r
                <*> r .: "Balance"
                <*> pure Nothing
                where r = cleanRecord record

-- | Read from DB
fetchFA :: Maybe Decimal -> SQL.ConnectInfo -> Int -> Maybe Day -> Maybe Day -> IO [FATransaction]
fetchFA balance0 cinfo bankAccount startM endM = do
    conn <- SQL.connect cinfo
    rows <- SQL.query_ conn (fromString sql)
    let      befores = case startM of
                     Nothing -> []
                     Just start -> takeWhile (\(_,_,_,day,_,_,_) -> day < start) rows
             prevBal = realFracToDecimal 2 $ sum (map (\(_,_,_,_,_,amount,_) -> amount) befores)
             balance = (fromMaybe 0 balance0) - prevBal
             (_, results) = mapAccumL tupleToFATransaction (balance, 1) rows
    -- writeFile "/home/max/Webshot/fetched.hs" (fromString $ show ("RESULT", results, "Rows" , rows, "balance0", balance0, "befores", befores, "prev", prevBal, "balance", balance))
    writeFile "/home/max/Webshot/fetched.hs" (fromString $ show ("RESULT [", results, "] Rows[" , rows, "] balance0 [", balance0, "] befores [", befores, " ]prev [", prevBal, "balance", balance, startM, endM))
    return results
    where q0 = "SELECT type, trans_no, ref, trans_date, CAST(person_id as CHAR(100)), amount, reconciled"
                         ++ " FROM 0_bank_trans"
                         ++ " WHERE amount <> 0 AND bank_act = "  ++ show bankAccount
          sql = reduceWith appEndo ws q0 ++ " ORDER BY trans_date, id"

          ws = catMaybes
                   [ startM <&> \start q -> q ++ " AND trans_date >= '"
                                        ++ format (addDays (-3) start) 
                                        ++ "'"
                   , endM   <&> \end   q -> q ++ " AND trans_date <= '"
                                        ++ format (addDays 7 end)
                                        ++ "'"
                   ]
          format d = formatTime defaultTimeLocale "%F" d
          -- as we load some transaction before the date corresponding to the balance
          -- we need to adjust the starting balance accordingly

          tupleToFATransaction :: (Amount, Int) ->  (Int, Int, String, Day, String, Double, Maybe Day) -> ((Amount, Int), FATransaction)
          tupleToFATransaction (bal, pos) (t,n,r,d,o,a,rd) =
            ((bal',pos+1), FATransaction (show t) n r d o a' bal' rd pos)
            where bal' = bal+a'
                  a' = realFracToDecimal 2 a

-- | HSBC Transaction
data HSBCTransactions = HSBCTransactions
    { _hDate :: !Day
    , _hType :: !String
    , _hDescription ::  !String
    , _hAmount :: !Amount
    , _hBalance :: !(Maybe (ValidField Amount))
    , _hDayPos :: !Int -- ^ position unique within a day, but doesn't have to start a one.
    } deriving (Show, Read, Eq, Ord)

makeClassy ''HSBCTransactions

instance FromNamedRecord (Int -> HSBCTransactions) where
    parseNamedRecord record = pure HSBCTransactions
      <*> (parseTime ["%e-%b-%y", "%e %b %0Y", "%F", "%d/%m/%Y"] =<< r .: "Date")
         <*> r .: "Type"
         <*> r .: "Description"
         <*> parseDebit "Paid in" "Paid out" r -- opposite to HSBC
         <*> (fmap Provided <$> r .: "Balance")
         where r = cleanRecord record

instance ToNamedRecord HSBCTransactions where
  -- we use %F to be easily sortable in excel
  toNamedRecord HSBCTransactions{..} = namedRecord [ "Date" .= formatTime defaultTimeLocale "%F" _hDate
                                    , "Type" .= _hType
                                    , "Description" .= _hDescription
                                    , "Paid in" .= paidIn
                                    , "Paid out" .= paidOut
                                    , "Balance" .= fmap validValue _hBalance
                                    ] where
    (paidIn, paidOut) = if _hAmount > 0
                        then (Just _hAmount, Nothing) 
                        else (Nothing, Just $ - _hAmount)

instance DefaultOrdered HSBCTransactions where
  headerOrder _ = V.fromList ["Date", "Type", "Description", "Paid in", "Paid out", "Balance"]

-- | HSBC Transaction (transaction download). Doesn't contain a balance
-- This data type is only used to read the statement.csv file
-- and to be converted into an HSBCTransactions.
-- Even though the files are called statement, they are actually not
-- statement as such and should not be mixed with real statement
-- The attributes don't need to be strict
-- as the data will be converted to strict attributse (from HSBCTransactions)
-- Not sure
data HSBCDaily = HSBCDaily
    { _hsDate :: Day
    , _hsDescription :: String
    , _hsAmount :: Amount
    , _hsDayPos :: Int
    } deriving (Show, Read, Generic)

instance FromRecord (Int -> HSBCDaily) where
    parseRecord r = HSBCDaily <$> (parseTime ["%e/%m/%Y"] =<< r .! 0)
                               <*> r .! 1
                               <*> r .! 2
-- | New HSBC Format after July 2019
-- we lose some information (like balance and type) which are now present
-- in the statement. That's not really a problem as we normally use the monthly statement
instance FromNamedRecord (Int -> HSBCDaily) where
  parseNamedRecord record = pure HSBCDaily
                <*> (parseTime ["%e %b %0Y"] =<< r .: "Date")
                <*> r .: "Description"
                <*> (r .: "Amount" <|> parseDebit "Paid in" "Paid out" r)
                where r = cleanRecord record

dailyToHTrans :: HSBCDaily -> HSBCTransactions
dailyToHTrans = HSBCTransactions <$> _hsDate
              <*> pure "Statement"
              <*> _hsDescription
              <*> _hsAmount
              <*> pure Nothing
              <*> _hsDayPos

-- | Paypal Statement
-- data PStatus
data PaypalTransaction = PaypalTransaction
     { _pDate :: Day
     , _pName :: String
     , _pType :: String
     -- , _pStatus :: PStatus
     , _pCurrency :: String
     , _pGross :: Amount
     , _pDayPos :: Int -- not in CSV, order
     } deriving (Show, Read, Generic)

makeClassy ''PaypalTransaction
instance FromNamedRecord (Int -> PaypalTransaction) where
  parseNamedRecord record = pure PaypalTransaction
                <*> (parseTime ["%e/%m/%Y"] =<< r .: "Date")
                <*> r .: " Name"
                <*> r .: " Type"
                -- <*> r .: "Status"
                <*> r .: " Currency"
                <*> r .: " Net" -- we want the net not the gross , as the gross include fees
                where r = cleanRecord record

pToS :: PaypalTransaction -> HSBCDaily
pToS = HSBCDaily <$> _pDate
                  <*> _pName
                  <*> _pGross
                  <*> _pDayPos
-- | Santander Statement
data SantanderTransaction = SantanderTransaction
     { _stDate :: Day
     , _stDescription :: String
     , _stAmount :: Amount
     , _stBalance :: Amount
     } deriving (Eq, Show)

data SantanderStatement = SantanderStatement
  { _stFrom :: Day
  , _stTo :: Day
  , _stAccount :: String
  , _stTrans :: [SantanderTransaction]
  } deriving (Eq, Show)

sToS :: SantanderTransaction -> Int -> HSBCDaily
sToS SantanderTransaction{..} pos = HSBCDaily
         _stDate
         _stDescription
         _stAmount
         pos
         
-- ** Parsec parser

parseSantanderStatement :: P.Stream s m Char
                        => P.ParsecT s u m SantanderStatement
parseSantanderStatement = do
  P.spaces
  -- date
  l_from <- either fail return . (readTimeE ["%e/%m/%Y"]) =<< parseAttribute "From:"
  l_to <- either fail return . (readTimeE ["%e/%m/%Y"]) =<< parseAttribute "to"
  -- account
  account <- parseAttribute "Account:"
  -- transactions
  trans <- many parseSantanderTransaction
  return $ SantanderStatement l_from l_to account trans

parseSantanderTransaction :: forall s (m :: * -> *) u. P.Stream s m Char
                          => P.ParsecT s u m SantanderTransaction
parseSantanderTransaction = do
  P.spaces
  date <- either fail return . (readTimeE ["%e/%m/%Y"]) =<< parseAttribute "Date:"
  desc <- fmap (dropWhile isSpace . dropWhileEnd isSpace ) (parseAttribute "Description:")
  amount <-  maybe (fail "Can't read amount") (return . normalizeDecimal) . readMaybe =<< parseAttribute "Amount:"
  balance <- maybe (fail "Can't read amount") (return . normalizeDecimal) . readMaybe =<< parseAttribute "Balance:"
  return $ SantanderTransaction date desc amount balance
-- parseAttribute :: Read a => String -> P.Parsec s u a

nonSpace :: P.Stream s m Char => P.ParsecT s u m Char
nonSpace = P.satisfy $ liftA2 (&&) isAscii  (not . isSpace)
skippable :: P.Stream s m Char => P.ParsecT s u m Char
skippable = P.satisfy $ liftA2 (||) isSpace (not . isAscii)
parseAttribute :: P.Stream s m Char => String -> P.ParsecT s u m [Char]
parseAttribute field = do
  _ <- P.string field >> P.many skippable
  v <- P.many1 (P.satisfy $ \c -> isAscii c && c /= '\n' && c /= '\r')
  _ <- P.many1 skippable
  return v
  -- case readMaybe v of
  --   Nothing -> error $ "Can't convert " ++ show v ++ " to a " ++ field
  --   Just v' -> do
  --     P.many skippable
  --     return v'
  




  


-- | We need to merge transaction comming from the official statements (HSBCTransactions)
-- and from the dailys statements Transaction.
-- The problem is to remove duplicates between the files.
-- Using `nub` doesn't work as it will remove duplicates within the same file.
-- This happens when a customer makes two payments of the same amount the same day.
-- Instead filter by date, if statement 1 cover the 10th to 13th
-- and statment 2 covers  from 12th to 14th. We will only use the transaction from 10 to 11
-- from statement 1. (We then start from latest statement to the oldest). However
-- official statement works the other way.
--
mergeTrans :: [HSBCTransactions] -> [[HSBCDaily]] -> [HSBCTransactions]
mergeTrans [] dailyss = concat $ map (map dailyToHTrans) dailyss 
mergeTrans transs dailyss = let
  lastHDate = maximum (fmap _hDate transs)
  -- list of pair, statement and it starting date
  datedStatement :: [([HSBCDaily], Maybe Day)]
  datedStatement = zip dailyss (map (\vs -> Just (minimum (fmap _hsDate vs))) dailyss)

  orderedStatements = (sortBy (comparing snd) datedStatement)

  filteredStatements :: [[HSBCDaily]]
  filteredStatements =
    zipWith (\(ss,_) (_,d) -> filter (\s -> filterStatement s d
                                              && _hsDate s > lastHDate
                                       )
                              ss
                              )
            orderedStatements
            (tail orderedStatements ++ [(error "Shoudn't be evaluated", Nothing)])
  filterStatement :: HSBCDaily -> Maybe Day -> Bool
  filterStatement _ Nothing = True
  filterStatement s (Just d) = _hsDate s < d


  filteredTransaction = map (map dailyToHTrans) filteredStatements
  -- in V.concat filteredTransaction
  in concat (transs:filteredTransaction)

-- ** Read functions


readCsv :: FromNamedRecord (Int -> r) => Maybe String -> String -> IO ([r])
readCsv discardPat path =  do
    records <-  readCsv' discardPat path
    case records of
        Left s -> error $ "can't parse file:" ++ path ++ "\n"  ++ s
        Right v -> return v

readCsv' :: FromNamedRecord (Int -> r) => Maybe String -> String -> IO (Either String [r])
readCsv' discardPat path = do
    csv' <- readWithFilter discardPat path
    let csv = stripUtf8Bom csv'
    return $ case decodeByName csv of
        Left s -> Left s
        Right (__header,v) -> Right . V.toList $ V.imap (&) v

-- | Remove Byte order Mark if necessary
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs (BL.stripPrefix "\239\187\191" bs)    

readFATransaction :: String -> IO [FATransaction]
readFATransaction = readCsv Nothing
-- |
readHSBCTransactions :: Maybe String -> String -> IO [HSBCTransactions]
readHSBCTransactions discardPatM path = do
    hs <- readCsv' discardPatM path
    ps <- readCsv' discardPatM path :: IO (Either String [PaypalTransaction])
    let phs = fmap (map (dailyToHTrans . pToS)) ps :: Either String [HSBCTransactions]
    case hs <|> phs  of
      Left s -> error $ "can't parse Transactions:" ++ path ++ "\n" ++ s
      Right v -> return v
      

readWithFilter :: Maybe String -> String -> IO BL.ByteString
readWithFilter discardPatM path = do
    csv' <- BL.readFile path
    case discardPatM of
      Nothing -> return . BL.unlines $ BL.lines csv' -- add newline at the of a file if needed
      Just discardPat -> do
        let pat :: Rg.Regex
            pat =  R.makeRegex (BL.pack discardPat)
          -- Left err -> error $ "Discard regular expression /" ++ discardPat ++ "/ is not valid :" ++ err
        return $ BL.unlines $ filter  (not . (R.matchTest pat)  ) $ BL.lines csv'
  
-- | Try to read Paypal or HSBC statements
readDaily:: Maybe String -> String -> IO [HSBCDaily]
readDaily discardPat path = do
    csv <- readWithFilter discardPat path

    let decodeHSBC = decode NoHeader csv
        decodeNewHSBC = case decodeByName csv of
          Left e -> Left e
          Right (_,v) -> Right v
        decodePaypal = case decodeByName csv of
          Left e -> Left e
          Right (_, v) -> Right $ V.map (\bf i -> pToS (bf i)) v
        decodeSantander = case P.parse parseSantanderStatement path csv of
          Left  e -> Left $ show e
          Right s -> let htranss  = map sToS (_stTrans s)
                     in Right (V.fromList htranss)
    -- it is important to start with decodeHSBC because if a file as only one line
    -- trying to parse first with a decoder expecting a header will return a empty list
    -- instead of failing: the header is actually not used (and not checked if there is no lines)
    case decodeHSBC <|> decodeNewHSBC <|> decodePaypal <|> decodeSantander of
        Left s -> error $ "can't parse Statement:" ++ path ++ "\n" ++ s
        Right v -> return . V.toList $ V.imap (\i f -> f (-i)) v -- Statements appears with
        -- the newest transaction on top, ie by descending date.
        -- transactions needs therefore to be numered in reverse order
-- * Encode Function
encodeHSBCTransactions :: [HSBCTransactions] -> BL.ByteString
encodeHSBCTransactions hs = encodeDefaultOrderedByName hs
  

-- * Main functions
--  | Group transaction of different source by amounts.
reconciliate :: [HSBCTransactions] -> [FATransaction] -> Map Amount (These [HSBCTransactions] [FATransaction])
reconciliate hsbcs fas = fmap cleanThese $ align groupH groupF where
    groupH = groupBy _hAmount hsbcs
    groupF = groupBy _fAmount fas

cleanThese :: These [a] [b] -> These [a] [b]
cleanThese (These [] bs) = That bs
cleanThese (These as []) = This as
cleanThese th = th

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy k as = Map.fromListWith (++) as' where
    as' = map ((,) <$> k <*>  (:[])) as


-- | How to aggregates group of transactions corresponding to a problem.
-- ALL keep them all, whereas TAIL tries to only keep the last unmatched ones.
-- BEST try to find the best unmatched, ie the one which maximize the distance between dates
-- ALL_BEST is keep everything but pair things using the BEST algorithms
data AggregateMode = DEBUG | ALL | TAIL | BEST | ALL_BEST  deriving (Show, Eq)

-- | Remove all matching transactions and only keep the *bad* ones.
bads :: AggregateMode ->  Map Amount (These [HSBCTransactions] [FATransaction]) -> [(Amount, These [HSBCTransactions] [FATransaction])]
bads mode0 m = let
    list = Map.toList m
    filtered = concatMap (goodM mode0) list
    sorted = sortBy (comparing (negate.abs.fst)) filtered
    goodM mode (a, t) = (,) a <$> good mode t
    good :: AggregateMode -> These [HSBCTransactions] [FATransaction] -> [(These [HSBCTransactions] [FATransaction])]
    good DEBUG t = return t
    good mode (These hs0 fs0)
        | length hs == length fs && mode /= ALL_BEST = []
        | mode == TAIL           = return $ These hs' fs'
        | mode == ALL            = return $ These hs fs
        | mode == BEST           = return $ These hs'' fs''
        | mode == ALL_BEST       = This hs'' : That fs'' : These hs''0 fs''0 : []
        where hs = sortBy (comparing (liftA2 (,) _hDate _hDayPos)) hs0
              fs = sortBy (comparing (liftA2 (,) _fDate _fPosition)) fs0
              (hs', fs') = zipTail hs fs
              (hs'', fs'', hfs''0) = best [] hs fs
              (hs''0, fs''0) = unzip hfs''0 -- matched pair
    good __mode t  = return t
    in sorted

-- | keep the tail left after a zip
-- [1,2,3] [a,b] -> ([3], [])
zipTail :: [a] -> [b] -> ([a], [b])
zipTail [] ys = ([], ys)
zipTail xs [] = (xs, [])
zipTail (_:xs) (_:ys) = zipTail xs ys

distance :: HSBCTransactions -> FATransaction -> Int
distance h f = abs .fromInteger $ diffDays (_hDate h) (_fDate f)

-- | Like zipTail but try to match each transaction by date.
-- To do so, we exclude the one with the greatest weight
-- .i.e, the sum of distance to (by date)  to all
-- also returns the matching pairs
best :: [(HSBCTransactions, FATransaction)] ->[HSBCTransactions] -> [FATransaction]
     -> ( [HSBCTransactions] --
        , [FATransaction] --
        , [(HSBCTransactions, FATransaction)] -- matching pairs
        )
best hfs [] fs = ([],fs, hfs)
best hfs hs [] = (hs,[], hfs)
best hfs (h:hs) (f:fs) | distance h f == 0 =  best ((h,f):hfs) hs fs -- not necessary but too speed up
best hfs hs fs = if minDistance < 31
                then best ((hs !! hi1,fs !! fi1):hfs) (deleteAt hi1 hs) (deleteAt fi1 fs)
                else (hs, fs, hfs) where
    ((hi1,fi1),minDistance) = minimumBy (comparing snd) pairs
    pairs = [((hi, fi), distance h f) | (h, hi) <- hs `zip` [0..], (f,fi) <- fs `zip` [0..]]
    deleteAt i xs =  head' ++ (tail tail')
        where (head', tail') = splitAt i xs

-- * Output

-- | Summary, for the output. Sort of Either HSBCTransactions FATransaction
-- but exportable to CSV
data TSource = HSBC | FA | Statement deriving (Show, Read, Eq)
instance ToField TSource where
    toField f = toField (show f)

data Transaction = Transaction
    { _sAmount :: !Amount
    , _sSource :: TSource -- ^ FA or bank 
    , _sDate :: !Day
    , _sType :: !String -- ^
    , _sDescription :: !String
    , _sNumber :: !(Maybe Int) -- ^ id in FA
    , _sObject :: !(Maybe String) -- ^ Customer or Supplier in FA
    , _sDayPos :: !Int -- ^ Used to sort transaction 
    , _sRecDate :: !(Maybe Day)
    , _sBalance :: !(Maybe (ValidField Amount)) -- ^ Balance if provided
    } deriving (Show, Read, Generic)


makeClassy ''Transaction
-- instance ToNamedRecord Transaction
instance DefaultOrdered Transaction
hsbcTransToTransaction :: HSBCTransactions -> Transaction
hsbcTransToTransaction h = pure Transaction
        <*> _hAmount
        <*> pure HSBC
        <*> _hDate
        <*> _hType
        <*> _hDescription
        <*> pure Nothing
        <*> pure Nothing
        <*> _hDayPos
        <*> pure Nothing
        <*> _hBalance
        $ h

-- ** Converters between input transactions and summaries.b
faTransToTransaction :: FATransaction -> Transaction
faTransToTransaction f = pure Transaction
                <*> _fAmount
                <*> pure FA
                <*> _fDate
                <*> _fType
                <*> _fReference
                <*> fmap Just _fNumber
                <*> fmap Just _fObject
                <*> _fPosition
                <*> _fRecDate
                <*> (Just . Provided <$>_fBalance)
                $ f

transactionToHsbcTrans :: Transaction -> HSBCTransactions
transactionToHsbcTrans = pure HSBCTransactions
  <*> _sDate
  <*> _sType
  <*> (fromMaybe "" <$> _sObject)
  <*> _sAmount
  <*> _sBalance
  <*> _sDayPos

buildTransactions :: These [HSBCTransactions] [FATransaction] -> [Transaction]
buildTransactions  (This hs) = map hsbcTransToTransaction hs
buildTransactions  (That fs) = map faTransToTransaction fs
buildTransactions  (These hs fs) = let
    shs = map hsbcTransToTransaction hs
    fhs = map faTransToTransaction fs
    in sortOn (liftA2 (,) _sDate _sDayPos) (shs ++ fhs)


fillBalance :: [Transaction] -> [Transaction]
fillBalance transs = let
  sorted = sortOn (liftA2 (,) _sDate _sDayPos) transs
  in flip evalState Nothing (mapM updateBalanceS sorted)

updateBalanceS :: Transaction -> State (Maybe Amount) Transaction
updateBalanceS trans = do
  prevBalanceM <- get
  case (_sBalance trans, prevBalanceM) of
    (Nothing, Just prevBalance) -> do
      let newBalance = prevBalance + _sAmount trans 
      put $ Just newBalance
      return $ trans {_sBalance = Just (Guessed newBalance )}
    (balance , _ ) -> do
      put $ fmap validValue balance
      return trans

fillHSBCBalance :: [HSBCTransactions] -> [HSBCTransactions]
fillHSBCBalance transs = let
  sorted = sortOn (liftA2 (,) _hDate _hDayPos) transs
  go prevBalanceM trans =
    case (_hBalance trans, prevBalanceM) of
          (Nothing, Just prevBalance) -> let newBalance = prevBalance + _hAmount trans
                                         in (Just newBalance, trans {_hBalance = Just (Guessed newBalance )})
          (balance , _ ) -> (fmap validValue balance, trans)
  in snd $ mapAccumL go Nothing sorted
-- * Main
-- ** Options
data Options = Options
    { statementFiles :: !(String) -- ^ pattern of files containing HSBC full statements.
    , dailyFiles :: !(String) -- ^ pattern of files containig HSBC recent transactions. (not a statment but called statement.csv)
    , output :: !(String)
    , startDate :: !(Maybe Day)
    , endDate :: !(Maybe Day)
    , faCredential :: !SQL.ConnectInfo -- ^ file to use to read credential  to connect to FA database
    , faMode :: !(FaMode) -- ^  read file or connect to FA database.
    , aggregateMode :: !(AggregateMode) -- ^ display all or just discrepencies.
    , initialBalance :: !(Maybe Decimal) -- ^ balance to use instead of correct one. Can make reconcilation easier
    , discardFilter :: !(Maybe String)
    } deriving (Show)

data FaMode = BankAccountId Int deriving (Show, Read, Eq)

-- ** Main body
readFa :: Options -> IO [FATransaction]
readFa opt  = case faMode opt of 
    BankAccountId i -> do 
                fetchFA (initialBalance opt) (faCredential opt) i <$> startDate <*> endDate $ opt
-- main :: Options -> IO [(Amount, These [HSBCTransactions] [FATransaction])]
main' :: Options -> IO ([Transaction], [Transaction])
main' opt = do
  (badTrans, hss) <- loadAllTrans opt
  let   summaries = concatMap buildTransactions  (map snd badTrans)

        filtered = (filterDate _sDate _sDate opt) summaries

  return (filtered, map hsbcTransToTransaction hss)

filterDate :: (a -> Day) -> (a -> Day) -> Options -> [a] -> [a]
filterDate a_sDate eDate opt = reduceWith appEndo $ catMaybes
                [ startDate opt <&> \d -> filter ((>=d). a_sDate)
                , endDate opt <&> \d -> filter ((<=d). eDate)
                ]

loadAllTrans :: Options -> IO ([(Amount, These [HSBCTransactions] [FATransaction])], [HSBCTransactions])
loadAllTrans opt = do
    hs <- (mapM (readHSBCTransactions $ discardFilter opt) =<< glob (statementFiles opt))
    sss <- mapM (readDaily $ discardFilter opt) =<<  glob (dailyFiles opt)
    fas <- readFa opt
    let hss = fillHSBCBalance $ concat hs `mergeTrans` sss 
        ths = reconciliate hss fas
    return $ (bads (aggregateMode opt) ths, hss)
  
-- | Read the latest update of the files involved in the
-- bank reconciliation
updateTime :: Options -> IO (Maybe UTCTime)
updateTime opt = do
  let go patFn = do
        paths <- glob (patFn opt)
        mapM getModificationTime paths
        
  timess <- mapM go [statementFiles, dailyFiles]
  case concat timess of
    [] -> return Nothing
    times -> return . Just $ maximum times 


-- | finishes the zip work by get a list of possible pair
rezip :: These [HSBCTransactions] [FATransaction] -> [These HSBCTransactions FATransaction]
rezip = rezip' . cleanThese
rezip' :: These [a] [b] -> [These a b]
rezip'  (This hs) = map This hs
rezip'  (That fs) = map That fs
rezip' (These hs fs) = align hs fs

-- | Regroup bads by matching pair. we lose the amount grouping
-- but we are here trying to display match, not discrepencies
badsByDay :: [(Amount, These [HSBCTransactions] [FATransaction])] -> [These HSBCTransactions  FATransaction]
badsByDay a'hts = let
  hts = map snd a'hts
  toSort = concatMap rezip hts
  getDay = these _hDate _fDate (const . _hDate)
  in sortBy (comparing getDay) toSort
  


thisFirst :: These Transaction Transaction -> Transaction
thisFirst (This a) = a
thisFirst (That a) = a
thisFirst (These a b) = a { _sNumber = _sNumber b, _sObject = _sObject b}
thatFirst :: These Transaction Transaction -> Transaction
thatFirst (This a) = a
thatFirst (That a) = a
thatFirst (These __a b) = b { _sNumber = _sNumber b, _sObject = _sObject b}

  
