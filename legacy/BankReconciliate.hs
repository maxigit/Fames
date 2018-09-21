{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- * Overview
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

import Lens.Micro
import Lens.Micro.TH
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.These
import Data.Align(align)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text (strip)

import System.FilePath.Glob (glob)
import Data.Decimal
import Data.List (sortBy, minimumBy, maximumBy, foldl', mapAccumL, nub, dropWhileEnd)
import Data.Ord (comparing)
import Data.String
import Data.Time(Day, parseTimeM, formatTime, diffDays, addDays)
import Data.Time.Format(defaultTimeLocale)
import Data.Char(isSpace, isAscii)

import GHC.Generics

import Data.Monoid
import Data.Generator(reduceWith)

import qualified Database.MySQL.Simple as SQL
import qualified Database.MySQL.Simple.QueryResults as SQL
import Prelude hiding(read)
import Text.Read(readMaybe)
import Debug.Trace
import qualified Text.Parsec as P
-- import qualified Text.Parsec.Char as P

read s = case readMaybe s of
  Nothing -> error $ "can't read ["   ++ s ++ "]"
  Just r -> r
-- * Inputs
-- ** Field converters

type Amount = Decimal

instance {-# OVERLAPPING #-} FromField (Maybe Decimal) where
    parseField f = do
        t <- parseField f
        case strip t of
            "" -> return Nothing
            s  -> Just <$> parseField f

instance FromField Decimal where
    parseField f = do
        s <- parseField f
        let _type = s :: String
        return (read s)


instance ToField Decimal where
    toField d = fromString (show d)

instance ToField Day where
    toField d = fromString $ formatTime defaultTimeLocale "%d %b %Y" d


readTime :: [String ]-> String -> Day
readTime formats s = either error id (readTimeE formats s)
readTimeE :: [String ]-> String -> Either String Day
readTimeE formats str= traceShow ("E", str, r) r where
 r = case concat $ map (parseTimeM True defaultTimeLocale) formats <*> [str] of
  [] -> Left $ "can't parse time : " ++ str
  [date] -> Right date
  _ -> Left $ "ambiguous parsing for time : " ++ str

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
    parseNamedRecord r = pure FATransaction
                <*> r .: "Type"
                <*> r .: "#"
                <*> r .: "Reference"
                <*> (readTime ["%Y/%m/%d"] <$>  r .: "Date")
                <*> r .: "Person/Item"
                <*> parseDebit "Debit" "Credit" r
                <*> r .: "Balance"
                <*> pure Nothing

-- | Read from DB
fetchFA :: SQL.ConnectInfo -> Int -> Maybe Day -> Maybe Day -> IO (Vector FATransaction)
fetchFA cinfo bankAccount startM endM = do
    conn <- SQL.connect cinfo
    (_, results) <- mapAccumL tupleToFATransaction (0,1) <$> SQL.query_ conn (fromString q)
    return $ V.fromList results

    where q0 = "SELECT type, trans_no, ref, trans_date, CAST(person_id as CHAR(100)), amount, reconciled"
                         ++ " FROM 0_bank_trans"
                         ++ " WHERE amount <> 0 AND bank_act = "  ++ show bankAccount
          q = reduceWith appEndo ws q0 ++ " ORDER BY trans_date, id"

          ws = catMaybes
                   [ startM <&> \start q -> q ++ " AND trans_date >= '"
                                        ++ format (addDays (-3) start) 
                                        ++ "'"
                   , endM   <&> \end   q -> q ++ " AND trans_date <= '"
                                        ++ format (addDays 7 end)
                                        ++ "'"
                   ]
          format d = formatTime defaultTimeLocale "%F" d

          tupleToFATransaction :: (Amount, Int) ->  (Int, Int, String, Day, String, Double, Maybe Day) -> ((Amount, Int), FATransaction)
          tupleToFATransaction (bal,pos) (t,n,r,d,o,a,rd) = ((bal',pos+1), FATransaction (show t) n r d o a' bal' rd pos)
            where bal' = bal+a'
                  a' = roundTo 2 $ read . show $ a

-- | HSBC Transaction
data HSBCTransactions = HSBCTransactions
    { _hDate :: !Day
    , _hType :: !String
    , _hDescription ::  !String
    , _hAmount :: !Amount
    , _hBalance :: !String
    , _hDayPos :: !Int -- ^ position unique within a day, but doesn't have to start a one.
    } deriving (Show, Read, Eq, Ord)

makeClassy ''HSBCTransactions

instance FromNamedRecord (Int -> HSBCTransactions) where
    parseNamedRecord r = pure HSBCTransactions
      <*> (readTime ["%e-%b-%y", "%e %b %0Y"] <$> r .: "Date")
         <*> r .: "Type"
         <*> r .: "Description"
         <*> parseDebit "Paid in" "Paid out" r -- opposite to HSBC
         <*> r .: "Balance"

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
    parseRecord r = HSBCDaily <$> (readTime ["%e/%m/%Y"] <$> r .! 0)
                               <*> r .! 1
                               <*> r .! 2

sToH :: HSBCDaily -> HSBCTransactions
sToH = HSBCTransactions <$> _hsDate
              <*> pure "Statement"
              <*> _hsDescription
              <*> _hsAmount
              <*> pure ""
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
  parseNamedRecord r = pure PaypalTransaction
                <*> (readTime ["%e/%m/%Y"] <$> r .: "Date")
                <*> r .: " Name"
                <*> r .: " Type"
                -- <*> r .: "Status"
                <*> r .: " Currency"
                <*> r .: " Net" -- we want the net not the gross , as the gross include fees

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

-- parseSStatment :: P.Parsec s u SantanderStatement
parseSantanderStatement = do
  P.spaces
  -- date
  from <- either fail return . (readTimeE ["%e/%m/%Y"]) =<< parseAttribute "From:"
  to <- either fail return . (readTimeE ["%e/%m/%Y"]) =<< parseAttribute "to"
  -- account
  account <- parseAttribute "Account:"
  -- transactions
  trans <- many parseSantanderTransaction
  return $ SantanderStatement from to account trans

parseSantanderTransaction = do
  P.spaces
  date <- either fail return . (readTimeE ["%e/%m/%Y"]) =<< parseAttribute "Date:"
  desc <- fmap (dropWhile isSpace . dropWhileEnd isSpace ) (parseAttribute "Description:")
  amount <-  maybe (fail "Can't read amount") (return . normalizeDecimal) . readMaybe =<< parseAttribute "Amount:"
  balance <- maybe (fail "Can't read amount") (return . normalizeDecimal) . readMaybe =<< parseAttribute "Balance:"
  return $ SantanderTransaction date desc amount balance
-- parseAttribute :: Read a => String -> P.Parsec s u a

nonSpace = P.satisfy $ liftA2 (&&) isAscii  (not . isSpace)
skippable = P.satisfy $ liftA2 (||) isSpace (not . isAscii)
-- parseAttribute :: String -> P.Parsec s u String
parseAttribute field = do
  P.string field >> P.many skippable
  v <- P.many1 (P.satisfy $ \c -> isAscii c && c /= '\n' && c /= '\r')
  P.many1 skippable
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
mergeTrans :: Vector HSBCTransactions -> [Vector HSBCDaily] -> Vector HSBCTransactions
mergeTrans hs sss | V.null hs = V.concat $ map (V.map sToH) sss 
mergeTrans hs sss = let
  lastHDate = V.maximum (fmap _hDate hs)
  -- list of pair, statement and it starting date
  datedStatement :: [(Vector HSBCDaily, Maybe Day)]
  datedStatement = zip sss (map (\vs -> Just (V.minimum (fmap _hsDate vs))) sss)

  orderedStatements = (sortBy (comparing snd) datedStatement)

  filteredStatements :: [Vector HSBCDaily]
  filteredStatements =
    zipWith (\(ss,_) (_,d) -> V.filter (\s -> filterStatement s d
                                              && _hsDate s > lastHDate
                                       )
                              ss
                              )
            orderedStatements
            (tail orderedStatements ++ [(error "Shoudn't be evaluated", Nothing)])
  filterStatement :: HSBCDaily -> Maybe Day -> Bool
  filterStatement s Nothing = True
  filterStatement s (Just d) = _hsDate s < d


  filteredTransaction = map (V.map sToH) filteredStatements
  -- in V.concat filteredTransaction
  in V.concat (hs:filteredTransaction)

-- ** Read functions


readCsv :: FromNamedRecord (Int -> r) => String -> IO (Vector r)
readCsv path =  do
    records <-  readCsv' path
    case records of
        Left s -> error $ "can't parse file:" ++ path ++ "\n"  ++ s
        Right v -> return v

readCsv' :: FromNamedRecord (Int -> r) => String -> IO (Either String (Vector r))
readCsv' path = do
    csv' <- BL.readFile path
    let csv = stripUtf8Bom csv'
    return $ case decodeByName csv of
        Left s -> Left s
        Right (h,v) -> Right $ V.imap (&) v

-- | Remove Byte order Mark if necessary
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs (BL.stripPrefix "\239\187\191" bs)    

readFATransaction :: String -> IO (Vector FATransaction)
readFATransaction = readCsv
-- |
readHSBCTransactions :: String -> IO (Vector HSBCTransactions)
readHSBCTransactions path = do
    hs <- readCsv' path
    ps <- readCsv' path :: IO (Either String (Vector PaypalTransaction))
    let phs = fmap (V.map (sToH . pToS)) ps :: Either String (Vector HSBCTransactions)
    case hs <|> phs  of
      Left s -> error $ "can't parse Transactions:" ++ path ++ "\n" ++ s
      Right v -> return v
      

-- | Try to read Paypal or HSBC statements
readDaily:: String -> IO (Vector HSBCDaily)
readDaily path = do
    csv <- BL.readFile path
    let decodeHSBC = decode NoHeader csv
        decodePaypal = case decodeByName csv of
          Left e -> Left e
          Right (_, v) -> Right $ V.map (\bf i -> pToS (bf i)) v
        decodeSantander = case P.parse parseSantanderStatement path csv of
          Left  e -> Left $ show e
          Right s -> let htranss  = map sToS (_stTrans s)
                     in Right (V.fromList htranss)
    case decodePaypal <|> decodeHSBC <|> decodeSantander of
        Left s -> error $ "can't parse Statement:" ++ path ++ "\n" ++ s
        Right v -> return $ V.imap (\i f -> f (-i)) v -- Statements appears with
        -- the newest transaction on top, ie by descending date.
        -- transactions needs therefore to be numered in reverse order

-- * Main functions
--  | Group transaction of different source by amounts.
-- reconciliate :: Vector HSBCTransactions -> Vector FATransaction -> Map Amount (These [HSBCTransactions] [FATransaction])
reconciliate hsbcs fas = align groupH groupF where
    groupH = groupBy _hAmount (V.toList hsbcs)
    groupF = groupBy _fAmount (V.toList fas)

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
bads mode m = let
    list = Map.toList m
    filtered = concatMap (goodM mode) list
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
    good mode t  = return t
    in sorted

-- | keep the tail left after a zip
-- [1,2,3] [a,b] -> ([3], [])
zipTail [] ys = ([], ys)
zipTail xs [] = (xs, [])
zipTail (x:xs) (y:ys) = zipTail xs ys

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
                then best ((hs !! hi,fs !! fi):hfs) (deleteAt hi hs) (deleteAt fi fs)
                else (hs, fs, hfs) where
    ((hi,fi),minDistance) = minimumBy (comparing snd) pairs
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
    , _sSource :: TSource
    , _sDate :: !Day
    , _sType :: !String
    , _sDescription :: !String
    , _sNumber :: !(Maybe Int)
    , _sObject :: !(Maybe String)
    , _sDayPos :: !Int
    , _sRecDate :: !(Maybe Day)
    } deriving (Show, Read, Generic)


instance ToNamedRecord Transaction
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
                $ f

buildTransactions :: These [HSBCTransactions] [FATransaction] -> [Transaction]
buildTransactions  (This hs) = map hsbcTransToTransaction hs
buildTransactions  (That fs) = map faTransToTransaction fs
buildTransactions  (These hs fs) = let
    shs = map hsbcTransToTransaction hs
    fhs = map faTransToTransaction fs
    in sortBy (comparing _sDate) (shs ++ fhs)

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
    } deriving (Show)

data FaMode = BankAccountId Int deriving (Show, Read, Eq)

-- ** Main body
readFa :: Options -> IO (Vector FATransaction)
readFa opt  = case faMode opt of 
    BankAccountId i -> do 
                fetchFA (faCredential opt) i <$> startDate <*> endDate $ opt
-- main :: Options -> IO [(Amount, These [HSBCTransactions] [FATransaction])]
main' :: Options -> IO ([Transaction], [Transaction])
main' opt = do
  (badTrans, hss) <- loadAllTrans opt
  let   summaries = concatMap buildTransactions  (map snd badTrans)

        filtered = (filterDate _sDate _sDate opt) summaries

  return (filtered, map hsbcTransToTransaction hss)

filterDate sDate eDate opt = reduceWith appEndo $ catMaybes
                [ startDate opt <&> \d -> filter ((>=d). sDate)
                , endDate opt <&> \d -> filter ((<=d). eDate)
                ]

loadAllTrans :: Options -> IO ([(Amount, These [HSBCTransactions] [FATransaction])], [HSBCTransactions])
loadAllTrans opt = do
    hs <- V.concat <$> (mapM readHSBCTransactions =<< glob (statementFiles opt))
    sss <- mapM readDaily =<<  glob (dailyFiles opt)
    fas <- readFa opt
    let hss = hs `mergeTrans` sss 
        ths = reconciliate hss fas
    return $ (bads (aggregateMode opt) ths, V.toList hss)
  
-- | finishes the zip work by get a list of possible pair
rezip :: These [HSBCTransactions] [FATransaction] -> [These HSBCTransactions FATransaction]
rezip  (This hs) = map This hs
rezip  (That fs) = map That fs
rezip (These hs fs) = align hs fs

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
thatFirst (These a b) = b { _sNumber = _sNumber b, _sObject = _sObject b}

  
