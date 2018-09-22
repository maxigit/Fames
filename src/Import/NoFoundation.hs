{-# LANGUAGE PatternSynonyms #-}
module Import.NoFoundation
    ( module Import
    , setWarning
    , setError 
    , setInfo
    , setSuccess
    , formatWarning
    , formatError 
    , formatInfo
    , formatSuccess
    , (<|&>)
    , (<&>)
    , (<$$>), (<$$$>), (<$$$$>)
    , (?:)
    , formatAmount
    , formatDouble
    , formatQuantity
    , showTransType
    , decodeHtmlEntities
    , groupAsMap
    , groupAscAsMap
    , groupAscWith
    , groupAsc
    , fanl, fanr
    , alignSorted
    , wordize
    , commasFixed
    , commasFixed'
    , mapWithKeyM
    , pattern RJust
    , pattern RNothing
    , pattern LLeft
    , pattern LRight
    , pattern RLeft
    , pattern RRight
    ) where

import ClassyPrelude.Yesod as Import
import Data.These as Import
import Model as Import
import ModelField as Import
import Settings as Import
import Settings.StaticFiles as Import
import SharedTypes as Import
import Yesod.Auth as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Text.Blaze.Html (Markup)
import qualified Text.HTML.TagSoup as TS
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Data.Char (isUpper)
import Formatting
import Data.Align(align)
import Data.Monoid(First(..))
import Data.Maybe (fromJust)

import Text.Printf(printf)

data MessageType = MError | MWarning | MInfo | MSuccess deriving (Eq, Read, Show)

setError, setWarning, setInfo, setSuccess :: MonadHandler m => Markup -> m ()
setError = addMessage "Error" . formatError
setWarning = addMessage "Warning" . formatWarning
setInfo = addMessage "Info" . formatInfo
setSuccess = addMessage "Success" . formatSuccess

formatError, formatWarning, formatInfo, formatSuccess :: Markup -> Markup
formatError = formatMessage MError
formatWarning = formatMessage MWarning 
formatInfo = formatMessage MInfo
formatSuccess = formatMessage MSuccess

formatMessage :: MessageType -> Markup -> Markup
formatMessage mtype t = 
  let (class_, icon, status) = case  mtype of
        MError  ->  ("danger", "exclamation-sign", "Error") :: (Text, Text, Text)
        MWarning -> ("warning", "warning-sign", "Warning")
        MInfo ->  ("info", "info-sign", " Information")
        MSuccess -> ("success", "ok-sign", "Success")
  in [shamlet|
<div.alert class="alert-#{class_}">
  <div.alert-header>
    <span.glyphicon class="glyphicon-#{icon}"> #{status}
  <br>
  ^{t}
|]

infixl 3 <|&>
-- | Similar to <&> but operate on Left instead of Right
(<|&>) :: Either a b -> (a -> a') -> Either a' b
Left l <|&> f = Left (f l)
Right r <|&> _ = Right r

infixl 3 <&>
(<&>) :: Functor f => f a  -> (a ->b) -> f b
x <&> f = fmap f x

infixl 4 <$$>, <$$$>, <$$$$>
(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f1 (f a) -> f1 (f b)
(<$$>) = fmap . fmap

(<$$$>) :: (Functor f, Functor f1, Functor f2) =>
           (a -> b) -> f2 (f1 (f a)) -> f2 (f1 (f b))
(<$$$>) = fmap . fmap . fmap
(<$$$$>) :: (Functor f, Functor f1, Functor f2, Functor f3) =>
            (a -> b) -> f3 (f2 (f1 (f a))) -> f3 (f2 (f1 (f b)))
(<$$$$>) = fmap . fmap . fmap . fmap

-- | cons a Maybe
infixr 5 ?:
(?:)  :: Maybe a -> [a] -> [a]
Nothing ?: xs = xs 
(Just x) ?: xs = x:xs

-- * Format
-- formatAmount :: Amount -> Text
formatAmount :: Rational -> String
formatAmount = (\t -> t :: String) .  printf "" . (\x -> x :: Double) .  fromRational
formatDouble :: Double -> String
formatDouble = (\t -> t :: String) .  printf "%0.2f"
formatQuantity :: Double -> String
formatQuantity = strip0 . (\t -> t :: String) .  printf "%0.2f" where
  strip0 s = fromMaybe s (stripSuffix ".00" s)

-- ** Formating lib
-- | display a amount to 2 dec with thousands separator
commasFixed = later go where
  go x = let
    (n,f) = properFraction x :: (Int, Double)
    b = (commas' % "." % (left 2 '0' %. int)) -- n (floor $ 100 *  abs f)
    in bprint b n (floor $ 100 *  abs f)

-- | Sames as commasFixed but don't print commas if number is a whole number
commasFixed' = later go where
  go x = let
    (n,f) = properFraction x :: (Int, Double)
    frac =  floor (100 * abs f)
    fracB = if frac < 1
            then fconst mempty
            else "." % left 2 '0' %. int
    b = (commas' % fracB) -- n (floor $ 100 *  abs f)
    in bprint b n frac

-- | Like Formatting.commas but fix bug on negative value
-- -125 - -,125
commas' = later go where
  go n = if n < 0
         then bprint ("-" % commas) (abs n)
         else bprint commas  n
-- * FA utilit
showTransType :: IsString t => FATransType -> t
showTransType ST_JOURNAL = "Journal Entry"
showTransType ST_BANKPAYMENT = "Bank Payment"
showTransType ST_BANKDEPOSIT = "Bank Deposit"
showTransType ST_BANKTRANSFER = "Bank Transfer"
showTransType ST_SALESINVOICE = "Sales Invoice"
showTransType ST_CUSTCREDIT = "Customer Credit Note"
showTransType ST_CUSTPAYMENT = "Customer payment"
showTransType ST_CUSTDELIVERY = "Customer Delivery"
showTransType ST_LOCTRANSFER = "Location Transfer"
showTransType ST_INVADJUST = "Inventory Adjustment"
showTransType ST_PURCHORDER = "Purchase Order"
showTransType ST_SUPPINVOICE = "Supplier Invoice"
showTransType ST_SUPPCREDIT = "Supplier Credit Note"
showTransType ST_SUPPAYMENT = "Supplier Payment"
showTransType ST_SUPPRECEIVE = "Supplier receive"
showTransType ST_WORKORDER = "Work Order"
showTransType ST_MANUISSUE = "Manu Issue"
showTransType ST_MANURECEIVE = "Manu Receive"
showTransType ST_SALESORDER = "Sales Order"
showTransType ST_SALESQUOTE = "Sales Quote"
showTransType ST_COSTUPDATE = "Cost Update"
showTransType ST_DIMENSION = "Dimensions"

-- * Html 
decodeHtmlEntities :: Text -> Text
decodeHtmlEntities s = maybe s TS.fromTagText (headMay $ TS.parseTags s)
  
-- * Util
groupAsMap :: (Semigroup a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map k a
groupAsMap key f xs = Map.fromListWith (<>) [(key x, f x ) | x <- xs]


-- | Expect element to be already sorted. Can happen from the result of the SQL query
groupAscAsMap :: (Semigroup a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map k a
groupAscAsMap key f xs = Map.fromAscListWith (<>) [(key x, f x ) | x <- xs]

groupAscWith :: Ord k =>  (a -> k) -> (a -> b) -> [a] -> [(k, [b])]
groupAscWith key f xs = map (first (fromJust . getFirst) . sequence)
  $ groupBy ((==) `on` fst) (map ((First . Just .  key) &&& f) xs)
-- | don't sort element
-- groupOn' :: Eq k => (a -> k) -> f a -> [f (k, a)]
groupAsc :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupAsc key =  groupAscWith key id

-- | Fanout but only left eq `f &&& id`
fanl :: (a -> b) -> a -> (b, a)
fanl f x = (f x, x)
-- | Fanout but only right eq `id &&& f`
fanr :: (a -> b) -> a -> (a, b)
fanr f x = (x, f x)


mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM f m = do
  pairs <- mapM (\(k, v) -> traverse (f k) (k, v)) (mapToList m)
  return $ mapFromList pairs
-- * Text
-- | Transform camel case to word and remove prefix and suffix
-- use full to make menu from data type
wordize :: Maybe Text -> Maybe Text -> Text -> Text
wordize prefixM suffixM s0 = let
  withoutPrefix  x = fromMaybe x  (join $ stripPrefix <$> prefixM  <*> Just x)
  withoutSuffix x = fromMaybe x (join $ stripSuffix <$> suffixM  <*> Just x)
  titles = Split.split (Split.keepDelimsL $ Split.whenElt isUpper) (unpack . withoutSuffix $ withoutPrefix s0)
  in intercalate " " (map pack titles)

-- | Align two sorted list. Even though it could be lazy, it is probably not
-- due to the strictness of Map used under the hood.
alignSorted :: (Ord k) => [(k,a)] -> [(k,b)] -> [(k, These a b)]
alignSorted as bs = Map.toList $ align (Map.fromAscList as)  (Map.fromAscList bs)


-- * Patterns
pattern RJust :: a -> Either e (Maybe a)
pattern RJust x = Right (Just x)

pattern RNothing :: Either e (Maybe a)
pattern RNothing = Right Nothing

pattern RRight :: a -> Either e (Either e' a)
pattern RRight x = Right (Right x)

pattern RLeft :: e' -> Either e (Either e' a)
pattern RLeft x = Right (Left x)

pattern LRight :: a -> Either (Either e' a) b
pattern LRight x = Left (Right x)

pattern LLeft :: e' -> Either (Either e' a) b
pattern LLeft x = Left (Left x)
