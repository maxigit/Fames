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
    , (<$$>), (<$$$>), (<$$$$>)
    , (?:)
    , curry3, uncurry3
    , formatAmount
    , formatDouble
    , formatQuantity
    , formatHours
    , formatTime0
    , showTransType
    , showShortTransType
    , transactionIcon
    , transactionIconSpan
    , transNoWithLink
    , transIconWithLink
    , decodeHtmlEntities
    , groupAsMap
    , groupAscAsMap
    , groupAscWith
    , groupAsc
    , fanl, fanr
    , alignSorted
    , wordize
    , commasFixedWith
    , commasFixedWith'
    , commasFixed
    , commasFixed'
    , commasDecimal
    , mapWithKeyM
    , pattern RJust
    , pattern RNothing
    , pattern LLeft
    , pattern LRight
    , pattern RLeft
    , pattern RRight
    ) where

import ClassyPrelude.Yesod as Import
import Control.Monad.Logger as Import
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
import Formatting as F
import Data.Align(align)
import Data.Monoid(First(..))
import Data.Maybe (fromJust)
import Data.Decimal
import Data.Time.Format(FormatTime)

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

-- ** Tuples
uncurry3 :: (a -> b -> c -> r) -> (a,b,c) -> r
uncurry3 f (a,b,c) = f a b c

curry3 :: ((a,b,c) -> r) -> a -> b -> c -> r
curry3 f a b c = f (a,b,c)
-- * Format
-- formatAmount :: Amount -> Text
formatAmount :: Rational -> String
formatAmount = (\t -> t :: String) .  printf "" . (\x -> x :: Double) .  fromRational
formatDouble :: Double -> String
formatDouble = (\t -> t :: String) .  printf "%0.2f"
formatQuantity :: Double -> String
formatQuantity = strip0 . (\t -> t :: String) .  printf "%0.2f" where
  strip0 s = fromMaybe s (stripSuffix ".00" s)

formatHours :: Double -> Text
formatHours duration = let
  (h, m) = properFraction duration :: (Int, Double)
  p2 = left 2 '0' %. F.int
  in sformat (p2 % ":" % p2) h (round $ 60*m :: Int)
  
formatTime0 :: (IsString a, FormatTime t) => String -> t -> a
formatTime0 l_format time = fromString $ formatTime defaultTimeLocale l_format time

-- ** Formating lforb
commasFixedWith :: Integral a => (Double -> a) -> Int -> Format r (Double -> r)
commasFixedWith roundFn digit = later go where
  go x = let
    (n,f) = properFraction x :: (Int, Double)
    b = (commas' % "." % (left digit '0' %. int)) -- n (floor $ 100 *  abs f)
    in bprint b n (roundFn $ (10^digit) *  abs f)

-- | display a amount to 2 dec with thousands separator
commasFixed :: Format r (Double -> r)
commasFixed = commasFixedWith floor (2 :: Int)
-- | Sames as commasFixed but don't print commas if number is a whole number
commasFixed' :: Format r (Double -> r)
commasFixed' = commasFixedWith' floor (2 :: Int)

commasFixedWith' :: Integral a =>  (Double -> a) -> Int -> Format r (Double -> r)
commasFixedWith' roundFn digit = later go where
  go x = let
    (n,f) = properFraction x :: (Int, Double)
    frac =  roundFn ((10^digit) * abs f)
    fracB = if frac < 1
            then fconst mempty
            else "." % left digit '0' %. int
    b = (commas' % fracB) -- n (floor $ 100 *  abs f)
    in if x < 0 && n == 0
       then bprint ("-" % b) n frac
       else bprint b n frac

-- | Like Formatting.commas but fix bug on negative value
-- -125 - -,125

commas' :: (Integral n , Buildable n) => Format r (n -> r)
commas' = later go where
  go n = if n < 0
         then bprint ("-" % commas) (abs n)
         else bprint commas  n

-- | Display a decimal number with comma (thousand separator)
commasDecimal :: Integral a => Format r (DecimalRaw a -> r)
commasDecimal = later go where 
  go x | x < 0 = "-" <> go (-x)
  go x = let
    digit = fromIntegral $ decimalPlaces x :: Int
    (n, f) = (fromIntegral $ decimalMantissa x :: Int) `divMod` (10^digit)
    b = (commas' % "." % (left digit '0' %. int)) -- n (floor $ 100 *  abs f)
    b0 = commas'
    in case digit of
      0 -> bprint b0 n -- no decimal  -> no dot 
      _ -> bprint b n f

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

-- C customer
--  S supplier
-- Bank $
-- stock ^ = home
-- invoice T
-- 
-- =>
  
showShortTransType :: IsString t => FATransType -> t
showShortTransType ST_JOURNAL = "#=#"
showShortTransType ST_BANKPAYMENT = "$->*"
showShortTransType ST_BANKDEPOSIT = "*->$"
showShortTransType ST_BANKTRANSFER = "$=$"
showShortTransType ST_SALESINVOICE = "C->#"
showShortTransType ST_CUSTCREDIT = "C<-#"
showShortTransType ST_CUSTPAYMENT = "C->$"
showShortTransType ST_CUSTDELIVERY = "C->#"
showShortTransType ST_LOCTRANSFER = "^<>^"
showShortTransType ST_INVADJUST = "^->#"
showShortTransType ST_PURCHORDER = "S->@"
showShortTransType ST_SUPPINVOICE = "S->#"
showShortTransType ST_SUPPCREDIT = "S<-#"
showShortTransType ST_SUPPAYMENT = "$->S"
showShortTransType ST_SUPPRECEIVE = "S->^"
showShortTransType ST_WORKORDER = "W->@"
showShortTransType ST_MANUISSUE = "W->#"
showShortTransType ST_MANURECEIVE = "W->^"
showShortTransType ST_SALESORDER = "S->@"
showShortTransType ST_SALESQUOTE = "S->'"
showShortTransType ST_COSTUPDATE = "CU"
showShortTransType ST_DIMENSION = "D"

data IconType = ICustomer
              | ISupplier
              | IStock
              | IBank
              | IAny
              | IGL
              | IInLeft
              | IInRight
              | IOutLeft
              | IOutRight
              | IInOut
              | IOrder  -- loc
              deriving (Eq, Show, Ord, Enum)

-- order ICustomer -> Stock | Bank | GL | Supplier
transactionIcons :: FATransType -> [IconType]
transactionIcons eType = case eType of
  ST_JOURNAL -> [IGL, IInOut, IGL ]
  ST_BANKPAYMENT -> [ IBank, IOutRight, IAny ]
  ST_BANKDEPOSIT -> [ IBank, IInLeft, IAny ]
  ST_BANKTRANSFER -> [ IBank, IInOut, IBank ]
  ST_SALESINVOICE -> [ ICustomer, IInRight, IGL ]
  ST_CUSTCREDIT ->   [ ICustomer, IOutLeft, IGL ]
  ST_CUSTPAYMENT -> [ ICustomer, IInRight, IBank ]
  ST_CUSTDELIVERY -> [ ICustomer, IOutLeft, IStock ]
  ST_LOCTRANSFER -> [ IStock, IInOut, IStock ]
  ST_INVADJUST -> [ IStock, IInOut, IGL  ]
  ST_PURCHORDER -> [ IOrder, IOutRight, ISupplier ]
  ST_SUPPINVOICE -> [ IGL, IOutRight , ISupplier ]
  ST_SUPPCREDIT -> [ IGL, IInLeft, ISupplier ]
  ST_SUPPAYMENT -> [ IBank, IOutRight, ISupplier ]
  ST_SUPPRECEIVE -> [ IStock, IInLeft, ISupplier ]
  ST_WORKORDER -> [ IInRight, IOrder ]
  ST_MANUISSUE -> [ ]
  ST_MANURECEIVE -> [ IInRight, IStock]
  ST_SALESORDER -> [ ICustomer, IInRight,  IOrder ]
  ST_SALESQUOTE -> [ ICustomer, IInRight,  IOrder ]
  ST_COSTUPDATE -> [  ]
  ST_DIMENSION -> [  ]
  
itypeToIcon :: IconType -> Html
itypeToIcon iType = [shamlet| <span.glyphicon class="glyphicon-#{glyph} #{class_}"> |]
  where
    glyph, class_ :: Text
    (glyph, class_) = case iType of
       ICustomer -> ("user", "text-primary")
       ISupplier -> ("user", "text-info")
       IStock -> ("home", "text-muted")
       IBank -> ("stats", "text-warning")
       IAny -> ("globe", "text-info")
       IGL -> ("list", "text-muted")
       IInLeft -> ("arrow-left", "text-success")
       IInRight -> ("arrow-right", "text-success")
       IOutLeft -> ("arrow-left", "text-danger")
       IOutRight -> ("arrow-right", "text-danger")
       IInOut -> ("transfer", "")
       IOrder  -> ("shopping-cart", "text-muted")
  
transactionIcon :: FATransType -> Html
transactionIcon eType = 
  let ttype = showShortTransType eType :: Text
  in [shamlet|
       $case transactionIcons eType
         $of []
           #{ttype}
         $of icons
           $forall i <- icons
             #{itypeToIcon i}
          |]
transactionIconSpan  :: FATransType -> Html
transactionIconSpan eType =
  [shamlet|
     <span data-toggle="tooltip" title="#{longType}">
       #{transactionIcon eType}
       <span.hidden> #{longType}
  |] where
      longType = showTransType eType :: Text
  
 
transNoWithLink :: (FATransType -> Int -> Text) -> Text -> FATransType -> Int -> Html
transNoWithLink =  transNoWithLink'  (\_ transNo -> "#" <> tshow transNo)
transNoWithLink' showTrans urlForFA' class_ transType transNo = [shamlet|
 <a href="#{urlForFA' transType transNo}"
    class="#{class_}"
    target=_blank
    data-toggle="tooltip" 
    title="#{longType} ##{tshow $ transNo}"
    >#{showTrans transType transNo}
  |] where
         longType = showTransType transType :: Text

transIconWithLink :: (FATransType -> Int -> Text) -> Text -> FATransType -> Int -> Html
transIconWithLink =  transNoWithLink'  (\transType _ -> transactionIcon transType)
 
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
