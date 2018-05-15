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
infixr 1 ?:
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
decodeHtmlEntities s = TS.fromTagText $ headEx $ TS.parseTags s
  
-- * Util
groupAsMap :: (Semigroup a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map k a
groupAsMap key f xs = Map.fromListWith (<>) [(key x, f x ) | x <- xs]
