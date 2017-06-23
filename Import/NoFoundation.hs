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
    , formatAmount
    , formatDouble
    , formatQuantity
    , showTransType
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


import Text.Printf(printf)

data MessageType = MError | MWarning | MInfo | MSuccess deriving (Eq, Read, Show)

setError = addMessage "Error" . formatError
setWarning = addMessage "Warning" . formatWarning
setInfo = addMessage "Info" . formatInfo
setSuccess = addMessage "Success" . formatSuccess

formatError = formatMessage MError
formatWarning = formatMessage MWarning 
formatInfo = formatMessage MInfo
formatSuccess = formatMessage MSuccess

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
(<$$>) = fmap . fmap
(<$$$>) = fmap . fmap . fmap
(<$$$$>) = fmap . fmap . fmap . fmap

-- * Format
-- formatAmount :: Amount -> Text
formatAmount = (\t -> t :: String) .  printf "" . (\x -> x :: Double) .  fromRational
formatDouble = (\t -> t :: String) .  printf "%0.2f"
formatQuantity = strip0 . (\t -> t :: String) .  printf "%0.2f" where
  strip0 s = fromMaybe s (stripSuffix ".00" s)

-- * FA utilit
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
