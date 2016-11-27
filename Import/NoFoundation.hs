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
