module Import.NoFoundation
    ( module Import
    , setWarning
    , setError 
    , setInfo
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
import Settings as Import
import Settings.StaticFiles as Import
import SharedTypes as Import
import Yesod.Auth as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Default.Config2 as Import


import Text.Printf(printf)

data MessageType = MError | MWarning | MInfo | MSuccess deriving (Eq, Read, Show)

setError = setMessage . formatError
setWarning = setMessage . formatWarning
setInfo = setMessage . formatInfo
setSuccess = setMessage . formatSuccess

formatError = formatMessage MError
formatWarning = formatMessage MWarning 
formatInfo = formatMessage MInfo
formatSuccess = formatMessage MSuccess

formatMessage mtype t = 
  let (class_, icon) = case  mtype of
        MError  ->  ("danger", "exclamation-sign") :: (Text, Text)
        MWarning -> ("warning", "warning-sign")
        MInfo ->  ("info", "info-sign")
        MSuccess -> ("success", "ok-sign")

            
  in [shamlet|
<div.alert class="alert-#{class_}">
  <span.glyphicon class="glyphicon-#{icon}"><nbsp>
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
