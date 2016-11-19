module Import.NoFoundation
    ( module Import
    , setWarning
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

setError = setMessage      
setWarning = setMessage

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
