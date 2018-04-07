module Locker
(Locker
, lock
, unlock
, Granted
)

where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import qualified Data.Set as Set

-- * Types

-- | The locker Monad. A value which can't be accessed without a key
data Locker r a = Locker r a

data Granted = Granted | Forbidden

-- * Manipulators
lock :: r -> a -> Locker r a
lock r x = Locker r x

unlock :: (r -> Granted) ->  Locker r a -> Either r a
unlock unlocker (Locker role value) = case unlocker role of
  Forbidden -> Left role
  Granted -> Right value
  
restrict :: Semigroup r => r Locker r a -> Locker r a
restrict r' (Locker r x) = Locker (r <> r') x


-- * Instances

instance Functor (Locker r) where
  fmap f (Locker r x)  = Locker r (f x)

instance Monoid r => Applicative (Locker r) where
  pure x = Locker mempty x
  Locker rf f <*> Locker rx x = Locker (rf `mappend` rx) (f x)
  
instance Monoid r => Monad (Locker r) where
  Locker r x >>= f = let (Locker r' x') = f x in Locker (r `mappend` r') x'
  
  
