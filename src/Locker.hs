module Locker
( Locker
, lock
, unlock
, Granted
, restrict
, permissions
)

where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import qualified Data.Set as Set

-- * Types

-- | The locker Monad. A value which can't be accessed without a key
data Locker r a = Locker [r] a deriving (Eq, Ord, Read) -- 

data Granted = Granted | Forbidden deriving (Eq, Ord, Show, Read)

-- * Manipulators
lock :: [r] -> a -> Locker r a
lock rs x = Locker rs x

unlock :: (r -> Granted) ->  Locker r a -> Either [r] a
unlock unlocker (Locker roles value) = case filter ((== Forbidden). unlocker) roles of
    [] -> Right value
    missings -> Left missings
  
permissions :: Locker r a -> [r]
permissions (Locker rs _)  = rs

restrict :: [r] ->  Locker r a -> Locker r a
restrict rs' (Locker rs x) = Locker (rs <> rs') x

-- * Instances

instance Functor (Locker r) where
  fmap f (Locker r x)  = Locker r (f x)

instance Applicative (Locker r) where
  pure x = Locker [] x
  Locker rfs f <*> Locker rxs x = Locker (rfs <> rxs) (f x)
  
instance Monad (Locker r) where
  Locker rs x >>= f = let (Locker rs' x') = f x in Locker (rs <> rs') x'
  
  

instance Num a => Num (Locker r a) where
  (+) = liftA2 (+) 
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Locker r a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational


-- Show Instance which is readable if there is no permission required
-- but not otherwise
instance (Show r, Show a) => Show (Locker r a) where
  show l@(Locker rs v) = "Locker " <> show rs <> " ("
                         <> (showLock (const Forbidden) l)
                         <>  ")" 

showLock  :: (Show r, Show a) => (r -> Granted) ->  (Locker r a ) -> String
showLock  unlocker lock = case unlock unlocker lock of
  Left required -> "Requires: " <> show required
  Right value -> show value
  
