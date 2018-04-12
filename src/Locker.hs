module Locker
( Locker
, lock
, unlock
, restrict
, permissions
, unsafeUnlock
, Granted
, granter
, Privilege(..)
, isUnlocked
)

where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import qualified Data.Set as Set
import Role
import Debug.Trace
import Data.Either(isRight)

-- * Types

-- | The locker Monad. A value which can't be accessed without a key
data Locker r a = Locker (Set r) a deriving (Eq, Ord, Read) -- 

data Granted = Granted | Forbidden deriving (Eq, Ord, Show, Read)

-- * Manipulators
lock :: Ord r => [r] -> a -> Locker r a
lock rs x = Locker (setFromList rs) x

unlock :: (r -> Granted) ->  Locker r a -> Either [r] a
unlock unlocker (Locker roles value) = case filter ((== Forbidden). unlocker) (toList roles) of
    [] -> Right value
    missings -> Left missings

isUnlocked granter = isRight . unlock granter
  
permissions :: Locker r a -> [r]
permissions (Locker rs _)  = toList rs

restrict :: Ord r => [r] ->  Locker r a -> Locker r a
restrict rs' (Locker rs x) = Locker (rs <> setFromList rs') x

unlock' unlocker privilege (Locker roles value) = unlock unlocker $ Locker (setFromList $ map (privilege,) (toList roles)) value

-- * Unsafe
unsafeUnlock (Locker rs x) = traceShow ("Unsafe UNLOCK requiring " <> show rs) x
-- * Instances

instance Functor (Locker r) where
  fmap f (Locker r x)  = Locker r (f x)

instance Ord r => Applicative (Locker r) where
  pure x = Locker mempty x
  Locker rfs f <*> Locker rxs x = Locker (rfs <> rxs) (f x)
  
instance Ord r => Monad (Locker r) where
  Locker rs x >>= f = let (Locker rs' x') = f x in Locker (rs <> rs') x'
  
  

instance (Ord r, Num a) => Num (Locker r a) where
  (+) = liftA2 (+) 
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Ord r, Fractional a) => Fractional (Locker r a) where
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
  

-- * Roles Granter

data Privilege = ViewPriv | CreatePriv | DeletePriv | SavePriv deriving (Eq, Read, Show, Ord, Enum ,Bounded)
granter :: Role -> (Privilege, Text) -> Granted
granter Administrator _ = Granted
granter role0 (priv, r) = let
  (attribute, wreq)  = case priv of
    ViewPriv -> ("view", ReadRequest )
    SavePriv -> ("save", WriteRequest )
    DeletePriv -> ("delete", WriteRequest )
    CreatePriv -> ("create", WriteRequest )
  role = filterRole attribute role0
  in roleToGranted role wreq r

roleToGranted role wreq r = if null  (filterPermissions wreq (setFromList [r])  role)
  then Granted
  else Forbidden
