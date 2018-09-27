{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Util.ValidField where
import Prelude


-- | Allows to distinguish between a given value and a guessed one.
-- Usefull when autofilling data from a csv (using the previous row) or
-- computing a bank accounting balance
data ValidField a = Provided { validValue :: a} | Guessed { validValue :: a }
     deriving (Show, Read, Eq, Ord, Functor)
instance Applicative ValidField  where
  pure = Provided
  (Provided f) <*> (Provided v) = Provided (f v)
  f <*> v = Guessed $ (validValue f) (validValue v)

guess :: ValidField a -> ValidField a
guess v = Guessed (validValue v)

instance Foldable ValidField where
  foldr f b v= f (validValue v) b
instance Traversable ValidField where
  sequenceA (Guessed m) = Guessed <$> m
  sequenceA (Provided m) = Provided <$> m
    

  
-- deriving instance Show a => Show (ValidField a)
-- deriving instance Read a => Read (ValidField a)
-- deriving instance Eq a => Eq (ValidField a)
-- deriving instance Ord a => Eq (Ord a)
