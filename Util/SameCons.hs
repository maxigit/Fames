-- | Provide constructor comparaison using Generics.
{-# LANGUAGE DefaultSignatures, TypeOperators #-}
module Util.SameCons where

import Prelude
import GHC.Generics

class SameCons a where
   -- | Returns true if both data have the same constructor
   sameCons :: a -> a -> Bool

   -- default sameCons :: (Generic a, GSameCons (Rep a)) => a -> a -> Bool
   default sameCons :: (Generic a, GSameCons (Rep a)) => a -> a -> Bool
   sameCons a b = gsameCons (from a) (from b)


class GSameCons f where
  gsameCons :: f a -> f a -> Bool
  

-- | Empty data types
instance GSameCons V1 where
  gsameCons _ _ = True

-- | Unit
instance GSameCons U1 where
  gsameCons _ _ = True

-- |Sums
instance (GSameCons a, GSameCons b) => GSameCons (a :+: b) where
  gsameCons (L1 x) (L1 y) = gsameCons x y
  gsameCons (R1 x) (R1 y) = gsameCons x y
  gsameCons _ _ = False

-- instance (GSameCons a, GSameCons b) => GSameCons (a :*: b) where
--   gsameCons = error "*"
instance GSameCons a => GSameCons (M1 i c a) where
  gsameCons (M1 x) (M1 y) = gsameCons x y
instance GSameCons (K1 i a ) where
  -- We are only interested in the constructor
  -- if we arrive at this level, the constructor should be correct
  gsameCons (K1 _) (K1 _) = True 
   

