-- | Generate a tree of all sensible values 
-- For example only use Nothing when expanding Maybe
-- Used to generate all possible routes for a Yesod website
-- It is really similar to the GEnum for the DeriveGenerics package.
-- However, we generate a tree instead of a flat list
-- and we need to define an instance for Maybe so it's better
-- to have our own class.
{-# LANGUAGE DefaultSignatures, TypeOperators #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Util.EnumTree where

import Prelude
import Data.Time(Day)

import GHC.Generics
import Data.Foldable (toList)

data EnumTree a = EnumTree [EnumTree a] | EnumNode a  deriving (Show, Functor, Foldable)
instance Semigroup (EnumTree a) where
  EnumTree xs <> EnumTree ys = EnumTree (xs <> ys)
  EnumTree xs <>  y = EnumTree (xs ++ [y])
  x <>  EnumTree ys = EnumTree (x : ys )
  x <>  y = EnumTree (x:[y])

instance Monoid (EnumTree a) where
  mempty = EnumTree []

-- | remove unused EnumTree []
cleanTree :: EnumTree a -> EnumTree a
cleanTree (EnumTree xs) = 
  case filter nonEmpty xs of
    [x] -> x
    xs' -> EnumTree xs'
  where
     nonEmpty (EnumTree []) = False
     nonEmpty _ = True
cleanTree t = t

class EnumTreeable a where
  enumTree :: EnumTree a
  default enumTree :: (Generic a , GEnumTreeable (Rep a)) => EnumTree a
  enumTree = cleanTree $ fmap to genumTree   -- _ genumTree

instance EnumTreeable (Maybe a) where
  enumTree = EnumNode Nothing
instance EnumTreeable () where
  enumTree = EnumTree []
instance EnumTreeable [a] where
  enumTree = EnumTree []

instance EnumTreeable Day where
  enumTree = EnumTree []

class GEnumTreeable f where
  genumTree :: EnumTree (f a)

instance GEnumTreeable V1 where
  genumTree = EnumTree []

instance GEnumTreeable U1 where
  genumTree = EnumNode U1

instance EnumTreeable c => GEnumTreeable (K1 i c) where
  genumTree =  EnumTree [fmap K1 enumTree ]

instance (GEnumTreeable f) => GEnumTreeable (M1 i c f) where
  genumTree = fmap M1 genumTree

instance (GEnumTreeable f, GEnumTreeable g)  => GEnumTreeable (f :+: g) where
  genumTree = fmap L1 genumTree <> fmap R1 genumTree

instance (GEnumTreeable f, GEnumTreeable g)  => GEnumTreeable (f :*: g) where
  genumTree = EnumTree $ do -- []
    x <- toList genumTree
    y <- toList genumTree
    return $ EnumNode $ x :*: y

  
