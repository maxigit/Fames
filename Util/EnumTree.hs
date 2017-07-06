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
import Data.Monoid
import GHC.Generics
import Data.Foldable (toList)

data EnumTree a = EnumTree [EnumTree a] | EnumNode a  deriving (Show, Functor, Foldable)
instance Monoid (EnumTree a) where
  mempty = EnumTree []
  EnumTree xs `mappend` EnumTree ys = EnumTree (xs <> ys)
  EnumTree xs `mappend` y = EnumTree (xs ++ [y])
  x `mappend` EnumTree ys = EnumTree (x : ys )
  x `mappend` y = EnumTree (x:[y])

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

  
