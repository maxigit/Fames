{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, StandaloneDeriving #-}
module Items.SOP where

import Prelude
import Data.Functor.Identity
import Generics.SOP hiding (Record)
import Generics.SOP.Constraint
import GHC.Exts (Constraint)
import qualified GHC.Generics as GHC

-- * fromNPI / toNPI

-- This is a version of fromNPI / toNPI that is a bit better behaved
-- wrt type inference compared to the type-level Map-based version.
--
-- This should really go into a library. The primary reason why I haven't
-- done it yet is because I don't like the name AllZip. But All2 is already
-- taken by generics-sop itself.

-- | Type family that ensures that the given parameterized constraint is
-- satisfied pointwise by the elements of the two type-level lists.
--
type family AllZipF (f :: a -> b -> Constraint) (xs :: [a]) (ys :: [b]) :: Constraint where
  AllZipF f '[] '[]             = ()
  AllZipF f (x ': xs) (y ': ys) = (f x y, AllZip f xs ys)

-- | Type family that ensures that the second list has the same shape as
-- the first.
--
type family SameShapeAs (xs :: [a]) (ys :: [b]) :: Constraint where
  SameShapeAs '[]       ys = (ys ~ '[])
  SameShapeAs (x ': xs) ys = (ys ~ (Head ys ': Tail ys), SameShapeAs xs (Tail ys))

type family Head (xs :: [a]) :: a where
  Head (x ': xs) = x

type family Tail (xs :: [a]) :: [a] where
  Tail (x ': xs) = xs

-- | In this class, we ensure not only that the given parameterized constraint
-- is satisfied pointwise for the elements of the type-level lists, but also that
-- the two lists have the same shape. This is good for inference. If we know more
-- about one list than about the other, we obtain suitable constraints that
-- determine the shape of the lesser known list.
--
class
  (SameShapeAs xs ys, SameShapeAs ys xs, AllZipF f xs ys)
  => AllZip (f :: a -> b -> Constraint) (xs :: [a]) (ys :: [b])

instance
  (SameShapeAs xs ys, SameShapeAs ys xs, AllZipF f xs ys)
  => AllZip f xs ys

-- | Transform an NP into a related NP if their elements are suitably related.
transNP :: AllZip c xs ys => Proxy c -> (forall x y . c x y => f x -> g y) -> NP f xs -> NP g ys
transNP _ t Nil       = Nil
transNP p t (x :* xs) = t x :* transNP p t xs

-- | Transform an NS into a related NS if their elements are suitably related.
transNS :: AllZip c xs ys => Proxy c -> (forall x y . c x y => f x -> g y) -> NS f xs -> NS g ys
transNS _ t (Z x)     = Z (t x)
transNS p t (S y)     = S (transNS p t y)

class (f x ~ y) => AppEqualL f x y
instance (f x ~ y) => AppEqualL f x y

class (x ~ f y) => AppEqualR f x y
instance (x ~ f y) => AppEqualR f x y

toNPI :: forall f xs ys . AllZip (AppEqualL f) xs ys => NP f xs -> NP I ys
toNPI = transNP (Proxy :: Proxy (AppEqualL f)) I

fromNPI :: forall f xs ys . AllZip (AppEqualR f) ys xs => NP I ys -> NP f xs
fromNPI = transNP (Proxy :: Proxy (AppEqualR f)) unI

-- * Conversion of record types where all fields are parameterized over a type constructor

-- | Convert from a parameterized record type to a generic representation.
from' ::
  ( Generic (r f)               -- the applied record type must be generic
  , Code (r f) ~ '[ xs ]        -- and can have only a single construcotr
  , AllZip (AppEqualR f) xs ys  -- then we can extract f into the parameter of NP
  ) => r f -> NP f ys
from' = fromNPI . unZ . unSOP . from

-- | Convert from a generic representation to a paramterized record type.
to' ::
   ( Generic (r f)
   , Code (r f) ~ '[ xs ]
   , AllZip (AppEqualL f) ys xs
   ) => NP f ys -> r f
to' = to . SOP . Z . toNPI

-- | Compute a diff of two records.
diffRecord ::
  forall r xs ys zs .
  ( Generic (r I), Generic (r Maybe)
  , Code (r I) ~ '[ xs ], Code (r Maybe) ~ '[ ys ]
  , AllZip (AppEqualR I    ) xs zs  -- xs ~ Map I     zs
  , AllZip (AppEqualL Maybe) zs ys  -- ys ~ Map Maybe zs
  , All Eq zs
  ) => r I -> r I -> r Maybe
diffRecord r1 r2 =
  to' (diffNP (from' r1) (from' r2) :: NP Maybe zs)

diffNP :: All Eq xs => NP I xs -> NP I xs -> NP Maybe xs
diffNP =
  hczipWith (Proxy :: Proxy Eq)
    (\ (I x) (I y) -> if x == y then Nothing else Just y)

-- | Version of diffRecord that works with 'Identity' rather than 'I'.
diffRecord' ::
  forall r xs ys zs .
  ( Generic (r Identity), Generic (r Maybe)
  , Code (r Identity) ~ '[ xs ], Code (r Maybe) ~ '[ ys ]
  , AllZip (AppEqualR Identity) xs zs  -- xs ~ Map Identity zs
  , AllZip (AppEqualL Maybe   ) zs ys  -- ys ~ Map Maybe    zs
  , All Eq zs
  ) => r Identity -> r Identity -> r Maybe
diffRecord' r1 r2 =
  to' (diffNP' (from' r1) (from' r2) :: NP Maybe zs)

diffNP' :: All Eq xs => NP Identity xs -> NP Identity xs -> NP Maybe xs
diffNP' =
  hczipWith (Proxy :: Proxy Eq)
    (\ (Identity x) (Identity y) -> if x == y then Nothing else Just y)

-- * Example

data Record f = Record { x :: f Int, y :: f Int, z :: f String }
  deriving (GHC.Generic)

deriving instance (Show (f Int), Show (f String)) => Show (Record f)

instance Generic (Record f)

-- | Test on a concrete record type.
--
-- >>> test
-- Record {x = Nothing, y = Just 5, z = Nothing}
--
test :: Record Maybe
test =
  diffRecord
    (Record (I 3) (I 4) (I "foo"))
    (Record (I 3) (I 5) (I "foo"))

-- | Test on a concrete record type.
--
-- >>> test'
-- Record {x = Nothing, y = Just 5, z = Nothing}
--
test' :: Record Maybe
test' =
  diffRecord'
    (Record (Identity 3) (Identity 4) (Identity "foo"))
    (Record (Identity 3) (Identity 5) (Identity "foo"))
