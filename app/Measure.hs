{-# LANGUAGE StandaloneDeriving, DefaultSignatures, DerivingVia, DeriveTraversable, FunctionalDependencies, TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Measure
(module Measure
)
-- ( Amount
-- , Quantity
-- , Price
-- , Scalar
-- , Mul(..)
-- , alaDouble
-- , Measure(..)
-- )
where

import ClassyPrelude
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Sized as VGS
import GHC.TypeLits (KnownNat)
import Data.Vector.Unboxed.Deriving
import Data.Kind (Type)
-- import Data.Coerce
-- imp

newtype MeasureF (u :: Type) a  = Measure { measured :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving (Applicative, Monad) via Identity
  
instance Num a => Num (MeasureF u a ) where
   Measure x + Measure y = Measure (x + y)
   Measure x - Measure y = Measure (x - y)
   Measure _ * Measure _ = error "Can't multiply measure. use ^* instead"
   negate (Measure x) = Measure (negate x)
   abs (Measure x) = Measure (abs x)
   signum (Measure x) = Measure (signum x)
   fromInteger i = Measure (fromInteger i)

deriving instance Fractional a => Fractional (MeasureF u a )
deriving instance Real a => Real (MeasureF u a )
deriving instance RealFrac a => RealFrac (MeasureF u a )
deriving instance Floating a => Floating (MeasureF u a )
deriving instance RealFloat a => RealFloat (MeasureF u a )
deriving instance Enum a => Enum (MeasureF u a )
deriving instance Integral a => Integral (MeasureF u a )

type Measure u = MeasureF u Double
type Measures u f  = MeasureF u (f Double)
  
    
derivingUnbox "MeasureF"
   [t| forall u a. (Unbox a) => MeasureF u a -> a |]
   [| \(Measure x) -> x|]
   [| Measure |]
   
   
data AmountU
data QuantityU
data PriceU
data ScalarU
data MonthU
data YearU
data WeekU
data DayU

type Amounted = MeasureF AmountU
type Counted = MeasureF QuantityU
type Priced = MeasureF PriceU

-- type Amounts = Measures AmountU
-- type Quantities = Measures QuantityU
-- type Prices = Measures PriceU
-- type Scalars = Measures ScalarU
-- 
-- type Days = Measures DayU
-- type Weeks = Measures WeekU

infixl 7 ^* , ^/, *^
class Mul a b where
    (^*) :: a -> b -> (a :*: b)
class Div ab b where
     (^/) :: ab -> b  -> (ab :/: b)
    
    
(*^) :: Mul a b => b -> a -> a :*: b
(*^) = flip (^*)

type family (a :: Type) :*: (b :: Type)  where
   -- (MeasureF u a) :*: (MeasureF v a) = MeasureF (u :*: v) a
   (u, u') :*: (v, v') = (u :*: v , u' :*: v')
   -- VGS.Vector v n a :*: VGS.Vector v n b = VGS.Vector v n (a :*: b)
   f3 v n a :*: f3 v n b = f3 v n (a :*: b)
   f3 v n a :*: b = f3 v n (a :*: b)
   f3 v a x :*: f3 v b x = f3 v (a :*: b) x

   f2 a x :*: f2 b x = f2 (a :*: b) x
   f2 n a :*: f2 n b = f2 n (a :*: b)

   PriceU :*: QuantityU = AmountU
   QuantityU :*: PriceU = AmountU
   ScalarU :*: b = b
   a :*: ScalarU = a

   (a :/ b)  :*: b = a
   b :*: (a :/ b)  = a
   a :*: (b :* c) = a :*: b :*: c

   f a :*: b = f (a :*: b)
   a :*: f b = f (a :*: b)

   Double :*: b = Double
   a :*: Double = Double

   a :*: b = a :* b

type family (ab :: Type) :/: (b :: Type) where
   (u, u') :/: (v, v') = (u :/: v , u' :/: v')
   f2 ab x :/: f2 b x = f2 (ab :/: b) x
   (a :* b) :/: b = a
   ScalarU :/: b = b
   a :/: ScalarU = a
   f a :/: f b = f (a :/: b)
   a :/: f b = f (a :/: b)
   Double :/: b = Double
   a :/: Double = Double
   AmountU :/: PriceU = QuantityU
   AmountU :/: QuantityU = PriceU
   ab :/: b = ab :/ b


instance Mul Integer Double where
  i ^* x = fromIntegral i * x
  -- y ^/ i = y / fromIntegral i 

instance Mul Double Double where
  x ^* y = x * y
  
instance (Num a) =>  Mul (MeasureF u a) (MeasureF v a)  where
  (^*) = mulmeasure
instance (Fractional a) =>  Div (MeasureF u a) (MeasureF v a)  where
  (^/) = divmeasure

--

mulmeasure :: (Num a) => MeasureF u a -> MeasureF v a -> MeasureF (u :*: v) a --  a :*: MeasureF v a
mulmeasure (Measure x) (Measure y) = Measure (x * y)

instance (Mul u v, Mul u' v') => Mul (u, u') (v, v') where
   (u, u') ^* (v, v') = (u ^* v, u' ^* v')

divmeasure :: Fractional a => MeasureF uv a -> MeasureF v a -> MeasureF (uv :/: v) a
divmeasure (Measure x) (Measure y) = Measure (x / y)


instance (KnownNat n, Mul a b ) => Mul (VS.Vector n a ) (VS.Vector n b)  where
    (^*) = liftA2 (^*)
instance (KnownNat n, Div a b ) => Div (VS.Vector n a ) (VS.Vector n b)  where
    (^/) = liftA2 (^/)

instance (KnownNat n, Mul a b, VG.Vector v a, VG.Vector v b, VG.Vector v (a :*: b) ) => Mul (VGS.Vector v n a ) (VGS.Vector v n b)  where
  (^*) = VGS.zipWith (^*)
instance (KnownNat n, Div ab b, VG.Vector v ab, VG.Vector v b, VG.Vector v (ab :/: b) ) => Div (VGS.Vector v n ab ) (VGS.Vector v n b)  where
 (^/) = VGS.zipWith (^/)

-- instance (KnownNat n, Mul a b, VG.Vector v a, VG.Vector v b, VG.Vector v (a :*: b) ) => Mul (VGS.Vector v n a ) b   where
  -- v ^* x = VGS.map (^* x) v
data a :* b 
data a :/ b   
