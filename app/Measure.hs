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
-- import Data.Coerce
-- imp

newtype MeasureF u a  = Measure { measured :: a }
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

infix 7 ^*, /^, *^
class Mul a b ab | a b -> ab, a ab -> b, b ab -> a where
    (^*) :: a -> b -> ab
    (/^) :: ab -> a -> b
    
    

(*^) :: Mul a b ab => b -> a -> ab
(*^) = flip (^*)

instance Mul Integer Double Double where
  i ^* x = fromIntegral i * x
  y /^ i = y / fromIntegral i 

  
instance (Fractional a) =>  Mul (MeasureF ScalarU a) (MeasureF u a) (MeasureF u a) where
  (^*) = mulmeasure
  (/^) = divmeasure
--
-- We remove the following instance because in would implied that
-- ab -> a -> b ie. a -> Scalar -> a
-- instance (Num (f Double), Fractional (f Double)) =>  Mul (Measures a f) (Measures ScalarU f) (Measures a f) where
--   (^*) = mulmeasure
--   (/^) = divmeasure

instance (Fractional a) =>  Mul (MeasureF QuantityU a) (MeasureF PriceU a) (MeasureF AmountU a) where
  (^*) = mulmeasure
  (/^) = divmeasure

instance (Fractional a) =>  Mul (MeasureF PriceU a) (MeasureF QuantityU a) (MeasureF AmountU a) where
  (^*) = mulmeasure
  (/^) = divmeasure

mulmeasure :: Num a => MeasureF u a -> MeasureF v a -> MeasureF uv a
mulmeasure (Measure x) (Measure y) = Measure (x * y)

divmeasure :: Fractional a => MeasureF u a -> MeasureF v a -> MeasureF uv a
divmeasure (Measure x) (Measure y) = Measure (x / y)


instance (KnownNat n, Mul a b ab) => Mul (VS.Vector n a ) (VS.Vector n b) (VS.Vector n ab) where
  (^*) = liftA2 (^*)
  (/^) = liftA2 (/^)

instance (KnownNat n, Mul a b ab, VG.Vector v a, VG.Vector v b, VG.Vector v ab) => Mul (VGS.Vector v n a ) (VGS.Vector v n b) (VGS.Vector v n ab) where
  (^*) = VGS.zipWith (^*)
  (/^) = VGS.zipWith (/^)

data a :* b 
data a :/ b

-- (^^*)  :: Mul a b ab => a -> VS.Vector n b -> VS.Vector n ab 
-- x ^^* v = VS.map (*^x) v

{- Can't be made generice because it overlaps with previous definition
   We could define Amount as Quantity :* Price
   but we couldn't instantiate :/ 
   maybe we can use close type family ?
 
 HOWEMER this could be done for every combination we need

instance (Fractional a) =>  Mul (MeasureF u a) (MeasureF v a) (MeasureF (u :* v) a) where
  (^*) = mulmeasure
  (/^) = divmeasure

 -}
instance (Fractional a) =>  Mul (MeasureF DayU a) (MeasureF (v :/ DayU) a) (MeasureF v a) where
  (^*) = mulmeasure
  (/^) = divmeasure

instance (Fractional a) =>  Mul (MeasureF YearU a) (MeasureF (v :/ YearU) a) (MeasureF v a) where
  (^*) = mulmeasure
  (/^) = divmeasure
{- 
instance (Num (f Double), Fractional (f Double)) =>  Mul (Measures DayU f) (Measures (a :/ DayU)  f) (Measures a f) where
  (^*) = mulmeasure
  (/^) = divmeasure

instance (Num (f Double), Fractional (f Double)) =>  Mul (Measures YearU f) (Measures (a :/ YearU)  f) (Measures a f) where
  (^*) = mulmeasure
  (/^) = divmeasure
-}
