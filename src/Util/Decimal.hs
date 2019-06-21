module Util.Decimal
( module Util.Decimal
, module Data.Decimal
)
where
import ClassyPrelude
import Data.Decimal


-- * Decimal rounding


data RoundingMethod = Round {roundDec :: Word8 }
  | RoundUp {roundDec :: Word8 }
  | RoundDown {roundDec :: Word8 }
  | RoundBanker {roundDec :: Word8 }
  deriving (Eq, Show, Read)

applyRounding :: RoundingMethod -> Decimal -> Decimal
applyRounding method x = case method of
  RoundUp dec -> roundTo' ceiling dec x
  RoundDown dec -> roundTo' floor dec x
  RoundBanker dec -> roundTo' round dec x
  Round dec -> let -- normal, do the rounding manually
    x' = roundTo (dec+1) x
    lastDigit = decimalMantissa x' `mod` 10
    in if lastDigit < 5
       then applyRounding (RoundDown dec) x
       else applyRounding (RoundUp dec) x
    
toDecimalWithRounding :: RealFrac f => RoundingMethod -> f -> Decimal
toDecimalWithRounding method f = applyRounding method $ realFracToDecimal (roundDec method + 1) f
