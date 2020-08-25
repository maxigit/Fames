module Util.Decimal
( module Util.Decimal
, module Data.Decimal
)
where
import ClassyPrelude
import Data.Decimal


-- * Decimal rounding


data RoundingMethod = Round {roundDec' :: Word8 }
  | RoundUp {roundDec' :: Word8 }
  | RoundDown {roundDec' :: Word8 }
  | RoundBanker {roundDec' :: Word8 }
  | RoundAbs RoundingMethod 
  deriving (Eq, Show, Read)

roundDec :: RoundingMethod -> Word8
roundDec (RoundAbs r) = roundDec r
roundDec r = roundDec' r

applyRounding :: RoundingMethod -> Decimal -> Decimal
applyRounding method x = case method of
  RoundUp dec -> roundTo' ceiling dec x
  RoundDown dec -> roundTo' floor dec x
  RoundBanker dec -> roundTo' round dec x
  Round dec -> let -- normal, do the rounding manually
    -- we need to check the we are in the case 0.5 (with correct deci)
    -- lastDigit =  (decimalMantissa x `div` fromIntegral (decimalPlaces x - dec - 1)) `mod` 10
    lastDigit = 
      -- traceShow ("x", x, dec) $ traceShowId  $
      (decimalMantissa x `div` (fromIntegral $ 10 ^ (decimalPlaces x - dec - 1)))`mod` 10
    in if lastDigit < 5
                 then applyRounding (RoundDown dec) x
                 else applyRounding (RoundUp dec) x
        
  RoundAbs l_round -> signum x * applyRounding l_round (abs x)
    
toDecimalWithRounding :: RealFrac f => RoundingMethod -> f -> Decimal
toDecimalWithRounding method f = applyRounding method $ realFracToDecimal (roundDec method + 1) f
toDecimalWithRounding' :: RealFrac f => Word8 -> RoundingMethod -> f -> Decimal
toDecimalWithRounding' dec method f = applyRounding method $ realFracToDecimal (dec) f
