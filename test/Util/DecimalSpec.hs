module Util.DecimalSpec (spec) where
import Prelude
import Test.Hspec
import Util.Decimal
import Test.QuickCheck(property, (===), elements)

spec :: Spec
spec = roundingSpec

roundingSpec = describe "@Rounding" $ do
  it "rounds down 0.9 to 0" $ do
    applyRounding (RoundDown 0) 0.9 `shouldBe` 0
  it "rounds up 0.1 to 1" $ do
    applyRounding (RoundUp 0) 0.1 `shouldBe` 1
  it "rounds 0.5 to 1 " $ do
    applyRounding (Round 0) 0.5 `shouldBe` 1
  it "rounds 1.5 to 2 " $ do
    applyRounding (Round 0) 1.5 `shouldBe` 2
  it "banker rounds 0.5 to 0 " $ do
    applyRounding (RoundBanker 0) 0.5 `shouldBe` 0
  it "banker rounds 1.5 to 2 " $ do
    applyRounding (RoundBanker 0) 1.5 `shouldBe` 2

  it "is idempotent" $ property $ \x0 -> do
    -- \x -> x + 1 === (x + 1 ::  Double)
           r0 <- elements [0..3 :: Int]
           let x' = applyRounding r x
               x = realFracToDecimal 4 (x0 :: Double)
               r = case r0 of
                      0 -> RoundDown 2
                      1 -> RoundUp 2
                      2 -> Round 2
                      3 -> RoundBanker 2
                      _ -> error "Bug r0 shoulb be element [0..3]"
           return $ applyRounding r x' === x'
  it "convert back on forth without loss" $ property $
      \x ->
          let r = Round 2
              d = toDecimalWithRounding r (x :: Double)
          in toDecimalWithRounding r ( realFracToDecimal 2 d) === d
spec :: Spec
