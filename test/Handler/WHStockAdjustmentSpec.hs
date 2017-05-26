module Handler.WHStockAdjustmentSpec where

import TestImport
import Handler.WH.StockAdjustment
import SharedStockAdjustment

spec :: Spec
spec = parallel pureSpec

pureSpec = describe "@pure @parallel" $ do
  context "next working day without rolling" $ do
     it"get next day" $ do
       nextWorkingDay (fromGregorian 2017 05 22) `shouldBe` (fromGregorian 2017 05 23)
     it "get previous day" $ do
       previousWorkingDay (fromGregorian 2017 05 23) `shouldBe` (fromGregorian 2017 05 22)
  context "next working day rolling" $ do
     it "get next monday day" $ do
       nextWorkingDay (fromGregorian 2017 05 19) `shouldBe` (fromGregorian 2017 05 22)
     it "get previous friday day" $ do
       previousWorkingDay (fromGregorian 2017 05 22) `shouldBe` (fromGregorian 2017 05 19)
  describe "computeBadges" $ do
    let o0 = OriginalQuantities 0 0 0 Nothing
        b0 = BadgeQuantities 0 0 0 0 0
    it "find 1 missing" $ do
      computeBadges (OriginalQuantities 5 6 1 Nothing)
        `shouldBe` b0 {bMissing = 1}
    it "find 1 new" $ do
      computeBadges (OriginalQuantities 7 6 0 Nothing)
        `shouldBe` b0 {bNew = 1}
    it "find 1 found" $ do
      computeBadges (OriginalQuantities 7 6 5 Nothing)
        `shouldBe` b0 {bFound = 1}
    it "bug " $ do
      computeBadges (OriginalQuantities 12 11 1 Nothing)
        `shouldBe` b0 {bFound = 1}
    it "find some found and some new" $ do
      computeBadges o0 {qtake = 12, qoh = 5, qlost = 4}
        `shouldBe` b0 {bFound = 4, bNew = 3}
    describe "using modulo" $ do 
      let o6 = o0 {qModulo = Just 6}
      context "missing in stocktake" $ do
        it "find 1 missing" $ do
          computeBadges o6 {qtake = 5, qoh = 12}
            `shouldBe` b0 {bMissing = 1, bMissingMod = 6}
        it "find 1 too many:new" $ do
          computeBadges o6 {qtake = 7, qoh = 12}
            `shouldBe` b0 {bNew =1, bMissingMod = 6 }
        it "find 1 too many:lost" $ do
          computeBadges o6 {qtake = 7, qoh = 12, qlost = 1}
            `shouldBe` b0 {bFound =1, bMissingMod = 6 }
      context "missing in FA" $ do 
        context "new" $ do
          it "find 1 missing" $ do
            computeBadges o6 {qtake = 5}
              `shouldBe` b0 {bMissing = 1, bFoundMod = 6}
          it "find 1 too many (new)" $ do
            computeBadges o6 {qtake = 7}
              `shouldBe` b0 {bNew = 1, bFoundMod = 6}
          it "find 1 too many (found)" $ do
            computeBadges o6 {qtake = 7, qlost = 1}
              `shouldBe` b0 {bFound = 1, bFoundMod = 6}
          it "find 0 missing" $ do
            computeBadges o6 {qtake = 5, qoh=5}
              `shouldBe` b0 
        context "lost" $ do
          it "@fail find all missing" $ do
            computeBadges o6 {qtake = 5, qlost=5}
              `shouldBe` b0 {bFound=5}
          it "find 1 too many" $ do
            computeBadges o6 {qtake = 8, qlost = 1}
              `shouldBe` b0 {bNew = 1, bFound = 1, bFoundMod = 6}
      context "edge case" $ do
        -- 3 missing should be seen as missing, not as found
            it "find 2 too many" $ do
              computeBadges o6 {qtake = 2}
                `shouldBe` b0 {bNew = 2}
            it "find 3 missing" $ do
              computeBadges o6 {qtake = 3}
                `shouldBe` b0 {bMissing = 3, bFoundMod = 6}
            it "find 2 missing" $ do
              computeBadges o6 {qtake = 4}
                `shouldBe` b0 {bMissing = 2, bFoundMod = 6}
      context "remove full boxes missing" $ do
        it "cancels 12 missing" $ do
              computeBadges o6 {qtake = 6, qoh = 24}
                `shouldBe` b0 {bMissingMod = 18}
        it "cancels 12 found" $ do
              computeBadges o6 {qtake = 24, qoh = 6}
                `shouldBe` b0 {bFoundMod = 18}
      context "modulo not needed" $ do
        it "find 1 missing" $ do
          computeBadges o6 {qtake = 5, qoh = 6, qlost = 1}
            `shouldBe` b0 {bMissing = 1}
        it "find 1 new" $ do
          computeBadges o6 {qtake = 7, qoh = 6}
            `shouldBe` b0 {bNew = 1}
        it "find 1 found" $ do
          computeBadges o6 {qtake = 7, qoh = 6, qlost = 5}
            `shouldBe` b0 {bFound = 1}
        it "bug " $ do
          computeBadges o6 {qtake = 12, qoh = 11, qlost = 1}
            `shouldBe` b0 {bFound = 1}
        it "find some found and some new" $ do
          computeBadges o6 {qtake = 12, qoh = 4, qlost = 1}
            `shouldBe` b0 {bFound = 1, bNew = 1, bFoundMod =6}
      context "bugs" $ do
        it "find not too much some found and some new" $ do
          computeBadges o6 {qtake = 8, qoh = 20, qlost = 9}
            `shouldBe` b0 {bMissingMod = 12}
      




