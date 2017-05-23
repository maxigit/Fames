module Handler.WHStockAdjustmentSpec where

import TestImport
import Handler.WH.StockAdjustment

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
       nextWorkingDay (fromGregorian 2017 05 22) `shouldBe` (fromGregorian 2017 05 19)

  
