module GL.Payroll.TimesheetSpec(spec) where
import Prelude
import GL.Payroll.Timesheet

import Test.Hspec

import Data.Time

spec = describe "@Timesheet" $ do
  context "adjustTaxYear" $ do
    it "" $ do adjustTaxYear (fromGregorian 2017 01 01) (fromGregorian 2017 01 01) `shouldBe` fromGregorian 2017 01 01
    it "finds the beginning of the year" $ do adjustTaxYear (fromGregorian 2017 01 01) (fromGregorian 2017 01 02) `shouldBe` fromGregorian 2017 01 01
    it "finds the beginning of the next year" $ do adjustTaxYear (fromGregorian 2017 01 01) (fromGregorian 2018 01 02) `shouldBe` fromGregorian 2018 01 01
    it "finds the beginning of the previous year" $ do adjustTaxYear (fromGregorian 2017 01 01) (fromGregorian 2016 12 31) `shouldBe` fromGregorian 2016 01 01
  context "week" $ do
    it "finds the correct week on the same day" $ do weekNumber (fromGregorian 2017 01 01) (fromGregorian 2017 01 01) `shouldBe` (2017, 1)
    it "finds week N#2" $ do weekNumber (fromGregorian 2017 01 01) (fromGregorian 2017 01 10) `shouldBe` (2017, 2)
    it "the correct week on the next year" $ do weekNumber (fromGregorian 2017 01 01) (fromGregorian 2018 01 02) `shouldBe` (2018, 1)
    it "the previous year last week" $ do weekNumber (fromGregorian 2017 01 01) (fromGregorian 2016 12 31) `shouldBe` (2016, 53)
