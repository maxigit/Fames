module GL.Payroll.TimesheetSpec(spec) where
import Prelude
import GL.Payroll.Timesheet

import Test.Hspec

import Data.Time

startFrom year month day = Start (fromGregorian year month day)
mkPeriod freq year month day = Period freq (startFrom year month day)
spec = describe "@Timesheet" $ do
  context "adjustTaxYear" $ do
    it "" $ do adjustTaxYear (startFrom 2017 01 01) (fromGregorian 2017 01 01) `shouldBe` fromGregorian 2017 01 01
    it "finds the beginning of the year" $ do adjustTaxYear (startFrom 2017 01 01) (fromGregorian 2017 01 02) `shouldBe` fromGregorian 2017 01 01
    it "finds the beginning of the next year" $ do adjustTaxYear (startFrom 2017 01 01) (fromGregorian 2018 01 02) `shouldBe` fromGregorian 2018 01 01
    it "finds the beginning of the previous year" $ do adjustTaxYear (startFrom 2017 01 01) (fromGregorian 2016 12 31) `shouldBe` fromGregorian 2016 01 01
  context "week" $ do
    it "finds the correct week on the same day" $ do weekNumber (startFrom 2017 01 01) (fromGregorian 2017 01 01) `shouldBe` (2017, 1)
    it "finds week N#2" $ do weekNumber (startFrom 2017 01 01) (fromGregorian 2017 01 10) `shouldBe` (2017, 2)
    it "the correct week on the next year" $ do weekNumber (startFrom 2017 01 01) (fromGregorian 2018 01 02) `shouldBe` (2018, 1)
    it "the previous year last week" $ do weekNumber (startFrom 2017 01 01) (fromGregorian 2016 12 31) `shouldBe` (2016, 53)
    describe "Period" $ do
      context "Generates reference" $ do
        it "short week" $ do
          shortRef (mkPeriod Weekly 2018 01 01) (fromGregorian 2018 01 01 ) `shouldBe` "1801"
        it "long week" $ do
          longRef (mkPeriod Weekly 2018 01 01) (fromGregorian 2018 01 01 ) `shouldBe` "2018/01"
        it "short Month" $ do
          shortRef (mkPeriod Monthly 2018 01 01) (fromGregorian 2018 01 01 ) `shouldBe` "18M01"
        it "long Month" $ do
          longRef (mkPeriod Monthly 2018 01 01) (fromGregorian 2018 01 01) `shouldBe` "2018/M01"
  
      context "dayRef" $ do
        it "short Month" $ do
          dayRef (mkPeriod Weekly 2018 01 01) (fromGregorian 2018 01 04 ) `shouldBe` "Thu"
        it "long Month" $ do
          dayRef (mkPeriod Monthly 2018 01 01) (fromGregorian 2018 01 11) `shouldBe` "Thu-11"
        -- it "for GRN" $ do
        --   pending $ "grnRef (Period Weekly $ fromGregorian 2018 01 01) (fromGregorian 2018 ) `shouldBe` '1801/Mon'"

