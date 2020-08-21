module GL.Payroll.TimesheetSpec(spec) where
import Prelude
import GL.Payroll.Timesheet
import GL.Utils

import Test.Hspec

import Data.Time

startFrom year month day = Start (fromGregorian year month day)
mkPeriod freq year month day = Period freq (startFrom year month day)
spec :: Spec
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
        context "for corner case" $ do
          context "#week" $ do
            it "first long week" $ do
              longRef (mkPeriod Weekly 2018 04 06) (fromGregorian 2018 04 08 ) `shouldBe` "2018/01"
            it "first long week (same day)" $ do
              longRef (mkPeriod Weekly 2018 04 06) (fromGregorian 2018 04 06 ) `shouldBe` "2018/01"
            it "last long week (53)" $ do
              longRef (mkPeriod Weekly 2018 04 06) (fromGregorian 2018 04 05 ) `shouldBe` "2017/53"
            it "last long week (52)" $ do
              longRef (mkPeriod Weekly 2018 04 06) (fromGregorian 2018 04 04 ) `shouldBe` "2017/52"
          context "#Month" $ do
            it "first long month" $ do
              longRef (mkPeriod Monthly 2018 04 06) (fromGregorian 2018 04 08 ) `shouldBe` "2018/M01"

            it "first long month" $ do
              longRef (mkPeriod Monthly 2018 04 06) (fromGregorian 2018 04 06 ) `shouldBe` "2018/M01"
            it "last long month" $ do
              longRef (mkPeriod Monthly 2018 04 06) (fromGregorian 2018 04 01 ) `shouldBe` "2017/M12"

      context "dayRef" $ do
        it "short Month" $ do
          dayRef (mkPeriod Weekly 2018 01 01) (fromGregorian 2018 01 04 ) `shouldBe` "Thu"
        it "long Month" $ do
          dayRef (mkPeriod Monthly 2018 01 01) (fromGregorian 2018 01 11) `shouldBe` "Thu-11"
        -- it "for GRN" $ do
        --   pending $ "grnRef (Period Weekly $ fromGregorian 2018 01 01) (fromGregorian 2018 ) `shouldBe` '1801/Mon'"

      describe "date splitter" $ do
        context "weekNumber" $ do
          it "find same day" $ do
            weekNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 04 06) `shouldBe` (2018, 01)
          it "find last day" $ do
            weekNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 04 12) `shouldBe` (2018, 01)
          it "find previous day" $ do
            weekNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 04 05) `shouldBe` (2017, 53)
          it "find next period" $ do
            weekNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 04 13) `shouldBe` (2018, 2)
        context "monthNumber" $ do
          it "find same day" $ do
            monthNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 04 06) `shouldBe` (2018, 01)
          it "find last day" $ do
            monthNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 05 05) `shouldBe` (2018, 01)
          it "find previous day" $ do
            monthNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 04 05) `shouldBe` (2017, 12)
          it "find next period" $ do
            monthNumber (Start $ fromGregorian 2018 04 06) (fromGregorian 2018 05 06) `shouldBe` (2018, 2)
