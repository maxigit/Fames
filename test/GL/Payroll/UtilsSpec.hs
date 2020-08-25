{-# LANGUAGE NegativeLiterals #-}
module GL.Payroll.UtilsSpec (spec) where
import Prelude
import GL.Utils
import GL.Payroll.Settings
import Test.Hspec
import Data.Time (fromGregorian)
import Data.Map(fromList)

check calculator s1 s2 = let
  d1 = read s1
  d2 = read s2
  in calculateDate calculator d1 `shouldBe` d2


spec :: Spec
spec = dateSpec >> periodSpec
dateSpec = describe "@DateCalculator" $ do
  context "AddDays" $ do
    it "get same day" $ do
      check (AddDays 0) "2018-03-27" "2018-03-27"
    it "next day" $ do
      check (AddDays 1) "2018-03-27" "2018-03-28"
    it "previous day" $ do
      check (AddDays (-1)) "2018-03-27" "2018-03-26"
    it "over month" $ do
      check (AddDays 30) "2018-03-27" "2018-04-26"
  context "DayOfMonth" $ do
    it "without cuttof" $ do
      check (DayOfMonth 19 6) "2018-03-27" "2018-04-19"
    it "without cuttof" $ do
      check (DayOfMonth 19 6) "2018-04-05" "2018-04-19"
    it "with cuttof" $ do
      check (DayOfMonth 19 6) "2018-04-06" "2018-05-19"
    it "with cuttof" $ do
      check (DayOfMonth 19 19) "2018-04-19" "2018-05-19"
    it "without cuttof" $ do
      check (DayOfMonth 19 19) "2018-04-18" "2018-04-19"
    context "with cuttof before" $ do
      it "with cuttof" $ do
        check (DayOfMonth 6 19) "2018-04-10" "2018-05-06"
      it "with cuttof" $ do
        check (DayOfMonth 6 19) "2018-04-20" "2018-06-06"
  context "DayOfWeek" $ do
    it "without cuttof" $ do
      check (NextDayOfWeek Wednesday Wednesday) "2018-04-01" "2018-04-04"
    it "with cuttof" $ do
      check (NextDayOfWeek Wednesday Sunday) "2018-04-01" "2018-04-11"
  context "Beginning Of Week" $ do
    it "stays the same" $ do
      check (BeginningOfWeek Monday) "2019-05-06" "2019-05-06"
    it "find the previous one " $ do
      check (BeginningOfWeek Monday) "2019-05-07" "2019-05-06"
  context "Chain"  $ do
    it "apply +1 after moving to sunday" $ do
      check (Chain [BeginningOfWeek Sunday, AddDays -1] ) "2019-05-05" "2019-05-04"
    it "apply +1 before moving to sunday" $ do
      check (Chain [AddDays -1, BeginningOfWeek Sunday] ) "2019-05-12" "2019-05-05"
  context "Oldest" $ do
    it "find the oldest" $ do
      check (Oldest [AddDays 0, AddDays 1]) "2019-05-05" "2019-05-05"
    it "find the oldest" $ do
      check (Oldest [AddDays 1, AddDays 0]) "2019-05-05" "2019-05-05"
    
  context "Newest" $ do
    it "find the newest" $ do
      check (Newest [AddDays 0, AddDays 1]) "2019-05-05" "2019-05-06"
    it "find the newest" $ do
      check (Newest [AddDays 1, AddDays 0]) "2019-05-05" "2019-05-06"
  context "WeekDayCase" $ do
    it "use day" $ do
      check (WeekDayCase $ fromList  [(Just Friday, AddDays 1)]) "2019-05-03" "2019-05-04"
    it "use default" $ do
      check (WeekDayCase $ fromList  [(Nothing, AddDays 1)]) "2019-05-03" "2019-05-04"
    it "ignore rule  default" $ do
      check (WeekDayCase $ fromList  [(Just Tuesday, AddDays 1)]) "2019-05-03" "2019-05-03"
  context "helper" $ do
    context "#previousMonthStarting At" $ do
      it "finds same day" $ do
        previousMonthStartingAt 6 (fromGregorian 2018 03 06) `shouldBe` fromGregorian 2018 03 06
      it "finds current month" $ do
        previousMonthStartingAt 6 (fromGregorian 2018 03 26) `shouldBe` fromGregorian 2018 03 06
      it "finds previous month" $ do
        previousMonthStartingAt 6 (fromGregorian 2018 03 02) `shouldBe` fromGregorian 2018 02 06
  context "test bank default" $ do
    let defaultCalculator = Oldest [ AddDays (-1)
                                , Chain [ NextDayOfWeek Friday Tuesday
                                        ,  AddWeeks (-1)
                                        ]
                                ]
    it "Friday -> Thursday" $ check defaultCalculator "2019-05-03" "2019-05-02"
    it "Sat -> Fri" $ check defaultCalculator "2019-05-04" "2019-05-03"
    it "Mon -> Fri" $ check defaultCalculator "2019-05-06" "2019-05-03"
    it "Tue -> Mon " $ check defaultCalculator "2019-05-07" "2019-05-06"

periodSpec = describe "@Period" $ do
  context "whole year" $ do
    let folding = FoldYearly (fromGregorian 2018 01 01)
    it "stays in current period" $ do
      foldTime  folding (fromGregorian 2018 03 02)
             `shouldBe` (fromGregorian 2018 03 02, Start (fromGregorian 2018 01 01))
    it "find previous period" $ do
      foldTime  folding (fromGregorian 2017 03 02)
             `shouldBe` (fromGregorian 2018 03 02, Start (fromGregorian 2017 01 01))
    it "find next period" $ do
      foldTime  folding (fromGregorian 2019 03 02)
             `shouldBe` (fromGregorian 2018 03 02, Start (fromGregorian 2019 01 01))
    it "manages leap year" $ do
      foldTime  folding (fromGregorian 2016 02 29)
             `shouldBe` (fromGregorian 2018 02 28, Start (fromGregorian 2016 01 01))
  context "partial yearl" $ do
    let folding = FoldYearly (fromGregorian 2018 05 01)
    it "stays in current period" $ do
      foldTime  folding (fromGregorian 2019 03 02)
             `shouldBe` (fromGregorian 2019 03 02, Start (fromGregorian 2018 05 01))
    it "find previous period" $ do
      foldTime  folding (fromGregorian 2018 03 02)
             `shouldBe` (fromGregorian 2019 03 02, Start (fromGregorian 2017 05 01))
    it "find next period" $ do
      foldTime  folding (fromGregorian 2020 03 02)
             `shouldBe` (fromGregorian 2019 03 02, Start (fromGregorian 2019 05 01))
    it "manages leap year" $ do
      foldTime  folding (fromGregorian 2020 02 29)
             `shouldBe` (fromGregorian 2019 02 28, Start (fromGregorian 2019 05 01))
  context "whole monthly" $ do
    let folding = FoldMonthly 2018
    it "stays in current period" $ do
      foldTime  folding (fromGregorian 2018 03 02)
             `shouldBe` (fromGregorian 2018 01 02, Start (fromGregorian 2018 03 01))
    it "find previous period" $ do
      foldTime  folding (fromGregorian 2017 03 02)
             `shouldBe` (fromGregorian 2018 01 02, Start (fromGregorian 2018 03 01))
  context "weekly" $ do
    let folding = FoldWeekly
    it "stays in current period" $ do
      foldTime  folding (fromGregorian 2018 06 08)
             `shouldBe` (fromGregorian 2018 01 05, Start (fromGregorian 2018 06 04))
             --                   ^ Weekly fold to 2018-01-01
    -- it "find previous period" $ do
    --   foldTime  folding (fromGregorian 2017 03 02)
    --          `shouldBe` (fromGregorian 2018 01 02, Start (fromGregorian 2018 03 01))
