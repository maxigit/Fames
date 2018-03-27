module GL.Payroll.UtilsSpec (spec) where
import Prelude
import GL.Utils
import GL.Payroll.Settings
import Test.Hspec

check calculator s1 s2 = let
  d1 = read s1
  d2 = read s2
  in calculateDate calculator d1 `shouldBe` d2


spec = describe "@Date Calculator" $ do
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
  context "DayOfWeek" $ do
    it "without cuttof" $ do
      check (NextDayOfWeek Wednesday Wednesday) "2018-04-01" "2018-04-04"
    it "with cuttof" $ do
      check (NextDayOfWeek Wednesday Sunday) "2018-04-01" "2018-04-11"
    
