module GL.Payroll.ParserSpec(spec) where
import Prelude
import GL.Payroll.Parser
import GL.Payroll.Timesheet
import Data.Maybe

import Test.Hspec

import Data.Time (Day)
import qualified Data.Time as Time

day0 = Time.fromGregorian 2015 06 01
makeDay :: Integer -> Day
makeDay i = Time.addDays i day0

alice = Employee  "Alice" "" "Alice" Nothing 1
spec = describe "@Payroll" $ do
    describe "#Tokenizer" $ do
        it "parses payrollId" $ do
            token "#45" `shouldBe` PayrollIdT 45

        it "parses name" $ do
            token "Hello" `shouldBe` NameT "Hello"

        it "parses simple duration" $ do
            token "4.5" `shouldBe` DurationT Work 4.5

        it "parses holiday" $ do
            token "!4.5" `shouldBe` DurationT Holiday 4.5

        it "parses duration with minute" $ do
            token "4h30" `shouldBe` DurationT Work 4.5
  
        it "parses range" $ do
            token "9:30-11:00" `shouldBe` RangeT (fromJust $ Time.makeTimeOfDayValid 9 30 0)
                                                 (fromJust $ Time.makeTimeOfDayValid 11 0 0)
        it "parses rate" $ do
            token "$7.50" `shouldBe` RateT 7.5
   
        it "parses Date" $ do
            token "2015/12/31" `shouldBe` DayT (Time.fromGregorian 2015 12 31)

        it "parses Date" $ do
            token "2015-12-31" `shouldBe` DayT (Time.fromGregorian 2015 12 31)
    describe "#Parsing" $ do
        it "one shift per line"  $ do
            let content = "\
                          \2015/06/01\n\
                          \$10\n\
                          \Alice #1\n\
                          \1\n\
                          \2\n\
                          \3\n\
                          \"

                expected = Timesheet ss day0
                ss = [
                    Shift (alice, makeDay (i-1), Work)
                          Nothing
                          (fromIntegral i)
                          (10* (fromInteger i ))
                        | i <- [1..3]
                    ]

            parseFastTimesheet (lines content)
                `shouldBe` expected
        it "all shift in one line"  $ do
            let content = "\
\2015/06/01\n\
\$10\n\
\Alice #1\n\
\Alice 1 2 3\n\
\"

                expected = Timesheet ss day0
                ss = [
                    Shift (alice, makeDay (i-1), Work)
                          Nothing
                          (fromIntegral i)
                          (10* (fromInteger i ))
                        | i <- [1..3]
                    ]

            parseFastTimesheet (lines content)
                `shouldBe` expected


