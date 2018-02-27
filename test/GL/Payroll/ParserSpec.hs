module GL.Payroll.ParserSpec(spec) where
import Prelude
import GL.Payroll.Parser
import GL.Payroll.Timesheet

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


