module GL.Payroll.ParserSpec(spec) where
import Prelude
import GL.Payroll.Parser
import GL.Payroll.Timesheet
import Data.Maybe

import Test.Hspec

import Data.Time (Day)
import qualified Data.Time as Time
import Data.These

day0 = Time.fromGregorian 2015 06 01
makeDay :: Integer -> Day
makeDay i = Time.addDays i day0

lockAD x = lockA x >>= lockD
alice = PayrooEmployee  "Alice" "" 1 (Employee "Alice" Nothing)
spec = describe "@Payroll" $ do
    describe "#Tokenizer" $ do
        it "parses payrollId" $ do
            token "#45" `shouldBe` (Right $ PayrollIdT 45)

        it "parses name" $ do
            token "Hello" `shouldBe` (Right $ NameT "Hello")

        it "parses simple duration" $ do
            token "4.5" `shouldBe` (Right $ DurationT Work $ lockD 4.5)

        it "parses holiday" $ do
            token "!4.5" `shouldBe` (Right $ DurationT Holiday $ lockD 4.5)

        it "parses duration with minute" $ do
            token "4h30" `shouldBe` (Right $ DurationT Work $ lockD 4.5)
  
        it "parses range" $ do
            token "9:30-11:00" `shouldBe` (Right $ RangeT (fromJust $ Time.makeTimeOfDayValid 9 30 0)
                                                          (fromJust $ Time.makeTimeOfDayValid 11 0 0))
        it "parses rate" $ do
            token "$7.50" `shouldBe` (Right $ RateT $ lockA 7.5)
   
        it "parses Date" $ do
            token "2015/12/31" `shouldBe` (Right $ DayT (Time.fromGregorian 2015 12 31))

        it "parses Date" $ do
            token "2015-12-31" `shouldBe` (Right $ DayT (Time.fromGregorian 2015 12 31))
        it "parses external party"  $ do
            token "@a" `shouldBe` (Right $ ExternalT "a")
            token "@a1" `shouldBe` (Right $ ExternalT "a1")
        it "parses integer costs" $ do
            token "1^" `shouldBe` (Right $ DeductionAndCostT (lockA <$> Just 1) Nothing)
        it "parses real costs" $ do
            token "1.5^" `shouldBe` (Right $ DeductionAndCostT (lockA <$> Just 1.5) Nothing)
        it "parses integer deductions" $ do
            token "^1" `shouldBe` (Right $ DeductionAndCostT Nothing (Just $ lockA 1))
        it "parses real deductions" $ do
            token "^1.5" `shouldBe` (Right $ DeductionAndCostT Nothing (Just $ lockA 1.5))
        it "parses costs and deductions" $ do
            token "2.3^1.5" `shouldBe` (Right $ DeductionAndCostT (Just $ lockA 2.3) (Just $ lockA 1.5))
    describe "#Parsing" $ do
        it "one shift per line"  $ do
            let content = unlines
                          [ "2015/06/01"
                          , "$10"
                          , "Alice #1"
                          , "1"
                          , "2"
                          , "3"
                          ]

                expected = Right $ Timesheet ss day0 Weekly []
                ss = [
                    Shift (alice, makeDay (i-1), Work)
                          Nothing
                          (lockD $ fromIntegral i)
                          (lockAD  $ 10* (fromInteger i ))
                        | i <- [1..3]
                    ]

            parseFastTimesheet (lines content)
                `shouldBe` expected
        it "all shift in one line"  $ do
            let content = unlines
                            [ "2015/06/01"
                            , "$10"
                            , "Alice #1"
                            , "Alice 1 2 3"
                            ]

                expected = Right $Timesheet ss day0 Weekly []
                ss = [
                    Shift (alice, makeDay (i-1), Work)
                          Nothing
                          (lockD $ fromIntegral i)
                          (lockAD $ 10* (fromInteger i ))
                        | i <- [1..3]
                    ]

            parseFastTimesheet (lines content)
                `shouldBe` expected
        it "parses deduction and costs"  $ do
            let content = unlines
                            [ "2015/06/01"
                            , "$10"
                            , "Alice #1"
                            , "Alice"
                            , "@PAYE ^5"
                            , "@Pension 1.5^1.87"
                            ]
                expected = Right $Timesheet [] day0 Weekly [ DeductionAndCost ("PAYE", alice) (That $ lockA 5)
                                                           , DeductionAndCost ("Pension", alice) (These (lockA 1.5) (lockA 1.87))
                                                           ]
            parseFastTimesheet (lines content)
                `shouldBe` expected
        it "parses half multiplier"  $ do
            let content = unlines [ "2015/06/01"
                                  , "$10"
                                  , "Alice #1"
                                  , "Alice"
                                  , "2.5x8|!4"
                                  ]
                ss = [ Shift (alice, makeDay 0, Work) Nothing (lockD 8) (lockAD 80) 
                     , Shift (alice, makeDay 1, Work) Nothing (lockD 8) (lockAD 80)
                     , Shift (alice, makeDay 2, Work) Nothing (lockD 4) (lockAD 40)
                     , Shift (alice, makeDay 2, Holiday) Nothing (lockD 4) (lockAD 40)
                     ]
                expected = Right $Timesheet ss day0 Weekly []
            parseFastTimesheet (lines content)
                `shouldBe` expected



