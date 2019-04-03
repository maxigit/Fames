module Handler.Items.BatchSpec where

import TestImport
import Handler.Items.Batches
import Handler.Items.Batches.Matches
import ModelField
import Test.QuickCheck(property, (===), (==>), Arbitrary(..), arbitraryBoundedEnum)

spec :: Spec
spec = parallel pureSpec 

instance Arbitrary MatchQuality where
  arbitrary = arbitraryBoundedEnum

shouldThrowPure x e = seq x (return ()) `shouldThrow` e
pureSpec = do 
  describe "@current @Batches Matches" $ do
      describe "#mergeBatchMatches" $ do
        describe "#MergeRaisesError" $ do
          it "works with only one batch" $ do
            mergeBatchMatches MergeRaisesError "sku" [ [ ("A", Excellent) , ("B", Bad) ]
                                                     ]
              `shouldBe` Just [ ("A", Excellent), ("B", Bad) ]
          it "works with similiar batches" $ do
            mergeBatchMatches MergeRaisesError "sku" [ [ ("A", Excellent) ] 
                                                     , [ ("A", Excellent) ]
                                                     ]
              `shouldBe` Just [ ("A", Excellent) ]
          -- it "takes the lowest in case of intersection" $ do
          --   mergeBatchMatches MergeRaisesError "sku" [ [ ("A", Excellent)]
          --                                            , [ ("A", Good) ]
          --                                            ]
          --     `shouldBe` Just [("A", Good)] 
          -- it "but only if the result is good enought" $ do
          --   mergeBatchMatches MergeRaisesError "sku" [ [ ("A", Excellent)]
          --                                            , [ ("A", Fair) ]
          --                                            ]
              -- `shouldThrowPure` anyErrorCall
      describe "#SafeMatch" $ do
          it "works with only one batch" $ do
            mergeBatchMatches SafeMatch "sku" [ [ ("A", Excellent) , ("B", Bad) ]
                                                     ]
              `shouldBe` Just [ ("A", Excellent), ("B", Bad) ]
          it "takes the lowest in case of intersection" $ do
            mergeBatchMatches SafeMatch "sku" [ [ ("A", Excellent)]
                                                     , [ ("A", Good) ]
                                                     ]
              `shouldBe` Just [("A", Good)] 
          it "gets lowest  of the intersection" $ do
            mergeBatchMatches SafeMatch "sku" [ [ ("FPK", Fair), ("FPK", Good)
                                                , ("BGM", Close), ("BGM", Fair)
                                                ]
                                              , [ ("CES", Close)
                                                , ("BGM", Bad)
                                                ]
                                              ]
              `shouldBe` Just [("BGM", Bad)] 
      describe "@Expand" $ do
        let [b1, b2, b3] = map BatchKey [1..3]
            op = OperatorKey 1
            day1 = fromGregorian 2019 04 01
            day2 = fromGregorian 2019 05 01
            docKey = DocumentKeyKey' 1
            nokey = DocumentKeyKey' 0
            -- tweak expandMatche to set the comment
            expandMatches0 ms = map updateComment $ expandMatches tshow ms where
              updateComment b = b {batchMatchComment = Just "<expanded>"}
            
        it "should connect 2 batches" $ do
          expandMatches0 
                        [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        ]  `shouldBe`
                        [ BatchMatch b3 "A" b1 "A" Nothing Good (Just "<expanded>") day1 nokey
                        ]
        it "should remove duplicate guesses " $ do
          expandMatches0 
                        [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        , BatchMatch b1 "A" b3 "A" Nothing   Good Nothing day1 docKey
                        ]  `shouldBe`
                        []
        it "should keeps the given ones" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" (Just op) Excellent (Just "Via #3") day1 docKey
                              , BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day2 docKey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" (Just op) Excellent (Just "Via #3") day1 docKey
                              ]
        it "should keeps the given ones (reverse order)" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 docKey
                              , BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day2 docKey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day2 docKey
                              ]
        it "should keep guess with the last doc key" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 nokey
                              , BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day2 docKey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day2 docKey
                              ]
        it "should keep guess with the last doc key (reverse order)" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 docKey
                              , BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day2 nokey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 docKey
                              ]
      describe "#mergeQuality" $ do
          it  "should be symmetric" $ property $
                \(a, b) -> mergeQualities [a , b] ===  mergeQualities [b , a]
          it  "should be transitive" $ property $
                \(a, b, c) -> mergeQualities [a, b, c]
                    === mergeQualities [b, c, a]
          it "2 excellents make a excellent" $ do
            mergeQualities [Excellent, Excellent] `shouldBe` Just Excellent
          it "3 excellent make a good" $ do
             mergeQualities [Excellent,  Excellent , Excellent] `shouldBe` Just Good
          it "2 goods make a good" $ do
             mergeQualities [Good , Good ] `shouldBe` Just Good
          it "2 goods and a excelent make a fair" $ do
             mergeQualities [Good , Good , Excellent ]  `shouldBe` Just Fair
          it "Good <> Fair => Fair" $ do
             mergeQualities [Good , Fair] `shouldBe` Just Fair
          it "Fair <> Fair " $ do
             mergeQualities [Fair , Fair] `shouldBe` Just Close
          describe "#qToD" $ do
            it "quality to double is idempotan" $ property $ do
              \q -> dToQ (qToD q) === q
            it "Quality + 0.5 stays the same" $ property $ do
              \q -> dToQ (qToD q + (qToD Excellent * 0.5)) === q
              

mergeQualities:: [MatchQuality] -> Maybe MatchQuality
mergeQualities qs = dToQ <$> mergeQualities' (map qToD qs)
mergeQualities' [] = Nothing
mergeQualities' [q] = Just q
mergeQualities' (d:ds) = mergeQualities' ds >>= (mergeQs d)

qToD Identical = 0
qToD Excellent = 1
qToD Good = 3
qToD Fair = 7
qToD Close = 14
qToD Bad = 50

dToQ :: Double -> MatchQuality
dToQ d = let qs = [Bad .. Identical]
             go d [] = error "proven to not happen"
             go d (q:qs) | d >= (qToD q) = q
                         | otherwise =  go d qs
         in    go d qs

mergeQs a b= Just $ a + b
