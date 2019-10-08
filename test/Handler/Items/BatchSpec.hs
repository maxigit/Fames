module Handler.Items.BatchSpec where

import TestImport
import Handler.Items.Batches.Matches
import ModelField
import Test.QuickCheck(property, (===), (==>), Arbitrary(..), arbitraryBoundedEnum)
import Import.NoFoundation

spec :: Spec
spec = parallel pureSpec 

instance Arbitrary MatchQuality where
  arbitrary = arbitraryBoundedEnum

identical = qualityToScore Identical
excellent = qualityToScore Excellent
good = qualityToScore Good
fair = qualityToScore Fair
close = qualityToScore Close
bad = qualityToScore Bad

shouldThrowPure x e = seq x (return ()) `shouldThrow` e


-- | Reset the score to the equivalent of its quality
-- ex 95 (Good) -> 97 (Good)
roundupScore :: BatchMatch -> BatchMatch
roundupScore BatchMatch{..} = BatchMatch{batchMatchScore=score,..}  where
  score = qualityToScore $ scoreToQuality batchMatchScore

shouldBeRoundedUp as bs = norm as `shouldBe` norm bs
  where norm = sortOn batchMatchKeys . map (normalizeBatchMatch . roundupScore)
pureSpec = do 
  describe "@Batches Matches" $ do
      let [b1, b2, b3, b4] = map BatchKey [1..4]
          day1 = fromGregorian 2019 04 01
          day2 = fromGregorian 2019 05 01
          docKey = DocumentKeyKey' 1
          nokey = DocumentKeyKey' 0
      describe "#mergeBatchMatches" $ do
        let mergeBatchMatches' :: BatchMergeMode -> Text -> [[(Text, MatchQuality)]] -> Maybe [(Text, MatchQuality)]
            mergeBatchMatches' mode sku qs =  batchToCol'qual <$$> mergeBatchMatches mode sku ss
                  where ss = col'qualToBatch <$$> qs :: [[BatchMatch]]
            col'qualToBatch :: (Text, MatchQuality) -> BatchMatch
            col'qualToBatch (col, qty) = BatchMatch{..} where
              batchMatchSource = b1
              batchMatchTarget = b2
              batchMatchSourceColour = "<undefined>" -- set if spec breaks
              batchMatchOperator = Nothing -- set if spec breaks
              batchMatchDate = day1 -- set if spec breaks
              batchMatchDocumentKey = nokey -- set if spec breaks
              batchMatchComment = Nothing
              batchMatchTargetColour = col
              batchMatchScore = qualityToScore qty
            batchToCol'qual :: BatchMatch -> (Text, MatchQuality)
            batchToCol'qual BatchMatch{..} = (batchMatchTargetColour, scoreToQuality batchMatchScore)
        describe "#MergeRaisesError" $ do
          it "works with only one batch" $ do
            mergeBatchMatches' MergeRaisesError "sku" [ [ ("A", Excellent) , ("B", Bad) ]
                                                     ]
              `shouldBe` Just [ ("A", Excellent), ("B", Bad) ]
          it "works with similiar batches" $ do
            mergeBatchMatches' MergeRaisesError "sku" [ [ ("A", Excellent) ] 
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
              mergeBatchMatches' SafeMatch "sku" [ [ ("A", Excellent) , ("B", Bad) ]
                                                      ]
                `shouldBe` Just [ ("A", Excellent), ("B", Bad) ]
            it "takes the lowest in case of intersection" $ do
              mergeBatchMatches' SafeMatch "sku" [ [ ("A", Excellent)]
                                                      , [ ("A", Good) ]
                                                      ]
                `shouldBe` Just [("A", Good)] 
            it "gets lowest  of the intersection" $ do
              mergeBatchMatches' SafeMatch "sku" [ [ ("FPK", Fair), ("FPK", Good)
                                                  , ("BGM", Close), ("BGM", Fair)
                                                  ]
                                                , [ ("CES", Close)
                                                  , ("BGM", Bad)
                                                  ]
                                                ]
                `shouldBe` Just [("BGM", Bad)] 
      describe "@Expand" $ do
        let 
            op = OperatorKey 1
            -- tweak expandMatche to set the comment
            expandMatches0 ms = map updateComment $ expandMatches scoreLimiter tshow ms where
              updateComment b = b {batchMatchComment = Just "<expanded>"}
            expandMatches1 ms = let
              new = expandMatches0 ms
              in expandMatches0 (ms <> new)
            scoreLimiter _ _ v  | v == qualityToScore Identical = Just v
            scoreLimiter a b v | a == b = mergeScores [min v (qualityToScore Close)]
            scoreLimiter _ _ v = Just v
            
        it "should connect 2 batches" $ do
          expandMatches0 
                        [ BatchMatch b1 "A" b2 "A" (Just op) excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) excellent Nothing day1 docKey
                        ]  `shouldBeRoundedUp`
                        [ BatchMatch b3 "A" b1 "A" Nothing excellent (Just "<expanded>") day1 nokey
                        ]
        it "should connect 2 batches in a different order" $ do
          expandMatches0 
                        [ BatchMatch b2 "A" b1 "A" (Just op) excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) excellent Nothing day1 docKey
                        ]  `shouldBeRoundedUp`
                        [ BatchMatch b3 "A" b1 "A" Nothing excellent (Just "<expanded>") day1 nokey
                        ]
        it "should connect 2 batches in a different order" $ do
          expandMatches0 
                        [ BatchMatch b2 "A" b1 "A" (Just op) excellent Nothing day1 docKey
                        , BatchMatch b2 "A" b3 "A" (Just op) excellent Nothing day1 docKey
                        ]  `shouldBeRoundedUp`
                        [ BatchMatch b3 "A" b1 "A" Nothing excellent (Just "<expanded>") day1 nokey
                        ]
        it "should connect 2 batches - last order" $ do
          expandMatches0 
                        [ BatchMatch b1 "A" b2 "A" (Just op) excellent Nothing day1 docKey
                        , BatchMatch b2 "A" b3 "A" (Just op) excellent Nothing day1 docKey
                        ]  `shouldBeRoundedUp`
                        [ BatchMatch b3 "A" b1 "A" Nothing excellent (Just "<expanded>") day1 nokey
                        ]
        it "should remove duplicate guesses " $ do
          expandMatches0 
                        [ BatchMatch b1 "A" b2 "A" (Just op) excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) excellent Nothing day1 docKey
                        , BatchMatch b1 "A" b3 "A" Nothing   good Nothing day1 docKey
                        ]  `shouldBeRoundedUp`
                        []
        context "#short path" $ do
          it "A +++ B +++ C +++ D ==> A ++ D" $ do
            expandMatches1 [ BatchMatch b1 "A" b2 "B" Nothing excellent Nothing day1 docKey
                           , BatchMatch b2 "B" b3 "C" Nothing excellent Nothing day1 docKey
                           , BatchMatch b3 "C" b4 "D" Nothing excellent Nothing day1 docKey
                           ] `shouldBeRoundedUp`
                           [ BatchMatch b1 "A" b4 "D" Nothing good (Just "<expanded>") day1 nokey]
          it "A +++ B +++ C +++ D | A ++ C  => B +++ D" $ do
            expandMatches0 [ BatchMatch b1 "A" b2 "B" Nothing excellent Nothing day1 docKey
                           , BatchMatch b2 "B" b3 "C" Nothing excellent Nothing day1 docKey
                           , BatchMatch b3 "C" b4 "D" Nothing excellent Nothing day1 docKey
                           -- add short cut
                           , BatchMatch b1 "A" b3 "C" Nothing fair Nothing day1 docKey
                           ] `shouldBeRoundedUp`
                           [ BatchMatch b2 "B" b4 "D" Nothing excellent (Just "<expanded>") day1 nokey
                           , BatchMatch b1 "A" b4 "D" Nothing fair (Just "<expanded>") day1 nokey
                           ]
          it "A +++ B +++ C +++ D | A ++ C  => A + D" $ do
            expandMatches1 [ BatchMatch b1 "A" b2 "B" Nothing excellent Nothing day1 docKey
                           , BatchMatch b2 "B" b3 "C" Nothing excellent Nothing day1 docKey
                           , BatchMatch b3 "C" b4 "D" Nothing excellent Nothing day1 docKey
                           -- add short cut
                           , BatchMatch b1 "A" b3 "C" Nothing fair Nothing day1 docKey
                           ] `shouldBeRoundedUp`
                           [ 
                            -- BatchMatch b1 "A" b4 "D" Nothing close (Just "<expanded>") day1 nokey
                           ]
        context "#use limiter " $ do
          it "A +++ B +++ A' +++ D  => A ~ A' | B +++ D" $ do
            expandMatches0 [ BatchMatch b1 "A" b2 "B" Nothing excellent Nothing day1 docKey
                           , BatchMatch b2 "B" b1 "A'" Nothing excellent Nothing day1 docKey -- limitation
                           , BatchMatch b1 "A'" b4 "D" Nothing excellent Nothing day1 docKey
                           ] `shouldBeRoundedUp`
                           [ BatchMatch b2 "B" b4 "D" Nothing excellent (Just "<expanded>") day1 nokey
                           , BatchMatch b1 "A" b1 "A'" Nothing  close (Just "<expanded>") day1 nokey -- limit to close
                           ]
          it "A +++ B +++ A' +++ D ==> A ~ D " $ do
            expandMatches1 [ BatchMatch b1 "A" b2 "B" Nothing excellent Nothing day1 docKey
                           , BatchMatch b2 "B" b1 "A'" Nothing excellent Nothing day1 docKey -- limitation
                           , BatchMatch b1 "A'" b4 "D" Nothing excellent Nothing day1 docKey
                           ] `shouldBeRoundedUp`
                           [ 
                            BatchMatch b1 "A" b4 "D" Nothing close (Just "<expanded>") day1 nokey
                           ]
                           -- already done in round 1. Shouldn't be overloaded

        it "should keeps the given ones" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" (Just op) excellent (Just "Via #3") day1 docKey
                              , BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day2 docKey
                        
                              ]) `shouldBeRoundedUp`
                              [ BatchMatch b1 "A" b2 "A" (Just op) excellent (Just "Via #3") day1 docKey
                              ]
        it "should keeps the given ones (reverse order)" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day1 docKey
                              , BatchMatch b1 "A" b2 "A" (Just op) excellent Nothing day2 docKey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" (Just op) excellent Nothing day2 docKey
                              ]
        it "should keep guess with the last doc key" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day1 nokey
                              , BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day2 docKey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day2 docKey
                              ]
        it "should keep guess with the last doc key (reverse order)" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day1 docKey
                              , BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day2 nokey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" Nothing excellent Nothing day1 docKey
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
          it "Good <> Good <> Fair => Close" $ do
             mergeQualities [Good , Good, Fair] `shouldBe` Just Close
          it "Fair <> Fair => Close " $ do
             mergeQualities [Fair , Fair] `shouldBe` Just Close
          it "Bad <> Close => 0 " $ property $ do
            \a -> (a <= Close) ==> mergeQualities [a, Bad] === Nothing
          it "Bad <> Anything => Bad " $ property $ do
            \a -> (a > Close) ==> mergeQualities [a, Bad] === Just Bad
          describe "#qToD" $ do
            it "quality to double is idempotan" $ property $ do
              \q -> dToQ (qToD q) === q
            it "Quality + 0.5 stays the same" $ property $ do
              \q -> dToQ (qToD q - 0.9 ) === q
               
      describe "#aggregateScore" $ do
        it "doesn't touch single elements" $ property $ do
          mode <- arbitraryBoundedEnum
          q <- arbitraryBoundedEnum
          let a = qualityToScore q
          return $ aggregateScore mode [("A", a)] === [("A", a)]
          
mergeQualities:: [MatchQuality] -> Maybe MatchQuality
mergeQualities qs = scoreToQuality <$> mergeScores (map qualityToScore qs)

qToD = unMatchScore . qualityToScore 
dToQ = scoreToQuality . MatchScore


  
