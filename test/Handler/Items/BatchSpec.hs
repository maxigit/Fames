module Handler.Items.BatchSpec where

import TestImport
import Handler.Items.Batches
import Handler.Items.Batches.Matches
import ModelField

spec :: Spec
spec = parallel pureSpec 


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
            
        it "should connect 2 batches" $ do
          expandMatches [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        ]  `shouldBe`
                        [ BatchMatch b1 "A" b3 "A" (Just op) Good Nothing day1 nokey
                        ]
        it "should remove duplicate guesses " $ do
          expandMatches [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        , BatchMatch b3 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
                        , BatchMatch b1 "A" b3 "A" Nothing   Good Nothing day1 nokey
                        ]  `shouldBe`
                        [ BatchMatch b1 "A" b3 "A" Nothing Good Nothing day1 nokey
                        ]
        it "should keeps the given ones" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day1 docKey
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
        it "should keeps guess with the last doc key" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 nokey
                              , BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day2 docKey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" (Just op) Excellent Nothing day2 docKey
                              ]
        it "should keeps guess with the last doc key (reverse order)" $ do
          keepBests (SameKeys [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 docKey
                              , BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day2 nokey
                        
                              ]) `shouldBe`
                              [ BatchMatch b1 "A" b2 "A" Nothing Excellent Nothing day1 docKey
                              ]
