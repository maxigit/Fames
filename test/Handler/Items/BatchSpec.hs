
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
