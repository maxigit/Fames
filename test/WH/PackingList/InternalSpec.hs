{-# LANGUAGE OverloadedStrings #-}
module WH.PackingList.InternalSpec where

import TestImport
import WH.PackingList.Internal


spec :: Spec
spec = parallel $ pureSpec

dim1 = Dimension 31 34 78
dim2 = Dimension 60 40 50
dim1' = Dimension 32 34 78


pureSpec  = do
  describe "groupByStyle" $ do
    it "aggregate quantites" $ do
      groupByStyle [(Box dim1 "A" 10), (Box dim1 "A" 3)] `shouldBe` mapFromList [("A", Box dim1 "A" 13)]
    it "split different styles" $ do
      groupByStyle [(Box dim1 "A" 10), (Box dim1 "B" 3), (Box dim1 "A" 2)]
        `shouldBe` mapFromList [("A", Box dim1 "A" 12), ("B", Box dim1 "B" 3)]
    it "keep most numerous boxes" $ do
      groupByStyle [(Box dim1 "A" 10), (Box dim2 "A" 43)] `shouldBe` mapFromList [("A", Box dim2 "A" 53)]
    it "group everything" $ do
      groupByStyle [(Box dim1 "A" 10), (Box dim1' "A" 3), (Box dim2 "B" 7)]
      `shouldBe` mapFromList [("A", Box dim1 "A" 13), ("B", Box dim2 "B" 7 )]
  describe "holes" $ do
    it "works" $ do
      holes [1,2,3] `shouldBe` [(1, [2,3]), (2, [1,3]), (3, [1,2])]

  

