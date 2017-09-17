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
  describe "#groupByStyle" $ do
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
  describe "#utils" $ do
    context "works" $ do
      it "holes" $ do
        holes [1,2,3] `shouldBe` [(1, [2,3]), (2, [1,3]), (3, [1,2])]
    context "#divUp" $ do
      it "divides" $ do
        (2 `divUp` 2) `shouldBe` 1
      it "divides but round up" $ do
        (3 `divUp` 2) `shouldBe` 2
      it "divides but round up" $ do
        (4 `divUp` 2) `shouldBe` 2
      it "divides 0" $ do
        (0 `divUp` 2) `shouldBe` 0
  describe "#slice" $ do
    let box = Box dim1 "A"
        zone = Zone "Z1" (Dimension 70  160 200 ) []  -- fix 2 4 3
    it "fits" $ do
      slice (box 11)  zone `shouldBe` ( Just $ Slice (box 11) 1 4 3 31 (4*34)
                                      , Nothing
                                      )
    it "doesn't fit" $ do
      let slices = [ Slice (box 24) 2 4 3 62 136]
      slice (box 15)  zone {zoneSlices = slices} `shouldBe` ( Nothing, Just $ box 15)
    it "overflow" $ do
      slice (box 25)  zone `shouldBe` ( Just $ Slice (box 24) 2 4 3 (2*31) (4*34)
                                      , Just $ box 1
                                      )

  describe "#tryFitOne" $ do
    it "rejects if too many row" $ do
      tryFitOne (Box dim1 "A" 100) (Zone "Z1" (Dimension 200 200 200 ) [] ) `shouldBe` Nothing
    it "reject if not enough space too wide" $ do
      tryFitOne (Box dim1 "A" 4) (Zone "Z1" (Dimension 200 200 200 )
                                       [ Slice {slLength = 100}
                                       , Slice {slLength = 80}
                                       ] ) `shouldBe` Nothing
    it "returns correct slice" $ do
      let box = Box dim1 "A" 4
          zone = Zone "Z1" (Dimension 200 200 200 ) [] 
          slice  = Slice box 1 2 4 31 34
      tryFitOne (Box dim1 "A" 4) (Zone "Z1" (Dimension 200 200 200 ) [] ) `shouldBe`
          Just zone { zoneSlices = [slice]}
  


