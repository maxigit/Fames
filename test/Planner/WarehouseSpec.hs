{-# Language BlockArguments #-}
module Planner.WarehouseSpec (spec) where

import TestImport
import WarehousePlanner.Csv
import WarehousePlanner.Base
import Planner.Internal

spec :: Spec
spec = parallel pureSpec 

pureSpec :: Spec
pureSpec = do
  expandSpecs 
  boxArrangements 
  expandAttributes
  affDimensions
expandSpecs = describe "Expand" $ do
  it "exands brackets" $ do
    expand "a[12]" `shouldBe` [("a1", Nothing), ("a2", Nothing)]

  it "add tags" $ do
    expand "a[12#tag]" `shouldBe` [("a1", Nothing), ("a2", Just "tag")]

  it "append tags" $ do
    expand "a[12#tag]#old" `shouldBe` [("a1", Just "old"), ("a2", Just "tag#old")]
  it "expands multiple times with tags" $ do
    expand "a[12#two][XY#boo]#old" `shouldBe` [("a1X", Just "old")
                                              , ("a1Y", Just "boo#old")
                                              , ("a2X", Just "two#old")
                                              , ("a2Y", Just "two#boo#old")
                                              ]

boxArrangements = describe "boxArrangements @F" $ do
  it "finds standard arrangement" $ do
    howMany (Dimension 33 1 36) 
            (Dimension 33 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (HowMany 6 3 1 2)
  it "finds complex arrangement in square"  $ do
    --    1477
    --    14 8
    --    2558
    --    2 69
    --    3369
    howManyWithDiagonal (Dimension 33 1 36) 
                        (Dimension 33 1 36) 
            (Dimension 10 1 13)
            `shouldBe` Diagonal (HowMany 9 3 1 3) 3
  it "finds complex arrangement - not big enough" $ do
    howManyWithDiagonal (Dimension 32 1 36) 
                        (Dimension 32 1 36) 
            (Dimension 10 1 13)
            `shouldBe` Regular (HowMany 6 3 1 2)
  it "finds complex arrangement 2 square" $ do
    --    1477adgg
    --    14 8ad h
    --    2558beeh
    --    2 69b fi
    --    3369ccfi
    howManyWithDiagonal (Dimension 66 1 36) 
                        (Dimension 66 1 36) 
            (Dimension 10 1 13)
            `shouldBe` Diagonal (HowMany 18 6 1 3) 3


shouldExpandTo attribute expected = do
  let shelfDim = Dimension 200 100 200
      today = fromGregorian 2023 07 10
  val <- execWH (emptyWarehouse today)$ do
    a <- newShelf "Shelf S" Nothing shelfDim shelfDim 0 DefaultOrientation ColumnFirst
    _ <- newBox "Box A" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=V"]
    box <- newBox "Box B" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=X"]
    _ <- newBox "Box C" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=Y"]
    expandAttribute box 1 attribute
  val `shouldBe` expected
expandAttributes = describe "expand box attributes" $ do
  describe "expands" do
      it  "one attribute" do
          "prefix-${shelfname}-suffix"  `shouldExpandTo` "prefix-Shelf S-suffix"
      it "two attributes" do
          "${boxname} in ${shelfname}"  `shouldExpandTo` "Box B in Shelf S"
      it "nothing" do
          "" `shouldExpandTo` ""
      it "tags" do
          "tag value $[tag]." `shouldExpandTo` "tag value X."
      it "statistics" do
          "rank = $rank[tag]." `shouldExpandTo` "rank = 2."
      it "statistics with modulo" do
          "min rank = $rank-1[tag]." `shouldExpandTo` "min rank = 1."
      it "keeps $$" do
         "^full $$match$" `shouldExpandTo` "^full $match$"
      it "keeps { }" do
         "{I am between curly}" `shouldExpandTo` "{I am between curly}"
      it "keeps after { }" do
         "${shelfname} {I am between curly}" `shouldExpandTo` "Shelf S {I am between curly}"
      it "keeps prop { }" do
         "{ ${shelfname} is between curly}" `shouldExpandTo` "{ Shelf S is between curly}"
      it "keeps prop { }" do
         "{ $[tag] is between curly}" `shouldExpandTo` "{ X is between curly}"


  -- empty
  -- existing tags
  -- statistics
  -- $$
  -- 
  -- 

---------- * AffDimension

affDimensions :: Spec
affDimensions = do
  describe "@focus AffDimensions" do
   context "inAffDimension" do
     let aff = AffDimension bottomLeft topRight
         bottomLeft = Dimension 10 20 30
         topRight = Dimension 20 30 50 
     it "True in the middle" do
        inAffDimension (Dimension 15 25 40) aff `shouldBe` True
     it "True in the left edge" do
        inAffDimension (Dimension 10 25 40) aff `shouldBe` True
     it "False in the right edge" do
        inAffDimension (Dimension 15 25 50) aff `shouldBe` False
     it "False outside" do
        inAffDimension (Dimension 35 25 40) aff `shouldBe` False
   context "overlap" do
     --         
     --           C       D
     --                       N
     --       P       M
     --         
     --    O      A       B
     let o = Dimension 0 0 0
         p = Dimension 5 5 1
         a = Dimension 10 0 0
         b = Dimension 20 0 0
         c = Dimension 10 30 10
         d = Dimension 20 30 10
         m = Dimension 15 15 5
         n = Dimension 25 15 7
     context "intersection" do
       it "OC & OP == OP" do
          affDimensionIntersection (AffDimension o c) (AffDimension p c) `shouldBe` Just (AffDimension p c)
       it "OM & AC" do
          affDimensionIntersection (AffDimension o m) (AffDimension a d) `shouldBe` Just (AffDimension a (Dimension 15 15 5))
     it "overlaps" do
       affDimensionOverlap (AffDimension o m) (AffDimension a d) `shouldBe` True
     it "... reversed" do
       affDimensionOverlap (AffDimension a d) (AffDimension o m) `shouldBe` True
     it "doesn't do" do
       affDimensionOverlap (AffDimension o c) (AffDimension m d) `shouldBe` False
       affDimensionOverlap (AffDimension m d) (AffDimension o c) `shouldBe` False
     it "doesn't if edge touches" do
        affDimensionOverlap (AffDimension o c) (AffDimension a m) `shouldBe` False
        affDimensionOverlap (AffDimension a m) (AffDimension o c) `shouldBe` False
     it "True for crosses" do
        affDimensionOverlap (AffDimension p n) (AffDimension a d) `shouldBe` True
        affDimensionOverlap (AffDimension a d) (AffDimension p n) `shouldBe` True
      

