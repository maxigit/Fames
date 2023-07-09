module Planner.WarehouseSpec (spec) where

import TestImport
import WarehousePlanner.Csv
import WarehousePlanner.Base
import Planner.Internal

spec :: Spec
spec = parallel pureSpec 

pureSpec :: Spec
pureSpec = expandSpecs >> boxArrangements >> expandAttributes
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
            (Dimension 10 1 13)
            `shouldBe` (3, 1, 2)
  it "finds complex arrangement in square"  $ do
    --    1477
    --    14 8
    --    2558
    --    2 69
    --    3369
    howManyWithDiagonal (Dimension 33 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (3, 1, 3)
  it "finds complex arrangement - not big enough" $ do
    howManyWithDiagonal (Dimension 32 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (3, 1, 2)
  it "finds complex arrangement 2 square" $ do
    --    1477adgg
    --    14 8ad h
    --    2558beeh
    --    2 69b fi
    --    3369ccfi
    howManyWithDiagonal (Dimension 66 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (6, 1, 3)
  it "finds complex arrangement 2 square - no big enough" $ do
    --    1477adgg
    --    14 8ad h
    --    2558beeh
    --    2 69b fi
    --    3369ccfi
    howManyWithDiagonal (Dimension 65 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (6, 1, 2)
  it "finds complex arrangement 11/3 square" $ do
    --    1477ad |gg
    --    14 8ad | h
    --    2558bee|h
    --    2 69b  |fi
    --    3369c  |cfi
    howManyWithDiagonal (Dimension 46 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (4, 1, 3)
  it "finds complex arrangement 11/3 square - not big enough" $ do
    howManyWithDiagonal (Dimension 45 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (4, 1, 2)



shouldExpandTo attribute expected = do
  let shelfDim = Dimension 200 100 200
      today = fromGregorian 2023 07 10
  val <- execWH (emptyWarehouse today)$ do
    a <- newShelf "Shelf S" Nothing shelfDim shelfDim 0 DefaultOrientation ColumnFirst
    _ <- newBox "Box A" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=V"]
    box <- newBox "Box B" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=X"]
    _ <- newBox "Box C" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=Y"]
    expandAttribute box attribute
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

  -- empty
  -- existing tags
  -- statistics
  -- $$
