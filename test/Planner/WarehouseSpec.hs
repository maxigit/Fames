module Planner.WarehouseSpec (spec) where

import TestImport
import WarehousePlanner.Csv
import WarehousePlanner.Base

spec :: Spec
spec = parallel pureSpec 

pureSpec :: Spec
pureSpec = expandSpecs >> boxArrangements
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



