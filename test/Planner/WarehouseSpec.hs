module Planner.WarehouseSpec where

import TestImport
import WarehousePlanner.Csv

spec :: Spec
spec = parallel pureSpec 

pureSpec :: Spec
pureSpec = describe "Expand @F" $ do
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

