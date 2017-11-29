{-# LANGUAGE OverloadedStrings #-}
module Planner.InternalSpec where

import TestImport
import Items.Internal
import Items.Types

import Planner.Types
import Planner.Internal
import Model.DocumentKey

spec :: Spec
spec = parallel pureSpec


pureSpec :: Spec
pureSpec = describe "Parsing Scenario file @planner" $ do
  it "parses complex scenario"  $ do
    let text = unlines [ "* Layout"
                       , "A|B|C"
                       , ""
                       , "*   Moves  "
                       , "@sha1"
                       , "move 1"
                       , "move 2"
                       , "@sha2"
                       , "move 3"
                       , "move 4"
                       ]

    parseScenarioFile text `shouldBe` Right [ Section LayoutH (Right ["A|B|C"])
                                           , Section MovesH (Left (DocumentHash "sha1"))
                                           , Section MovesH (Right ["move 1", "move 2"])
                                           , Section MovesH (Left (DocumentHash "sha2"))
                                           , Section MovesH (Right ["move 3", "move 4"])
                                           ]
