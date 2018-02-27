{-# LANGUAGE OverloadedStrings #-}
module Planner.InternalSpec where

import TestImport
import Items.Internal
import Items.Types

import Planner.Types
import Planner.Internal
import Model.DocumentKey

spec :: Spec
spec = parallel pureSpec >> ioSpec


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

    parseScenarioFile text `shouldBe` Right [ Section LayoutH (Right ["A|B|C"]) "* Layout"
                                           , Section MovesH (Left (DocumentHash "sha1")) "*   Moves"
                                           , Section MovesH (Right ["move 1", "move 2"]) ""
                                           , Section MovesH (Left (DocumentHash "sha2")) ""
                                           , Section MovesH (Right ["move 3", "move 4"]) ""
                                           ]


ioSpec :: Spec
ioSpec = describe "Reading scenario @planner" $ do
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

    parseScenarioFile text `shouldBe` Right [ Section LayoutH (Right ["A|B|C"]) ""
                                           , Section MovesH (Left (DocumentHash "sha1")) ""
                                           , Section MovesH (Right ["move 1", "move 2"]) ""
                                           , Section MovesH (Left (DocumentHash "sha2")) ""
                                           , Section MovesH (Right ["move 3", "move 4"]) ""
                                           ]

  it "computes different SHA for different layout scenario" $ do
    let text = unlines [ "* Layout"
                       , "A"
                       , "*   Moves  "
                       , "move 1"
                       , "move 2"
                       , "*   Moves  "
                       , "move 3"
                       , "move 4"
                       ]
    let text2 = unlines [ "* Layout"
                       , "B" -- only layout changes
                       , "*   Moves  "
                       , "move 1"
                       , "move 20" -- only changes
                       , "*   Moves  "
                       , "move 3"
                       , "move 4"
                       ]

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario t
                               return $ scenarioKey `fmap` sc
                         )
                       [text, text2]
    sha1 `shouldNotBe` sha2

  it "computes same SHA for similar scenarios" $ do
    let text = unlines [ "* Layout"
                       , "A|B|C"
                       , "*   Moves  "
                       , "move 1"
                       , "move 2"
                       , "*   Moves  "
                       , "move 3"
                       , "move 4"
                       ]
    let text2 = unlines [ "* Layout"
                       , "A|B|C"
                       , "*   Moves  "
                       , "move 1"
                       , "" -- only changes
                       , "move 2"
                       , "*   Moves  "
                       , "move 3"
                       , "move 4"
                       ]

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario t
                               return $ scenarioKey `fmap` sc
                         )
                       [text, text2]
    -- length (rights [sha1, sha2]) `shouldBe` 2
    sha1 `shouldBe` sha2

  it "computes different SHA for different moves scenario" $ do
    let text = unlines [ "* Layout"
                       , "A"
                       , "*   Moves  "
                       , "move 1"
                       , "move 2"
                       , "*   Moves  "
                       , "move 3"
                       , "move 4"
                       ]
    let text2 = unlines [ "* Layout"
                       , "A"
                       , "*   Moves  "
                       , "move 1"
                       , "move 20" -- Change
                       , "*   Moves  "
                       , "move 3"
                       , "move 4"
                       ]

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario t
                               return $ scenarioKey `fmap` sc
                         )
                       [text, text2]
    -- length (rights [sha1, sha2]) `shouldBe` 2
    sha1 `shouldNotBe` sha2

