{-# LANGUAGE OverloadedStrings #-}
module Planner.InternalSpec where

import TestImport
import Planner.Types
import Planner.Internal
import Model.DocumentKey

spec :: Spec
spec = parallel pureSpec >> ioSpec


pureSpec :: Spec
pureSpec = describe "Parsing Scenario file @planner" $ do
  it "parses scenario without headings" $ do
    let text = unlines [ ":LAYOUT:"
                       , "A|B|C"
                       , ":MOVES:"
                       , "move 1"
                       , "move 2"
                       , ":END:"
                       ]

    parseScenarioFile text `shouldBe` Right [ Section LayoutH (Right ["A|B|C"]) ""
                                           , Section (MovesH []) (Right ["move 1", "move 2"]) ""
                                           ]
  it "parses scenario with headings" $ do
    let text = unlines [ "* Layout"
                       , ":LAYOUT:"
                       , "A|B|C"
                       , "* Initial Moves"
                       , ":MOVES:"
                       , "move 1"
                       , "move 2"
                       , ":END:"
                       ]

    parseScenarioFile text `shouldBe` Right [ Section TitleH (Right []) "* Layout"
                                            , Section LayoutH (Right ["A|B|C"]) ""
                                            , Section TitleH (Right []) "* Initial Moves"
                                            , Section (MovesH []) (Right ["move 1", "move 2"]) ""
                                            ]
  it "parses complex scenario"  $ do
    let text = unlines [ ":LAYOUT:"
                       , "A|B|C"
                       , ":MOVES:"
                       , "@sha1"
                       , "move 1"
                       , "move 2"
                       , "@sha2"
                       , "move 3"
                       , "move 4"
                       , ":END:"
                       ]

    parseScenarioFile text `shouldBe` Right [ Section LayoutH (Right ["A|B|C"]) ""
                                           , Section (MovesH []) (Left (DocumentHash "sha1")) ""
                                           , Section (MovesH []) (Right ["move 1", "move 2"]) ""
                                           , Section (MovesH []) (Left (DocumentHash "sha2")) ""
                                           , Section (MovesH []) (Right ["move 3", "move 4"]) ""
                                           ]

  it "rejects scenario with unknown section" $ do
    let text = unlines [ ":Layout:"
                       , "A|B|C"
                       , ":END:"
                       , ":Move:" -- <--- invalid : should be Moves
                       , "move 1"
                       , "move 2"
                       , ":END"
                       ]
    parseScenarioFile text `shouldBe` Left "Move is not a valid drawer."
  it "parse with or without :END: markers" $ do
    let without = unlines [ ":LAYOUT:"
                       , "C1"
                       , ":SHELVES:"
                       , "name,comment,length,width,height,type"
                       , "C1#_coming,coming,3800,100,1200,Shelf"
                   ]
        with = unlines [ ":LAYOUT:"
                       , "C1"
                       , ":END:"
                       , ":SHELVES:"
                       , "name,comment,length,width,height,type"
                       , "C1#_coming,coming,3800,100,1200,Shelf"
                       , ":END:"
                   ]
    parseScenarioFile with `shouldBe` parseScenarioFile without

ioSpec :: Spec
ioSpec = describe "Reading scenario @planner" $ do
  it "computes different SHA for different layout scenario" $ do
    let text = unlines [ ":Layout:"
                       , "A"
                       , ":Moves:"
                       , "move 1"
                       , "move 2"
                       , ":Moves:"
                       , "move 3"
                       , "move 4"
                       ]
    let text2 = unlines [ ":Layout:"
                       , "B" -- only layout changes
                       , ":Moves:"
                       , "move 1"
                       , "move 20" -- only changes
                       , ":Moves:"
                       , "move 3"
                       , "move 4"
                       ]

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario ( return . Right . (:[])) t
                               return $ scenarioKey `fmap` sc
                         )
                       [text, text2]
    sha1 `shouldNotBe` sha2

  it "computes same SHA for similar scenarios" $ do
    let text = unlines [ ":Layout:"
                       , "A|B|C"
                       , ":Moves:"
                       , "move 1"
                       , "move 2"
                       , ":Moves:"
                       , "move 3"
                       , "move 4"
                       ]
    let text2 = unlines [ ":Layout:"
                       , "A|B|C"
                       , ":Moves:"
                       , "move 1"
                       , "" -- only changes
                       , "move 2"
                       , ":Moves:"
                       , "move 3"
                       , "move 4"
                       ]

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario ( return . Right . (:[])) t
                               return $ scenarioKey `fmap` sc
                         )
                       [text, text2]
    -- length (rights [sha1, sha2]) `shouldBe` 2
    sha1 `shouldBe` sha2

  it "computes different SHA for different moves scenario" $ do
    let text = unlines [ ":Layout:"
                       , "A"
                       , ":Moves:"
                       , "move 1"
                       , "move 2"
                       , ":Moves:"
                       , "move 3"
                       , "move 4"
                       ]
    let text2 = unlines [ ":Layout:"
                       , "A"
                       , ":Moves:"
                       , "move 1"
                       , "move 20" -- Change
                       , ":Moves:"
                       , "move 3"
                       , "move 4"
                       ]

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario ( return . Right . (:[])) t
                               return $ scenarioKey `fmap` sc
                         )
                       [text, text2]
    -- length (rights [sha1, sha2]) `shouldBe` 2
    sha1 `shouldNotBe` sha2

  it "computes same SHA from nested scenario" $ do
    pendingWith "Not implementable right now"
    let part1 = unlines [":Moves:"
                        , "move 1"
                        , "move 2"
                        ]
    let part2 = unlines [":Moves:"
                        , "move 3"
                        , "move 4"
                        ]
    Right (DocumentHash shaPart1) <- do
                  sc <- readScenario ( return . Right . (:[])) part1
                  return $ scenarioKey `fmap` sc

    let text1 = part1 <> part2
        text2 = unlines [":Initial:"
                        , "@" ++ shaPart1] ++ part2

    [sha1, sha2] <- mapM ( \t -> do
                               sc <- readScenario ( return . Right . (:[])) t
                               return $ scenarioKey `fmap` sc
                         )
                       [text1, text2]
    sha1 `shouldBe` sha2
    -- text1 `shouldBe` text2

