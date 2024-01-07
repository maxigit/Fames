module Planner.RearrangeSpec (spec) where

import TestImport
import WarehousePlanner.Rearrange
import Data.Char (isLower)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

spec :: Spec
spec = parallel pureSpec

-- {{{1
pureSpec :: Spec
pureSpec = describe "ShiftUsed" do
   it "put inactive at the end" do
      testShiftUsed "A  b  c  d  "
                 "b* c* d* A* "

   it "put inactive at the end" do
      testShiftUsed "a  B  c  d  "
                 "a  c* d* B* "

   it "doesn't do anything if no dead" do
      testShiftUsed "a  b  c  d  "
                 "a  b  c  d  "

   >> describe "ShiftUsed_with_strategies" do
     it "shifts all" do
        testShiftUsedStrat "A  b  |_c  d  |_e  f  "
                        "b* c* |_d* e* |_f* A* "

     it "keeps b" do
        testShiftUsedStrat "!A  b  |_c  d  |_e  f  "
                        "!c* b  |_d* e* |_f* A* "

     it "keeps d" do
        testShiftUsedStrat "A  b  |!c  d  |e  f  "
                        "b* c* |!e* d  |f* A* "

     it "keeps most of all" do
        testShiftUsedStrat "!A  b  |!c  d  |!e  f  "
                        "!c* b  |!e* d  |!A* f  "

     it "keeps most of all (combined)" do
        testShiftUsedStrat "!A  b  c  d  |!e  f  "
                        "!e* b  c  d  |!A* f  "

     it "works on complex example" do
        testShiftUsedStrat "!a  b  C  d  |!E  f  G  h  |_i  j  k  l  "
        --                a  b  d  f    h  i  j  k    l  C  E G 
                        "!a  b  f* d  |!i* j* k* h  |_l* C* E* G* "

     it "works on complex example 2" do
        testShiftUsedStrat "_a  b  C  d  |!E  f  G  h  |_i  j  k  l  "
        --                a  b  d  f    h  i  j  k    l  C  E  G
                        "_a  b  d* f* |!i* j* k* h  |_l* C* E* G* "
        


-- Helper {{{2

testShiftUsed initial final = do
  let swaps = shiftUsed isLower (concat $ map fst $  mkBuckets initial)
  validateSwaps swaps
  applySwaps swaps initial `shouldBe` final

-- | Test rotation using the following format
-- abc|dee|!stay
testShiftUsedStrat initial final = do
 let swaps = shiftUsedInBucketsWithStrategy isLower (mkBuckets initial) :: [(Char, Char)]
 validateSwaps swaps
 applySwaps swaps initial `shouldBe` final
     
 

mkBuckets :: String-> [(String, ShiftStrategy)]
mkBuckets xs = let
  withStrategy s = case filter (/= ' ') s of
                   '!' : xs -> (xs, StayInPlace)
                   '_' : xs -> (xs, ShiftAll)
                   xs -> (xs, ShiftAll)
  in map withStrategy $ splitOn "|" xs
  
applySwaps :: [(Char, Char)] -> String -> String
applySwaps swaps xs = let
  -- we need to reverse the map because
  -- swaps are given source -> dest
  -- but when applying the swap we need to know 
  -- for a given slot (the dest) what is going to it.
  destToSource = Map.fromList $ map swap swaps
  replace c | c `elem` ['!','_','|'] = c:""
  replace c = case Map.lookup c destToSource of
                 Just new | new /= c -> new : "* "
                 _                   -> c   : "  "
  in concatMap replace (filter (/= ' ') xs)
  
  -- | Make sure that all item in source and destination
  -- are the same, i.e. there are not leaks
validateSwaps  swaps = do
   let (xs, ys) = unzip swaps
   sort xs `shouldBe` sort ys
               
