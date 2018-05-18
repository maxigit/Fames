{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ItemReportSpec where

import TestImport
import Handler.Items.Reports.Common hiding(sortAndLimit)
import Items.Types
import Data.List((!!))
import Data.Monoid(Sum(..))
import Control.Monad(zipWithM)

import qualified Data.Map as Map

spec :: Spec
spec = parallel pureSpec

-- * Helpers

getColour, getStyle, getPeriod :: Text -> Text
getColour = (!!0) . words
getStyle = (!!1) . words
getPeriod = (!!2) . words

columns :: [(Text, Text -> Text)]
columns = [ ("Colour", getColour) , ("Style", getStyle) , ("Period", getPeriod)]

mkGroup :: (Text, Text -> Text) -> (Maybe Text, Text -> NMapKey)
mkGroup (level, getter) = (Just level, mkNMapKey . PersistText . getter)

withPrices :: [(Text, Double, Double)] -> [(Text, QPrice)]
withPrices inputs = [(key, mkQPrice Outward qty (price / qty)) | (key, qty, price) <- inputs]

-- n :: [(Text, Double, Double)] -> NMap QPrice
n = groupAsNMap' columns

groupAsNMap' :: [(Text, Text -> Text)] -> [(Text, Double, Double)] -> NMap QPrice
groupAsNMap' groups inputs = groupAsNMap (map mkGroup groups) (withPrices inputs) 

pretty :: NMap QPrice -> [(Text, Double, Double)]
pretty grouped = [ (unwords (map (pvToText' . nkKey) keys), qty, amount)
                 | (keys, qprice) <- nmapToList grouped
                 , let qty = qpQty Outward qprice
                 , let amount = qpAmount Outward qprice
                 ]
pvToText' PersistNull = "<null>"
pvToText' pv = pvToText pv

-- shouldLookLike a b = pretty a `shouldBe` b
shouldLookLike a b = do
  let a' = pretty a
  zipWithM shouldBe a' b -- compare one by one
  a' `shouldBe` b -- and the all

sortAndLimit' inputs limits = sortAndLimit limits (n inputs)

-- to help type resolves constraints on empty list
type EmptySort = Maybe (NMapKey -> QPrice -> (), Maybe RankMode, Maybe Int)
-- * Specs
pureSpec :: Spec
pureSpec = describe "@Report @parallel @pure" $ do
  describe "grouping" $ do
    it "groups everything" $ do
      groupAsNMap' []
                   [ ("Red Shirt 3-Mar", 6, 18)
                   , ("Blue Shirt 1-Jan", 1, 3)
                   ]
      `shouldLookLike` [("", 7, 21)]
    it "groups by colour" $ do
      groupAsNMap' [("Colour", getColour)]
                   [ ("Red Shirt 3-Mar", 6, 18)
                   , ("Blue Shirt 1-Jan", 1, 3)
                   , ("Red Dress 1-Jan", 2, 50)
                   ]
      `shouldLookLike` [ ("Blue", 1, 3)
                       , ("Red", 8, 68)

                       ]
    it "groups by period" $ do
      groupAsNMap' [("Period", getPeriod)]
                   [ ("Red Shirt 3-Mar", 6, 18)
                   , ("Blue Shirt 1-Jan", 1, 3)
                   , ("Red Dress 1-Jan", 2, 50)
                   ]
      `shouldLookLike` [("1-Jan", 3, 53)
                       ,("3-Mar", 6, 18)
                       ]
    it "groups by colour and period" $ do
      groupAsNMap' [("Colour", getColour), ("Period", getPeriod)]
                   [ ("Red Shirt 3-Mar", 6, 18)
                   , ("Blue Shirt 1-Jan", 1, 3)
                   , ("Red Dress 3-Mar", 2, 50)
                   ]
      `shouldLookLike` [ ("Blue 1-Jan", 1, 3)
                       , ("Red 3-Mar", 8, 68)
                       ]

  describe "sortAndLimit" $ do
    let trans = [ ("Red Shirt 1-Jan", 10, 25)
                , ("Red Shirt 2-Feb", 2, 5)
                , ("Red Shirt 3-Mar", 6, 18)
                  
                , ("Blue Shirt 1-Jan", 1, 3)
                , ("Blue Shirt 2-Feb", 5, 15)
                , ("Blue Shirt 3-Mar", 15, 45)
                    
                , ("Blue Cap 1-Jan", 35,420)
                , ("Blue Cap 2-Feb", 28, 326)
                , ("Blue Cap 3-Mar", 15, 150)
                    
                , ("White Cap 1-Jan", 3,36)
                , ("White Cap 2-Feb", 8, 96)
                , ("White Cap 3-Mar", 5, 50)
                    
                , ("White Dress 1-Jan", 2,15)
                , ("White Dress 2-Feb", 8, 56)
                , ("White Dress 3-Mar", 25, 175)
                    
                ]
        salesAmount _ = qpAmount Inward -- reverse to we get top first
    it "should keep original (map) order' if there is nothing to" $ do
      sortAndLimit' trans ([] :: [EmptySort]) `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):


                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 2-Feb", 8, 96):
                 ("White Cap 3-Mar", 5, 50):
                 ("White Dress 1-Jan", 2,15):
                 ("White Dress 2-Feb", 8, 56):
                 ("White Dress 3-Mar", 25, 175):

                [])

    it "doesn't do anything if there is nothing to" $ do
      sortAndLimit' trans [Nothing :: EmptySort, Nothing] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):


                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 2-Feb", 8, 96):
                 ("White Cap 3-Mar", 5, 50):
                 ("White Dress 1-Jan", 2,15):
                 ("White Dress 2-Feb", 8, 56):
                 ("White Dress 3-Mar", 25, 175):

                [])
    it "limits the first level - ie two first colours" $ do
      sortAndLimit' trans [Just (id . const , Nothing, Just 2 )] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):


                 -- ("White Cap 1-Jan", 3,36):
                 -- ("White Cap 2-Feb", 8, 96):
                 -- ("White Cap 3-Mar", 5, 50):
                 -- ("White Dress 1-Jan", 2,15):
                 -- ("White Dress 2-Feb", 8, 56):
                 -- ("White Dress 3-Mar", 25, 175):

                [])
    it "limits the second level - ie the first style" $ do
      sortAndLimit' trans [Nothing, Just (id . const, Nothing, Just 1 )] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 -- ("Blue Shirt 1-Jan", 1, 3):
                 -- ("Blue Shirt 2-Feb", 5, 15):
                 -- ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):


                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 2-Feb", 8, 96):
                 ("White Cap 3-Mar", 5, 50):
                 -- ("White Dress 1-Jan", 2,15):
                 -- ("White Dress 2-Feb", 8, 56):
                 -- ("White Dress 3-Mar", 25, 175):

                [])
    it "limits the third level - ie two first months" $ do
      sortAndLimit' trans [Nothing, Nothing, Just (id . const, Nothing, Just 2 )] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 -- ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 -- ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 -- ("Red Shirt 3-Mar", 6, 18):


                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 2-Feb", 8, 96):
                 -- ("White Cap 3-Mar", 5, 50):
                 ("White Dress 1-Jan", 2,15):
                 ("White Dress 2-Feb", 8, 56):
                 -- ("White Dress 3-Mar", 25, 175):

                [])

    it "sorts the first level by sales amount" $ do
      -- blue first
      sortAndLimit' trans [Just (salesAmount, Nothing, Nothing)] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 2-Feb", 8, 96):
                 ("White Cap 3-Mar", 5, 50):
                 ("White Dress 1-Jan", 2,15):
                 ("White Dress 2-Feb", 8, 56):
                 ("White Dress 3-Mar", 25, 175):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):

                [])
    it "sorts the second level by sales amount" $ do
      -- blue first
      sortAndLimit' trans [Nothing, Just (salesAmount, Nothing, Nothing)] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):


                 ("White Dress 1-Jan", 2,15):
                 ("White Dress 2-Feb", 8, 56):
                 ("White Dress 3-Mar", 25, 175):
                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 2-Feb", 8, 96):
                 ("White Cap 3-Mar", 5, 50):

                [])
    it "sorts and limit third level by sales amount - ie top month" $ do
      -- blue first
      sortAndLimit' trans [Nothing, Nothing, Just (salesAmount, Nothing, Just 1)] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 -- ("Blue Cap 2-Feb", 28, 326):
                 -- ("Blue Cap 3-Mar", 15, 150):
                 -- ("Blue Shirt 1-Jan", 1, 3):
                 -- ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("Red Shirt 1-Jan", 10, 25):
                 -- ("Red Shirt 2-Feb", 2, 5):
                 -- ("Red Shirt 3-Mar", 6, 18):


                 -- ("White Dress 1-Jan", 2,15):
                 -- ("White Dress 2-Feb", 8, 56):
                 ("White Cap 2-Feb", 8, 96):
                 ("White Dress 3-Mar", 25, 175):
                 -- ("White Cap 1-Jan", 3,36):
                 -- ("White Cap 3-Mar", 5, 50):

                [])
    context "#residuals" $ do
      it "group the last level without creating an extar NMap level " $ do
        sortAndLimit' [("Blue Shirt 1-Jan", 2, 7)] [Nothing, Nothing, Just (salesAmount, Just RMResidual, Just 0)] `shouldLookLike`  [("Blue Shirt Last-1", 2, 7)]
      it "group first level but keep children " $ do
        -- first blue say unchanged
        -- everything else is aggregated but separated by color and period

        sortAndLimit' trans [Just (salesAmount, Just RMResidual, Just 1)] `shouldLookLike` 
              (
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt 3-Mar", 15, 45):

                 ("Last-2 Cap 1-Jan", 3,36):
                 ("Last-2 Cap 2-Feb", 8, 96):
                 ("Last-2 Cap 3-Mar", 5, 50):

                 ("Last-2 Dress 1-Jan", 2,15):
                 ("Last-2 Dress 2-Feb", 8, 56):
                 ("Last-2 Dress 3-Mar", 25, 175):

                 ("Last-2 Shirt 1-Jan", 10, 25):
                 ("Last-2 Shirt 2-Feb", 2, 5):
                 ("Last-2 Shirt 3-Mar", 6, 18):


                 []
              )

      it "group the snd level of residuals keeping third level" $ do
        sortAndLimit' trans [Nothing, Just (salesAmount, Just RMResidual, Just 0)] `shouldLookLike` 
                (
                 -- ("Blue Cap 1-Jan", 35,420):
                 -- ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Last-2 1-Jan", 36, 423):

                 -- ("Blue Cap 2-Feb", 28, 326):
                 -- ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Last-2 2-Feb", 33, 341):

                 -- ("Blue Cap 3-Mar", 15, 150):
                 -- ("Blue Shirt 3-Mar", 15, 45):
                 ("Blue Last-2 3-Mar", 30, 195):

                 -- ("Red Shirt 1-Jan", 10, 25):
                 ("Red Last-1 1-Jan", 10, 25):
                 -- ("Red Shirt 2-Feb", 2, 5):
                 ("Red Last-1 2-Feb", 2, 5):
                 -- ("Red Shirt 3-Mar", 6, 18):
                 ("Red Last-1 3-Mar", 6, 18):


                 -- ("White Dress 1-Jan", 2,15):
                 -- ("White Cap 1-Jan", 3,36):
                 ("White Last-2 1-Jan", 5,51):

                 -- ("White Cap 2-Feb", 8, 96):
                 -- ("White Dress 2-Feb", 8, 56):
                 ("White Last-2 2-Feb", 16, 152):

                 -- ("White Dress 3-Mar", 25, 175):
                 -- ("White Cap 3-Mar", 5, 50):
                 ("White Last-2 3-Mar", 30, 225):

                [])
      it "group the last level of residuals as one" $ do
        sortAndLimit' trans [Nothing, Nothing, Just (salesAmount, Just RMResidual, Just 1)] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,420):
                 -- ("Blue Cap 2-Feb", 28, 326):
                 -- ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Cap Last-2", 43,476):

                 ("Blue Shirt 3-Mar", 15, 45):
                 -- ("Blue Shirt 1-Jan", 1, 3):
                 -- ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt Last-2", 6, 18):

                 ("Red Shirt 1-Jan", 10, 25):
                 -- ("Red Shirt 2-Feb", 2, 5):
                 -- ("Red Shirt 3-Mar", 6, 18):
                 ("Red Shirt Last-2", 8, 23):


                 ("White Cap 2-Feb", 8, 96):
                 -- ("White Cap 1-Jan", 3,36):
                 -- ("White Cap 3-Mar", 5, 50):
                 ("White Cap Last-2", 8, 86):
                 ("White Dress 3-Mar", 25, 175):
                 -- ("White Dress 1-Jan", 2,15):
                 -- ("White Dress 2-Feb", 8, 56):
                 ("White Dress Last-2", 10, 71):

                [])
      it "group all level last level of residuals " $ do
        {- first group is Blue and everything else
                 ("Blue Cap 1-Jan", 35,420):
                 ("Blue Cap 2-Feb", 28, 326):
                 ("Blue Cap 3-Mar", 15, 150):
                 ("Blue Cap Last-2", 43,476):

                 ("Blue Shirt 3-Mar", 15, 45):
                 ("Blue Shirt 1-Jan", 1, 3):
                 ("Blue Shirt 2-Feb", 5, 15):
                 ("Blue Shirt Last-2", 6, 18):


                 ("Red Shirt 1-Jan", 10, 25):
                 ("Red Shirt 2-Feb", 2, 5):
                 ("Red Shirt 3-Mar", 6, 18):
                 ("Red Shirt Last-2", 8, 23):


                 ("White Cap 2-Feb", 8, 96):
                 ("White Cap 1-Jan", 3,36):
                 ("White Cap 3-Mar", 5, 50):
                 ("White Cap Last-2", 8, 86):
                 ("White Dress 3-Mar", 25, 175):
                 ("White Dress 1-Jan", 2,15):
                 ("White Dress 2-Feb", 8, 56):
                 ("White Dress Last-2", 10, 71):
          -}
        sortAndLimit' trans (replicate 3 (Just (salesAmount, Just RMResidual, Just 1))) `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan",35.0,420.0):
                 ("Blue Cap Last-2",43.0,476.0):
                 ---------------------
                 ("Blue Last-1 3-Mar",15.0,45.0):
                 ("Blue Last-1 Last-2",6.0,18.0):

                 ("Last-2 Dress 3-Mar",25.0,175.0):
                 ("Last-2 Dress Last-2",10.0,71.0):
                 ---------------
                 ("Last-2 Last-2 2-Feb",10.0,101.0):
                 ("Last-2 Last-2 Last-2",24.0,129.0):
                 [])


    
