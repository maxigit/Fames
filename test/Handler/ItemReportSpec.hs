{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ItemReportSpec where

import TestImport
import Handler.Items.Reports.Common hiding(sortAndLimit)
import Items.Types
import Data.List((!!))
import Data.Monoid(Sum(..))

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
pretty grouped = [ (unwords (map (pvToText . nkKey) keys), qty, amount)
                 | (keys, qprice) <- nmapToList grouped
                 , let qty = qpQty Outward qprice
                 , let amount = qpAmount Outward qprice
                 ]

shouldLookLike a b = pretty a `shouldBe` b
f = n                   [ ("Red Shirt 3-Mar", 6, 18)
                   , ("Blue Shirt 1-Jan", 1, 3)
                   ]
sortAndLimit :: (Monoid r, Ord r) => [Maybe (a -> r, Maybe RankMode, Maybe Int)]  -> NMap a -> NMap a
sortAndLimit _ inputs = inputs
sortAndLimit' inputs limits = sortAndLimit (map (fmap summize) limits) (n inputs)
  where summize (fn, rank, limit) = (Sum . fn, rank, limit)
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
                    
                , ("Blue Cap 1-Jan", 35,35*12)
                , ("Blue Cap 2-Feb", 28, 28*12)
                , ("Blue Cap 3-Mar", 15, 150)
                    
                , ("White Cap 1-Jan", 3,36)
                , ("White Cap 2-Feb", 8, 96)
                , ("White Cap 3-Mar", 5, 50)
                    
                , ("White Dress 1-Jan", 2,15)
                , ("White Dress 2-Feb", 8, 56)
                , ("White Dress 3-Mar", 25, 175)
                    
                ]
    it "should keep original (map) order' if there is nothing to" $ do
      sortAndLimit' trans [] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,35*12):
                 ("Blue Cap 2-Feb", 28, 28*12):
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
      sortAndLimit' trans [Nothing, Nothing] `shouldBe` n trans

    it "sort the first level by sales amount" $ do
      -- blue first
      sortAndLimit' trans [Just (qpAmount Outward, Nothing, Nothing)] `shouldLookLike` 
                (
                 ("Blue Cap 1-Jan", 35,35*12):
                 ("Blue Cap 2-Feb", 28, 28*12):
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
