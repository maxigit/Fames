{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ItemReportSpec where

import TestImport
import Handler.Items.Reports.Common
import Items.Types
import Data.List((!!))

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
f = n                   [ ("Red Shirt Mar", 6, 18)
                   , ("Blue Shirt Jan", 1, 3)
                   ]
-- * Specs
pureSpec :: Spec
pureSpec = describe "@Report @parallel @pure" $ do
  describe "grouping" $ do
    it "groups everything" $ do
      groupAsNMap' []
                   [ ("Red Shirt Mar", 6, 18)
                   , ("Blue Shirt Jan", 1, 3)
                   ]
      `shouldLookLike` [("", 7, 21)]
    it "groups by colour" $ do
      groupAsNMap' [("Colour", getColour)]
                   [ ("Red Shirt Mar", 6, 18)
                   , ("Blue Shirt Jan", 1, 3)
                   , ("Red Dress Jan", 2, 50)
                   ]
      `shouldLookLike` [ ("Blue", 1, 3)
                       , ("Red", 8, 68)

                       ]
    it "groups by period" $ do
      groupAsNMap' [("Period", getPeriod)]
                   [ ("Red Shirt Mar", 6, 18)
                   , ("Blue Shirt Jan", 1, 3)
                   , ("Red Dress Jan", 2, 50)
                   ]
      `shouldLookLike` [("Jan", 3, 53)
                       ,("Mar", 6, 18)
                       ]
    it "groups by colour and period" $ do
      groupAsNMap' [("Colour", getColour), ("Period", getPeriod)]
                   [ ("Red Shirt Mar", 6, 18)
                   , ("Blue Shirt Jan", 1, 3)
                   , ("Red Dress Mar", 2, 50)
                   ]
      `shouldLookLike` [ ("Blue Jan", 1, 3)
                       , ("Red Mar", 8, 68)
                       ]

  -- describe "sortAndLimit" $ do
  --   it "collapse first level but keep third" $ do
  --     sortAndLimit' [ ("Red Shirt Jan", 10, 25)
  --                   , ("Red Shirt Feb", 2, 5)
  --                   , ("Red Shirt Mar", 6, 18)
                    
  --                   , ("Blue Shirt Jan", 1, 3)
  --                   , ("Blue Shirt Feb", 5, 15)
  --                   , ("Blue Shirt Mar", 15, 45)
                    
  --                   , ("Blue Cap Jan", 35,35*12)
  --                   , ("Blue Cap Feb", 28, 28*12)
  --                   , ("Blue Cap Mar", 15, 150)
                    
  --                   , ("White Cap Jan", 3,36)
  --                   , ("White Cap Feb", 8, 96)
  --                   , ("White Cap Mar", 5, 50)
                    
  --                   , ("White Dress Jan", 2,15)
  --                   , ("White Dress Feb", 8, 56)
  --                   , ("White Dress Mar", 25, 175)
                    
  --                   ]
  --       [ (getColour, Just RMBestAndRes, Just 1 )
  --       , (getPeriod, Nothing, Nothing)
  --       ]
  --       `shouldLookLike` sort [ ("Red Shirt Jan", 10, 2.5)
  --                  , ("Red Shirt Feb", 2, 2.5)
  --                  , ("Red Shirt Mar", 6, 2.5)
                   
  --                  , ("Blue Shirt Jan", 1, 3)
  --                  , ("Blue Shirt Feb", 5, 3)
  --                  , ("Blue Shirt Mar", 15, 3)
                   
  --                  , ("Blue Cap Jan", 35,12)
  --                  , ("Blue Cap Feb", 28, 12)
  --                  , ("Blue Cap Mar", 15, 10)
                   
  --                  , ("White Cap Jan", 3,12)
  --                  , ("White Cap Feb", 8, 12)
  --                  , ("White Cap Mar", 5, 10)
                   
  --                  , ("White Dress Jan", 2,7.5)
  --                  , ("White Dress Feb", 8, 7.5)
  --                  , ("White Dress Mar", 25, 7.5)
  --                  ]
                   
                   
                   
         
