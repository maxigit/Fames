module SharedStockAdjustment where

import Prelude
import Data.Data


data OriginalQuantities = OriginalQuantities
  { qtake :: Int -- quantity found during the stocktake
  , qoh :: Int -- quantity on hand (in FA)
  , qlost :: Int -- quantity on hand in the lost location. Can be found
  , qModulo :: Maybe Int -- limit the adjustment to modulo, to take into account
  -- full box missing during the stocktake (or partial stocktakes)
  }

data BadgeQuantities = BadgeQuantities
 { bMissing, bMissingMod, bFound, bFoundMod, bNew :: Int }

computeBadges :: OriginalQuantities -> BadgeQuantities
computeBadges (OriginalQuantities qtake qoh qlost qModulo) = let
  missing = qoh - qtake
  tomany = -missing
  found = min qlost (-tomany)
  new = tomany - found
  in BadgeQuantities missing 0 found 0 new

badgeWidth :: Int -> Maybe Int
badgeWidth q | q <= 0 = Nothing
             | otherwise = Just (min (succ q) 9)

  


 
