module SharedStockAdjustment where

import Prelude


data OriginalQuantities = OriginalQuantities
  { qtake :: Int -- quantity found during the stocktake
  , qoh :: Int -- quantity on hand (in FA)
  , qlost :: Int -- quantity on hand in the lost location. Can be found
  , qModulo :: Maybe Int -- limit the adjustment to modulo, to take into account
  -- full box missing during the stocktake (or partial stocktakes)
  } deriving (Show, Eq)

data BadgeQuantities = BadgeQuantities
 { bMissing, bMissingMod, bFound, bFoundMod, bNew :: Int }
 deriving (Show, Eq)

computeBadges :: OriginalQuantities -> BadgeQuantities
computeBadges (OriginalQuantities qtake qoh qlost Nothing) = let
  missing = max 0 (qoh - qtake)
  tomany = max 0 (qtake - qoh)
  found = max 0 $ min qlost tomany
  new = tomany - found
  in BadgeQuantities missing 0 found 0 new
computeBadges o@(OriginalQuantities _ _ _ (Just 0)) = computeBadges o {qModulo = Nothing}
computeBadges o@(OriginalQuantities qtake qoh _ (Just modulo)) = let
  -- modulo represent the number of item per boxes in case boxes hasn't been stocktaked
  totalQoh = qoh -- + (max 2 qlost) -- we ignore at the moment where is what. 
  missing = totalQoh -qtake -- quantity missed from the stock take
  halfMod = (modulo  + 1) `div` 2 --half rounded up
  -- ^ we need negative or positive value centered around 0
  normal = computeBadges o {qModulo = Nothing}
  -- if everything can be found, no need for modulo
  in if bFound normal == -missing && (missing < 0) && bNew normal == 0
     then  normal
     else case compare missing 0 of
        EQ -> computeBadges o {qModulo = Nothing}
        GT -> -- missing stock takes
            -- we create some fake stocktakes
            let adjust = ((missing + halfMod-1) `div` modulo) * modulo
                badges = computeBadges o {qtake = qtake + adjust, qModulo = Nothing}
                in  badges {bMissingMod = adjust}
        LT -> -- missing in QOH
            -- we create some fake QOH
            let adjust = ((halfMod-missing) `div` modulo) * modulo
                badges = computeBadges o {qoh = qoh + adjust, qModulo = Nothing}
                in badges {bFoundMod = adjust}
    



badgeWidth :: Int -> Maybe Int
badgeWidth q | q <= 0 = Nothing
             | otherwise = Just (min (succ q) 9)

  


 
