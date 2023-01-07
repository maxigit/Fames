-- | Function to find (or at least) optimal  arrangement
module WarehousePlanner.Optimum 
(bestShelves, Ranking(..), usedRatio)
where
import Prelude
import WarehousePlanner.Base
import Data.List(sortBy)
import Data.Function(on)

data Ranking = Rank3D 
             | Rank2D -- ^ ignores the depth when sorting and reporting % of use. Handy if we know we can adjust the depth of a shelf.
-- | find shelves allowing the less waste
-- if it were filled with the same box
bestShelves :: Ranking -> Box s -> (Shelf s -> [OrientationStrategy]) -> [Shelf s] ->  [Shelf s]
bestShelves rankingMode box ors ss  = let
    tries = [ (- (usedRatio rankingMode box {orientation = or} s n k m), s)
            | s <- ss
            , let (or,_,n,k,m,_) = bestArrangement (ors s) [(minDim s, maxDim s, s)] (_boxDim box)
            ]
    in map snd $ sortBy (compare `on` fst) tries

-- | used ratio volume boxes / vol shelf
usedRatio :: Ranking -> Box s -> Shelf s -> Int -> Int -> Int -> Double
usedRatio Rank3D box s n k m  = (fromIntegral (n*k*m))*boxVolume box / shelfVolume  s
usedRatio Rank2D _ _ _ 0 _ = 0
usedRatio Rank2D box s n _ m = let
    bdim = boxDim box
    sdim = minDim s
    in (fromIntegral (n*m))*(dLength bdim * dHeight bdim) / (dLength sdim * dHeight sdim)
