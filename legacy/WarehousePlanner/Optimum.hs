-- | Function to find (or at least) optimal  arrangement
module WarehousePlanner.Optimum where
import Prelude
import WarehousePlanner.Base
import Data.List(sortBy)
import Data.Function(on)
import qualified Data.Map.Strict as Map'
import Control.Monad.State

-- | find shelves allowing the less waste
-- if it were filled with the same box
bestShelves :: Box s -> (Shelf s -> [OrientationStrategy]) -> [Shelf s] ->  [Shelf s]
bestShelves box ors ss  = let
    tries = [ (-((fromIntegral (n*k*m))*boxVolume box / shelfVolume s), s)
            | s <- ss
            , let (_,_,n,k,m,_) = bestArrangement (ors s) [(maxDim s, s)] (_boxDim box)
            ]
    in map snd $ sortBy (compare `on` fst) tries

fillBest :: (Box s -> [OrientationStrategy] -> [Shelf s] -> [Shelf s])
          -> [Box s] 
          -> [Shelf s]
          -> WH [Box s] s
fillBest  fit boxes shelves = do
    aroundArrangement (fillBest' fit) boxes shelves

fillBest' :: Shelf' shelf =>
             (Box s -> [OrientationStrategy] -> t -> [shelf s])
          -> [Box s] -> t -> WH [Box s] s
fillBest' fit boxes shelves = do
        boxo <- gets boxOrientations
        let groups = reverse . Map'.toList $ Map'.fromListWith (++) tuples
            tuples = [((_boxDim b, boxStyle b), [b]) | b <- boxes]
        boxess <- mapM ( \bs -> let
                shelvesInOrder = fit example ors shelves
                ors = boxo example (error "need one shelf and only one")
                example = head bs
                -- in moveBoxes [example] shelvesInOrder
                in moveBoxes ExitLeft bs shelvesInOrder
                ) (sortBy (compare `on` (\g -> - (length g))) (map snd groups))

        return $ concat boxess

    
    
