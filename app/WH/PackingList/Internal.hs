module WH.PackingList.Internal
( module WH.PackingList.Internal
, Dimension(..)
)
where

import ClassyPrelude.Yesod
import WarehousePlanner.Base hiding(Box(..))
import qualified Data.Map as Map

-- * Types
type Style = Text
data Slice = Slice
             { slBox :: Box -- ^ style and box information
             , slNL :: Int -- ^ width in boxes
             , slNW :: Int -- ^ Depth in Boxes
             , slNH :: Int -- ^ Height in boxes
             , slLength :: Double -- ^ width
             , slWidth :: Double -- ^ Depth
             } deriving (Show, Eq)
data Box = Box
  { boxDimension :: Dimension
  , boxStyle :: Style
  , boxNumber :: Int -- number of similar boxes
  } deriving (Show, Eq)

-- * Slices for Chalk
-- helps how to unload a container by finding "slices" of boxes of the same style
findSlices :: [Box] -> [Slice]
findSlices boxes = []


-- | Group boxes by style and ignore somehow, pathological dimension.
groupByStyle :: [Box] -> Map Style Box 
groupByStyle boxes = let
  -- collect all boxes of same style
  groups = Map.fromListWith (<>) [ (boxStyle box, opoint box)
                                 | box <- boxes
                                 ]
  in fmap aggregateGroupBox groups

-- | Aggregate box of the same style, by using the dimension of the most numerous one
-- We assume here all box have the same style
aggregateGroupBox :: NonNull [Box] -> Box
aggregateGroupBox (boxes) = let
  -- group and count dimension
  dim'countz = Map.toList $ Map.fromListWith (+) [(boxDimension box, boxNumber box) | box <- toNullable boxes]
  dim = fst $ (maximumBy (comparing snd) (impureNonNull dim'countz))
  number = sum (impureNonNull $ map snd dim'countz)
  in  (head boxes) { boxDimension = dim, boxNumber = number }

  

