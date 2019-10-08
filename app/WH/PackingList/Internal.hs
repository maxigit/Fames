module WH.PackingList.Internal
( module WH.PackingList.Internal
, Dimension(..)
, volume
)
where

import ClassyPrelude.Yesod
import WarehousePlanner.Base hiding(Box(..))
import qualified Data.Map as Map
import Data.List(scanl')

-- * Types
type Style = Text
-- | A slice of boxes in a  unloading zone.
-- Boxes are unloaded in the unloaded area in a rectangular zone.
-- Each boxes of the same stiles are together using the full depth (width) of the zone
-- example
-- Lest's say we have 9 boxes of A, 30 Bs , 24 C and one 1
--  ****************** Wall ***************************
--  +-------+-----------------------+----------------+
--  + A     +    B                  + C              +
--  + 1x3x3 +    5x3x2              + 2x4x3          +  Zone 1
--  +-------+-----------------------+----------------+
--       path
--  +------------------------------------------------+
--  + D                                              + Zone 2
--  + 1x1x1                                          +
--  +------------------------------------------------+
--  Each section of similar style box is called a slice.

data Slice = Slice
             { slBox :: Box -- ^ style and box information
             , slNL :: Int -- ^ width in boxes
             , slNW :: Int -- ^ Depth in Boxes
             , slNH :: Int -- ^ Height in boxes
             , slLength :: Double -- ^ width
             , slWidth :: Double -- ^ Depth
             } deriving (Show, Eq)
data Placement = Begining | Middle | End deriving(Show, Eq, Ord)
data Box = Box
  { boxDimension :: Dimension
  , boxStyle :: Style
  , boxNumber :: Int -- number of similar boxes
  , boxPlacement :: Placement
  } deriving (Show, Eq)

instance Ord Box where
  compare a b = compare (key a) (key b) where
    key box = (boxPlacement box, boxDimension box) 

-- | Zone description + slices
data Zone = Zone
  { zoneName :: Text
  , zoneDimension :: Dimension
  , zoneSlices :: [Slice]
  } deriving (Show, Eq)

-- * Slices for Chalk
-- helps how to unload a container by finding "slices" of boxes of the same style
findSlices :: [Zone] -> [Box] -> [Zone]
findSlices zones0 boxes = let
  styles0 = groupByStyle boxes
  -- first, we need to see if all boxes of a given style
  -- fits in any zone in only on row. Those styles are put in priority in the best fitting zone.
  (zones1, boxes1) = fitOneRow zones0 (toList styles0)
  (zones2, _boxes2) = fitAllRow (sortOn zoneName zones1) boxes1
  r =  sortOn zoneName zones2
  in r


-- | Sort slices and calculate offset
sortSlices :: [Slice] -> [(Slice, Double)]
sortSlices slices = let
  sorted = sortOn slBox slices
  lengths = scanl' (+) 0 (map slLength sorted)
  in zip sorted lengths



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


 -- | Try to fit style occupying one row
-- One of the problem, is to find the best fit.
-- However, we don't try to find the best fit but just an acceptable.
-- We try all boxes in order, and try to find the best zone.
fitOneRow :: [Zone] -> [Box] -> ([Zone], [Box])
fitOneRow = fitOneRow' []
fitOneRow' :: [Box] -> [Zone] -> [Box] -> ([Zone], [Box])
fitOneRow' triedBoxes [] boxes =  ([], triedBoxes ++ boxes)
fitOneRow' triedBoxes zones [] =  (zones, triedBoxes)
fitOneRow' triedBoxes zones (box:boxes) = let
  -- find best zone if any
  tries = catMaybes $ map (tryFitOne'  box) (holes zones)
  tryFitOne' box (z, zs) = fmap (,zs) (tryFitOne box z)
  -- minimize width left
  -- rank zone = traceShow ("TO RANK", (fst zone)) (rank' zone)
  rank (Zone _ zdim (slice:_), _) = width - usedWidth where
    -- we know there is at least one slice and the one we need is the first one.
    width = dWidth zdim
    usedWidth = slWidth slice
  rank _ = error "never happen"
  in case fromNullable tries of
        Nothing -> fitOneRow' (box:triedBoxes) zones boxes
        Just tries' -> let (bestZone, otherZones) = head $ sortOn rank tries'
             in fitOneRow' triedBoxes (bestZone:otherZones) boxes
                
                
fitAllRow :: [Zone] -> [Box] -> ([Zone], [Box])
fitAllRow = fitAllRow' []
fitAllRow' :: [Zone] -> [Zone] -> [Box] -> ([Zone], [Box])
fitAllRow' usedZones zones [] = (usedZones ++ zones, [])
fitAllRow' usedZones [] boxes = (usedZones, boxes) 
fitAllRow' usedZones (zone:zones) bs@(box:boxes) = case slice box zone of
  (Nothing, Just _) -> fitAllRow' (zone:usedZones) zones bs
  (Just slice, Nothing) -> fitAllRow' usedZones (addSlice zone slice: zones) (boxes)
  (Just slice, Just box') -> let -- the box is split, we need to change the placement to end and beging
    box'' = box' {boxPlacement = Begining}
    lastBox = slBox slice
    slice' = slice {slBox = lastBox { boxPlacement = End}}
    in fitAllRow' usedZones (addSlice zone slice': zones) (box'':boxes)
  _ -> error "should not happen"
  

divUp p q = (p+q-1) `div` q

-- | Arrange boxes into a slice to fit the given zone
slice :: Box -> Zone -> (Maybe Slice, Maybe Box)
slice box zone = let
  bDim = boxDimension box
  zDim = freeZoneDimension zone
  n = boxNumber box
  in case howMany zDim bDim  of
      (lmax, wmax, hmax) | lmax >0, wmax >0, hmax >0 -> let
                          -- we fill boxes verticaly first, then the depth,
                          -- to end with the length, so that we keep length minimal
                          nh = min (hmax+1) n -- we use all height possible . We allow boxes to stick out up
                                              -- this is because the height in the zone correspond
                                              -- to the bottom of the highest box
                          nw = min wmax (n `divUp`  nh)
                          nl = min lmax (n `divUp` (nh*nw))
                          slice = Slice { slBox = box {boxNumber = fitted}
                                        , slNL = nl
                                        , slNW = nw
                                        , slNH = nh
                                        , slLength  = dLength bDim * fromIntegral nl + 5  --  margin
                                        , slWidth = dWidth bDim * fromIntegral nw
                                        }
                          fitted = min (nl*nw*nh) n
                          leftOver = n - fitted
                          boxm = if leftOver <= 0
                                then Nothing
                                else Just $ box { boxNumber = leftOver}
                          in (Just slice, boxm)
      _ -> (Nothing, Just box)

addSlice :: Zone -> Slice -> Zone
addSlice zone slice = zone {zoneSlices = slice : zoneSlices zone}
  
-- | Can a full style in the given zone within a single row ?
tryFitOne :: Box -> Zone -> Maybe Zone
tryFitOne box zone = let
  _bDim = boxDimension box
  _zDim = zoneDimension zone
  in case slice box zone of
       (Just slice, Nothing) | slNL slice == 1 -> Just $ addSlice zone slice
       _ -> Nothing

-- | Computes the length left in the zone, given the slices already in it.
lengthLeft :: Zone -> Double
lengthLeft zone =  dLength (zoneDimension zone) - usedLength zone

usedLength :: Zone -> Double
usedLength zone = foldr (+) 0 (map slLength (zoneSlices zone))

-- | Dimension left in the zone after removing the space occupied by the slices
freeZoneDimension :: Zone -> Dimension
freeZoneDimension zone = (zoneDimension zone) { dLength = lengthLeft zone}



-- |  computes a pair of elements, other elements in the list
-- ex: [1,2,3] -> [(1, [2,3]), (2, [1,3]), (3, [1,2])
holes :: [a] -> [(a, [a])]
holes xs = go [] xs where
  go _ [] = []
  go xs0 (x:xs) = (x, reverse xs0++xs) : go (x:xs0) xs
  
