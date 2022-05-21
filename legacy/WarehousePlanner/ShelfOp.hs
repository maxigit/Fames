module WarehousePlanner.ShelfOp 
( splitShelf
, unSplitShelf

, splitTo
, generateGrid
) where
import ClassyPrelude
import WarehousePlanner.Base
import Data.List (scanl')
import Data.Char (chr)

-- | Split a shelf into 2, 4 or 8 subshelves
splitShelf :: Shelf s -> Dimension ->  WH [Shelf s] s
splitShelf shelf@Shelf{..} dim = do
  let Dimension l w h = dim
      splits = generateGrid minDim [l] [w] [h]
      adjustMax last accessor gDim = 
        if last -- last elemen of a gree the max need to be increased
        then 
          accessor gDim + accessor maxDim - accessor minDim
        else
          accessor gDim
      go split@Split{..} =
        if (il, iw, ih) == (0,0,0)
        then -- original shelf
          updateShelf (\s -> s {minDim =  gDim, maxDim = gDim}) shelf
        else do -- new one
          -- create it
          let newMin = gDim
              newMax = Dimension (adjustMax ll dLength gDim)
                                 (adjustMax lw dWidth gDim)
                                 (adjustMax lh dHeight gDim)
              suffix = pack $ "/" <> map (\i -> chr $ i + 97) ( hasSuffix il ll ++ hasSuffix iw lw ++ hasSuffix ih lh)
              -- hasSuffix 1 True = [] -- if there is no split to need to create a suffix
              hasSuffix i _ = [i]


          new <- newShelf (shelfName <> suffix)
                          (Just $ intercalate "#" $ flattenTags shelfTag )
                          newMin
                          newMax
                          (dHeight gOffset)
                          shelfBoxOrientator
                          shelfFillingStrategy
          -- steal all boxes which belongs to it
          shelfBoxes <- findBoxByShelf shelf
          forM shelfBoxes (\box -> do
            case dimInSplit split (boxOffset box ) of
              Nothing -> return ()
              Just offset -> do
                assignShelf (Just new) box
                updateBox (\b -> b { boxOffset = offset }) box
                return ()
            )
          return new

  mapM go splits

-- | Generates breaks  so that
--    [1 2 4] 10 generates
--    0123456789
--    1224444555
--    1 0 :: (length offset
--    2 1
--    4 3
--    3 7
splitTo :: [Double] -> Double -> [(Double,Double, Bool)] -- ^ interval width, offset last
splitTo [] maxX = [(maxX, 0, True)]
splitTo (xs) maxX = let
  (offsets,_) = span (<maxX) (scanl' (+) 0 xs) 
  in reverse $ case reverse $ zip3 xs offsets (repeat False) of
    (last@(x, offset,_): reversed) -> 
      let most = x+offset
      in if most >= maxX
          then (maxX -offset, offset, True) : reversed
          else -- add last element
            (maxX - most, most, True) : last : reversed
    _ -> error "List should not be empty"
  

data Split = Split 
  { gDim :: Dimension
  , gOffset :: Dimension
  , il, iw, ih :: Int -- indexes
  , ll, lw, lh :: Bool -- last 
  } deriving (Show)
generateGrid :: Dimension -> [Double] -> [Double] -> [Double] -> [Split]
generateGrid Dimension{..} ls ws hs =
  [ Split{..}
  | (il, (l,ol,ll)) <- zip [0..] $ splitTo ls dLength
  , (iw, (w,ow,lw)) <- zip [0..] $ splitTo ws dWidth
  , (ih, (h,oh,lh)) <- zip [0..] $ splitTo hs dHeight
  , let gDim = Dimension l w h
  , let gOffset = Dimension ol ow oh
  ]


ds :: [Dimension -> Double]
ds = [dLength, dWidth, dHeight]
-- | Check if a given dim belongs to a split 
-- and return the relative offset
dimInSplit :: Split -> Dimension -> Maybe Dimension
dimInSplit Split{..} dim = let
  diff f = f dim - f gOffset
  diffs@[l, w, h] = map diff ds
  in if and $ zipWith (<=) diffs (map ($ gDim) ds) 
            ++ map (>=0) diffs
      then Just $ Dimension l w h
      else Nothing


-- | Unsplit a shelf by combining all its "children<
-- to it
unSplitShelf :: Shelf s -> WH (Shelf s) s
unSplitShelf shelf = do
  children <- findShelfBySelector (parseSelector $ shelfName shelf ++ "/*") >>= mapM findShelf
  let oMaps = [offsetsFromIndex (minDim shelf) i f children
                             | (i, f) <- zip [1..] ds
                             ]
  lwhS <- forM children $ \child -> do
    let [ol,ow, oh] = [findWithDefault 0 (childIndex child i) m 
                  | (i,m) <- zip [1..3] oMaps
                  ]
        offset = Dimension ol ow oh
    boxes <- findBoxByShelf child
    -- reassign each box the original shelf
    newBoxes <-  mapM (updateBox (\box -> box {boxOffset = boxOffset box <>  offset})) boxes
    mapM (assignShelf (Just shelf)) newBoxes
    deleteShelf $ shelfId child
    let Dimension ml mw mh = offset <> minDim child
    return (ml, mw, mh)
  -- compute new shelf size
  let  (mls, mws, mhs) = unzip3 lwhS
       newMin = Dimension (maximumEx mls) (maximumEx mws) (maximumEx mhs)
       newMax = newMin <> maxDim shelf <> invert (minDim shelf)
  updateShelf (\s -> s {minDim = newMin, maxDim = newMax }) shelf
  return shelf

-- | Extract the n index from a child nmae
-- index starts a 1
-- from exapmle childIndex 2 shelf/abc => b
childIndex :: Shelf s -> Int -> Char
childIndex = indexFromName . shelfName
indexFromName :: Text -> Int -> Char
indexFromName name z = let
  i = length name - 4 + z
  in fromMaybe 'a' $ index name i

  
  

-- | The naming scheme of all children
-- include a 3 letter index from a-z
-- We know that all children have been 
-- generated in a grid way or slices
-- within a slice (same index) all shelves should have the same
-- "thickness".
-- This function computes the offset for each slices given a direction
offsetsFromIndex :: Dimension -> Int -> (Dimension -> Double) -> [Shelf s] -> Map Char Double
offsetsFromIndex dim0 z f shelves = let
  widthMap :: Map Char Double
  widthMap = mapFromList [(childIndex s z, f (minDim s)) | s <- shelves]
  (indexes, widths) = unzip $ mapToList widthMap
  base = case indexes of
          'a':_ -> 0
          _ -> f dim0
  offsets = scanl' (+) base widths
  in mapFromList $ zip indexes offsets
  

