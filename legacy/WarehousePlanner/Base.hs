{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoOverloadedStrings #-}
module WarehousePlanner.Base
( newShelf
, newBox
, moveBoxes
, updateBoxTags
, updateBox
, updateShelf
, boxStyleAndContent
, extractTag
, extractTags
, tagToOnOff
, expandAttribute'
, expandAttribute
, findShelfByName
, findBoxByStyleAndShelfNames
, findBoxByShelf
, findBoxByStyle
, shelfBoxes
, splitBoxSelector
, patternToMatchers
, orTrue
, howMany
, filterBoxByTag
, filterByTag
, filterShelfByTag
, buildWarehouse
, emptyWarehouse
, defaultShelf
, incomingShelf
, aroundArrangement 
, bestArrangement
, usedDepth
, printDim
, module WarehousePlanner.Type
)
where
import Prelude
import Text.Printf(printf)
import Data.Vector(Vector)
import Control.Monad.State
import Data.Monoid
import qualified Data.Map.Strict as Map'
import Data.List(sort, sortBy, groupBy, nub, (\\), union, maximumBy, delete, stripPrefix)
import Data.Maybe(catMaybes, fromMaybe, isJust)
import Control.Applicative
import Data.Ord (comparing, Down(..))
import Data.List.Split(splitOn)
import Data.Foldable (asum)
import Data.Function(on)
import Data.Traversable (traverse, sequenceA)
import Data.STRef
import Control.Monad.ST
import Data.Sequence (Seq, (|>))
import Data.Foldable (toList)
import qualified Data.Traversable as T
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either(lefts,rights)
import Data.Maybe(maybeToList, mapMaybe)
import WarehousePlanner.Type
import WarehousePlanner.SimilarBy
import Diagrams.Prelude(white)

import qualified System.FilePath.Glob as Glob
import Text.Read (readMaybe)


import Debug.Trace



-- | Computes the max length or height used within a shelf
-- | by all the boxes. Should be able to be cached.
-- maxUsedOffset :: (Dimension -> Double) -> Shelf -> WH
maxUsedOffset :: (Dimension -> Double) -> Shelf s -> WH Double s
maxUsedOffset proj shelf = do
    boxes <- findBoxByShelf shelf
    return $ foldr (\b r -> max r ((proj.boxOffset') b)) 0 boxes

lengthUsed, widthUsed, heightUsed :: Shelf s -> WH Double s
lengthUsed = maxUsedOffset dLength
widthUsed = maxUsedOffset dWidth
heightUsed = maxUsedOffset dHeight



-- | Nested groups of shelves, used for display

__unused_groupToShelfIds :: ShelfGroup s -> [ShelfId s]
__unused_groupToShelfIds (ShelfProxy sid) = [sid]
__unused_groupToShelfIds (ShelfGroup  groups _ ) = concatMap __unused_groupToShelfIds groups


rackUp :: Shelf' shelf => [shelf s] -> ShelfGroup s
rackUp ss = ShelfGroup (reverse g) Vertical where
    g = map (ShelfProxy . shelfId) ss

buildWarehouse :: [[[String]]] -> WH (ShelfGroup s) s
buildWarehouse xsss = do
    bays <- mapM buildBay xsss
    return $ ShelfGroup bays  Vertical

buildBay :: [[String]] -> WH (ShelfGroup s) s
buildBay xss = do
    racks <- mapM buildRack xss
    return $ ShelfGroup racks Horizontal

buildRack :: [String] -> WH (ShelfGroup s) s
buildRack xs = do
    idsS <- mapM ( \x ->  do
            case x of
                '-':name ->  fmap (map shelfId) $ updateShelfByName (\s -> s { flow = RightToLeft }) name
                name ->  findShelfByName name
            ) xs
    shelves <- mapM findShelf (concat idsS)
    let sorted = sortBy (comparing shelfName) shelves

    return $ rackUp sorted

 -- | shelf use as error, ie everything not fitting anywhere
defaultShelf :: WH (ShelfId s) s
defaultShelf = (flip Seq.index) 0 <$> gets shelves

-- | shelf use to put incoming boxes
incomingShelf :: WH (ShelfId s) s
incomingShelf = do
  shelves <- gets shelves
  return $ Seq.index shelves (min 1 (length shelves -1))

findBoxByShelf :: Shelf' shelf => shelf s -> WH [Box s] s
findBoxByShelf shelf = do
  boxIds <- _shelfBoxes <$> findShelf shelf
  mapM findBox boxIds


findBoxByStyle :: String -> WH [BoxId s] s
findBoxByStyle style = do
  boxIds <- toList <$> gets boxes
  fmap (map boxId) (findByName (mapM findBox boxIds) (boxStyle)  style)

findShelfByBox :: Box' b => b s -> WH (Maybe (ShelfId s)) s
findShelfByBox b = do
  box <- findBox b
  return $ boxShelf box

findShelvesByBoxes :: Box' b => [b s] -> WH ([ShelfId s]) s
findShelvesByBoxes boxes = fmap catMaybes (mapM findShelfByBox boxes)

-- | find shelf by name and tag
findShelfByName :: String -> WH [ShelfId s] s
findShelfByName name' = do
  let (name, tagM) = extractTag name'
  shelfIds <- toList <$> gets shelves
  shelves0 <-  findByName (mapM findShelf shelfIds) shelfName  name
  let shelves1 = filter (filterShelfByTag tagM) shelves0
  return $ map shelfId shelves1

filterShelfByTag :: Maybe String -> Shelf s -> Bool
filterShelfByTag tagM shelf = case tagM of
  Nothing -> True
  Just tag -> let (on,off) = tagToOnOff tag
              in filterByTag on off (maybeToList $ shelfTag shelf)

filterBoxByTag :: [String -> Bool] -> [String -> Bool] -> Box s -> Bool
filterBoxByTag ons offs box =  filterByTag ons offs (boxTagList box)

-- | all tags from the left must belong to the right
-- ex if box as tag A B C, A#B should match but not A#E
filterByTag :: [String -> Bool] -> [String -> Bool] -> [String] -> Bool
filterByTag on off tags = let
  selectorMatched matcher = any matcher tags
  in all selectorMatched (on) -- 
     && not (any selectorMatched (off))

tagToOnOff :: String -> ([String -> Bool], [String -> Bool])
tagToOnOff selector =  let
  selectors = map ok $ splitOn "#" selector
  ok ('-':t) = Left t
  ok t = Right t
  on = rights selectors
  off = lefts selectors
  matchers ss = map (Glob.match . Glob.compile) ss
  in (matchers on, matchers off)
patternToMatchers :: String -> [(String -> Bool)]
patternToMatchers "" = [const True]
patternToMatchers pat = map mkGlob (splitOn "|" pat) where
  mkGlob ('!':pat) = not . mkGlob pat
  mkGlob pat = Glob.match $ Glob.compile pat

findByName :: WH [a s] s -> (a s -> String) -> String -> WH [a s] s
findByName objects objectName "" = objects
findByName objects objectName name = do
   let matchers = patternToMatchers name --  map (Glob.match . Glob.compile) (splitOn "|" name) :: [String -> Bool]
   filter (\o -> any ($ (objectName o)) matchers) <$> objects

splitBoxSelector :: String -> (String, Maybe String,  BoxNumberSelector, String, Maybe String)
splitBoxSelector pat = let
  (styleMax, location') = break (=='/') pat
  (style', nMax) = break (=='^') styleMax
  (style, boxtag) = extractTag style'
  (location, locTag) = extractTag location'
  in  (style, boxtag, parseBoxNumberSelector (drop 1 nMax), drop 1 location, locTag)

-- | Syntax content^shelf^total
parseBoxNumberSelector :: String -> BoxNumberSelector
parseBoxNumberSelector "" = BoxNumberSelector Nothing Nothing Nothing
parseBoxNumberSelector s = let
  splits = splitOn "^" s
  parsed = map readMaybe splits 
  [content, shelves, total] = take 3 $ parsed <> cycle [Nothing]
  in BoxNumberSelector content shelves total

-- | TODO Should be true be seems to work like that
-- this will mean, that we need a normal or 
orTrue [] = False
orTrue bs = or bs
-- | Find box for a given style but only belonging
-- to the given shelves. This allows to only move
-- boxes in error or coming and leave the current box
-- of a particular style to stay where they are.
-- We can also specify a maximum number to pick
-- it needs to be before the shelf condition
--
-- syntax is  Box#tag^3/shelf#tag : 3 box from shelf shelf
findBoxByStyleAndShelfNames :: String -> WH [BoxId s] s
findBoxByStyleAndShelfNames style'' = do
  let (style, boxtag, nMax, location, locTag) = splitBoxSelector style''
      inShelves name  = or $ patternToMatchers location <*> [name]
      -- locations = splitOn "|" (drop 1 location)
      -- inShelves _ [] = True
      -- inShelves shelfName ('!':pattern_) = not $  Glob.match (Glob.compile pattern_) shelfName
      -- inShelves shelfName pattern_ = Glob.match (Glob.compile pattern_) shelfName
  -- all boxes matching name
  allBoxesBeforeTag' <- findBoxByStyle style
  allBoxesBeforeTag <- mapM findBox allBoxesBeforeTag'
  -- filter by tag if any
  let allBoxes = case boxtag of
                  Nothing -> allBoxesBeforeTag
                  Just bt -> do
                    let  (on, off) = tagToOnOff bt
                         fb = filterBoxByTag on off 
                    filter  fb allBoxesBeforeTag

  boxesWithShelfName <- mapM ( \b -> do
      shelfIdM <- findShelfByBox (boxId b)
      shelfM <- traverse findShelf shelfIdM
      let shelf = case shelfM of
                    Just shelf'  -> if filterShelfByTag locTag shelf'
                                              then shelfM
                                              else Nothing
                    _ -> shelfM

      return $ sequenceA (b, shelfName <$> shelf)
      ) allBoxes


  let box'nameS =  [ bs | bs@(_, shelfName) <- catMaybes boxesWithShelfName, inShelves shelfName ]
  -- filter boxes by number
  return . map (boxId  . fst) $ limitByNumber nMax box'nameS


-- | Limit a box selections by numbers
limitByNumber :: BoxNumberSelector -> [(Box s, String)] -> [(Box s, String)]
limitByNumber bn@(BoxNumberSelector contentN shelfN totalN) boxes0 = let
  sorted = sortBy (comparing  $ ((,) <$> boxGlobalPriority <*> boxRank) . fst ) boxes0
  boxes1 = maybe id (limitBy (boxSku . fst)) contentN $ sorted
  boxes2 = maybe id (limitBy snd) shelfN $ boxes1
  boxes3 = maybe id take totalN $ sortBy (comparing  $ ((,) <$> boxGlobalPriority <*> boxRank) . fst ) boxes2
  in boxes3


-- limitBy :: Ord k => (Box s -> k) -> Int -> [Box s] -> [a]
limitBy key n boxes = let
  sorted = sortBy (comparing  $ ((,) <$> boxGlobalPriority <*> boxRank) . fst ) boxes
  group = Map'.fromListWith (flip(<>)) [(key box, [box]) | box <- sorted]
  limited = fmap (take n . sortBy (comparing $ boxRank . fst) ) group
  in concat (Map'.elems limited)
  


defaultBoxOrientations box shelf =
    let orientations = case (shelfBoxOrientator shelf)  of
            (DefaultOrientation) ->  [up, rotatedUp]
            (ForceOrientations orientations) -> orientations
            (BoxOrientations) -> case boxBoxOrientations box of
                                    [] ->  [up, rotatedUp]
                                    ors -> ors
            FilterOrientations orientations -> boxBoxOrientations box \\ orientations
            AddOrientations lefts rights -> lefts `union` boxBoxOrientations box `union` rights
    in map (\o -> (o,0,1)) orientations

emptyWarehouse = Warehouse mempty mempty mempty (const white) (const (Nothing, Nothing)) defaultBoxOrientations

newShelf :: String -> Maybe String -> Dimension -> Dimension -> BoxOrientator -> FillingStrategy -> WH (Shelf s) s
newShelf name tag minD maxD boxOrientator fillStrat = do
        warehouse <- get
        ref <- lift (newSTRef (error "should never been called. Base.hs:327"))
        let shelf = Shelf (ShelfId ref) [] name tag minD maxD LeftToRight boxOrientator fillStrat
        lift $ writeSTRef ref shelf

        put warehouse { shelves = shelves warehouse  |> ShelfId ref }
        return shelf

newBox :: Shelf' shelf => String -> String ->  Dimension -> Orientation -> shelf s  -> [Orientation]-> [String] -> WH (Box s) s
newBox style content dim or shelf ors tags = do
    warehouse <- get
    let tags' = Set.fromList $ case content of
          "" -> tags
          _ -> ('\'':content): map (map replaceSlash) tags
          -- create tag of dimension
        dtags = dimensionTags dim

    ref <- lift $ newSTRef (error "should never been called. undefined. Base.hs:338")
    let box = Box (BoxId ref) (Just $ shelfId shelf) style content dim mempty or ors (tags' <> dtags) defaultPriorities (extractBoxBreak tags')
    shelf' <- findShelf shelf
    linkBox (BoxId ref) shelf'
    lift $ writeSTRef ref box
    put warehouse { boxes = boxes warehouse |> BoxId ref
                  }
    return box

-- | Generates tag for dimensions. Used when creating a box
-- but also to delete the existing one when updating a box dimension
dimensionTags :: Dimension  -> Set String
dimensionTags dim = Set.fromList [dshow 'l' dLength, dshow 'w' dWidth, dshow 'h' dHeight]
  where dshow c f = '\'' : c : show (floor $ 100 * f dim)

-- | Extract new dimensions from tags
extractDimensions :: Dimension -> Set String -> Maybe Dimension
extractDimensions dim tags = case (go "'l", go "'w", go "'h") of
  (Nothing, Nothing, Nothing) -> Nothing
  (l, w, h) -> Just dim { dLength = fromMaybe (dLength dim) l
                        , dWidth = fromMaybe (dWidth dim) w
                        , dHeight = fromMaybe (dHeight dim) h
                        }
  where go prefix = msum $ map (go' prefix) (Set.toList tags)
        go' prefix tag = (/100) `fmap` (stripPrefix prefix tag >>= readMaybe) 

updateDimFromTags :: Set String -> Box s -> Box s
updateDimFromTags tags box = case extractDimensions (_boxDim box) tags of
  Nothing -> box
  Just dim -> let to_add = dimensionTags dim
                  to_remove = dimensionTags (_boxDim box)
               in box { boxTags = (boxTags box Set.\\ to_remove) <> to_add, _boxDim = dim  }

 
-- | Assign a box to shelf regardless of if there is enough space
-- left or not.
-- For a "real" move see moveBox
assignShelf :: (Box' box,  Shelf' shelf) => Maybe (shelf s) -> box s -> WH () s
assignShelf s b = do
    box <- findBox b
    oldShelfM <- traverse findShelf (boxShelf box)
    newShelfM <- traverse findShelf s

    -- if box belong to a shelf
    -- we need to update the shelf to remove the link to the box
    when (oldShelfM /= newShelfM) $ do
      traverse (unlinkBox (boxId box)) oldShelfM
      traverse (linkBox (boxId box)) newShelfM
      updateBox (\b -> b { boxShelf = shelfId `fmap` s }) box
      return ()

-- | Unlink a box from a shelf without
-- checking anything. Shoudn't be exported
unlinkBox :: BoxId s -> Shelf s -> WH () s
unlinkBox box shelf = do
  let boxes = _shelfBoxes shelf
      boxes' = delete box boxes

      shelf' = shelf { _shelfBoxes = boxes' }
  updateShelf (const shelf') shelf'
  return ()

-- | link a box from a shelf without
-- checking anything. Shoudn't be exported
linkBox :: BoxId s -> Shelf s -> WH () s
linkBox box shelf = do
  let boxes = _shelfBoxes shelf
      boxes' = box:boxes

      shelf' = shelf { _shelfBoxes = boxes' }
  updateShelf (const shelf') shelf'
  return ()
-- | find the best way to arrange some boxes within the given space
-- For the same number of boxes. use the biggest diagonal first, then  the smallest shelf
bestArrangement :: Show a => [(Orientation, Int, Int)] -> [(Dimension, a)] -> Dimension -> (Orientation, Int, Int, Int, a)
bestArrangement orientations shelves box = let
    options = [ (o, extra, (nl, max minW (min nw maxW), nh), sl*sw*sh)
              | (o, minW, maxW) <-   orientations
              , (shelf, extra) <- shelves
              , let Dimension sl sw sh =  shelf
              , let (nl, nw, nh) = howMany shelf (rotate o box)
              ]

    bests = sortBy (compare `on` fst)
                [ ( ( -nl*nh*nw
                    , bh*bh*fromIntegral(nh*nh)+bl*bl*fromIntegral(nl*nl)
                    , vol
                    )
                  , (ori, nl, nw, nh, extra)

                  )
                 | (ori, extra, (nl, nw, nh), vol ) <- options
                 , let Dimension bl bh _bw = rotate ori box
                 ]
    in
        -- trace ({-show shelves ++ show box ++-}  show bests) $
        (snd . head) bests


-- | * test
{-
dx = Dimension 192 100 145
dn = Dimension 160 100 145
b = Dimension 47 39 85
os = [tiltedForward, tiltedFR]
-}


-- | How many inner rectangle can fit in the outer one ?
howMany :: Dimension -- ^ Outer
        -> Dimension -- ^ Inner
        -> (Int, Int, Int)
howMany (Dimension l w h) (Dimension lb wb hb) = ( fit l lb
                                                 , fit w wb
                                                 , fit h hb
                                                 ) where
        fit d db = floor (max 0 (d-0) /(db+0))


type SimilarBoxes s = SimilarBy Dimension (Box s)
-- Move "physicaly" a box into a shelf if there is enough space
-- Boxes are supposed to be the same size.
-- Sometime we want to arrange boxes in column across boxes
-- In that case, instead of filling the bottom shelf as much as we can
-- we need to fill only one column, go to the next shelves, then
-- start again with the bottom boxes with the remaining shelves.
-- This the purpose of the ExitOnTop mode.
-- | List of "similar" object. In case of boxes of the same dimension.
-- This type is just for information.
-- There is nothign to enforce the object similarity.
-- return the shelf itself if not empty
fillShelf :: (Shelf' shelf)
          => ExitMode
          -> shelf s
          -> SimilarBoxes s
          -> WH ( Maybe (SimilarBoxes s)
                , Maybe (Shelf s)
                )  s
fillShelf exitMode  s simBoxes0 = do
    let simBoxes@(SimilarBy dim b bs) = sortSimilarOn boxRank simBoxes0
    shelf <- findShelf s
    let boxes = b : bs
    -- first we need to find how much space is left
    lused <- lengthUsed shelf
    hused <- heightUsed shelf
    wused <- widthUsed shelf
    case  (boxBreak b, lused*hused*wused > 0) of
      (Just StartNewShelf, True ) -> return (Just simBoxes, Nothing ) -- shelf non empty, start new shelf
      _ -> do
        boxo <- gets boxOrientations
        let orientations = boxo b shelf
        let (bestO, nl_, nw, nh, (lused', hused')) =
                            bestArrangement orientations
                                              [ (Dimension (max 0 (shelfL -l)) shelfW (max 0 (shelfH-h)), (l,h))
                                              | (Dimension shelfL shelfW shelfH) <- [ minDim shelf, maxDim shelf ]
                                            -- try min and max. Choose min if  possible
                                              , (l,h) <- [(lused,0), (0,hused)] -- simplified algorithm
                                              ] dim
            nl = if exitMode == ExitLeft then nl_ else min 1 nl_
            Dimension l' w' h' = rotate bestO dim

            offsets = [Dimension (lused' + l'*fromIntegral il)
                                (w'* fromIntegral iw)
                                (hused' + h'*fromIntegral ih)
                      | (il, iw ,ih) <-  case (shelfFillingStrategy shelf) of
                                    ColumnFirst -> [(il, iw, ih)
                                        | il <- [0..nl-1]
                                        , ih <- [0..nh-1]
                                        , iw <- [0..nw-1]
                                        ]
                                    RowFirst -> [(il, iw, ih)
                                        | ih <- [0..nh-1]
                                        , il <- [0..nl-1]
                                        , iw <- [0..nw-1] -- width first
                                        ]
                       -- ^ with the current algorithm only looking
                       -- at the length and height used (and ignoring the depth )
                       -- we need to fill the depth (width) first, whichever filling row or column first
                       -- this might not be the expected Deadzone behavior but until we try
                       -- to find the biggest brick available (instead of the biggest 2d rectangle)
                       -- it seems a better solution
                       -- if modified modify assignOffsetWithBreaks accordingly
                      ]
            -- but within the box potentially move 
            box'Offsets = assignOffsetWithBreaks (shelfFillingStrategy shelf) Nothing boxes offsets
        -- traceShowM("Found break", mapMaybe (boxBreak . fst) box'Offset, breakm)
        mapM_ (uncurry $ shiftBox bestO) box'Offsets
        let leftm = dropSimilar (length box'Offsets) simBoxes
        case (nl*nw*nh, exitMode) of
            -- (0, _) -> return (leftm, Nothing) -- we can't fit any. Shelf is full
            (_ , ExitOnTop) -> return (leftm, Just shelf) -- ^ exit on top, we stop there, but the shelf is not full
            (_, ExitLeft) -> return (leftm, Nothing)  -- ^ pretends the shelf is full
            -- _ ->  fillShelfm exitMode  shelf leftm -- ^ try to fit what's left in the same shelf

    where shiftBox ori box offset = do
            updateBox (\b -> b { orientation = ori
                               , boxOffset = offset}) box
            assignShelf (Just s) box
          -- fillShelfm x s Nothing = return (Nothing, Just s)
          -- fillShelfm x s (Just lefts) = fillShelf x s lefts


-- |  Assign offset to boxes so they can be moved
-- taking  boxBreak into account. Basically a zip but can skip some offset or break
assignOffsetWithBreaks :: FillingStrategy -> Maybe Dimension ->   [Box s] -> [Dimension] -> [(Box s, Dimension)]
assignOffsetWithBreaks _ _ [] _  = []
assignOffsetWithBreaks _ _ _ []  = []
assignOffsetWithBreaks strat Nothing (box:bs) (offset:os) =  (box, offset) : assignOffsetWithBreaks strat (Just offset) bs os -- ignore the first break
assignOffsetWithBreaks strat (Just previous) bs@(box:_) os@(offset:_) = case boxBreak box of
  Nothing -> assignOffsetWithBreaks strat Nothing bs os
  Just StartNewShelf -> [] -- break
  Just StartNewSlice -> assignOffsetWithBreaks strat Nothing bs (dropWhile sameRow os)
  Just StartNewSlot -> assignOffsetWithBreaks strat Nothing bs (dropWhile sameSlot os)
  where sameSlot o = case strat of
          _ColumnFirst -> dHeight o <= dHeight previous  && dLength o <= dLength previous
          -- RowFirst -> dHeight o <= dHeight previous  && dWidth o <= dWidth previous
        sameRow o =  case strat of
          ColumnFirst -> dLength o <= dLength previous -- || dWidth o <= dWidth previous
          RowFirst -> dHeight o <= dHeight previous -- || dWidth o <= dWidth previous







-- Try to Move a block of boxes  into a block of shelves.
-- Boxes are move in sequence and and try to fill shelve
-- in sequence. If they are not enough space the left boxes
-- are returned.
moveBoxes :: (Box' b , Shelf' shelf) => ExitMode -> [b s] -> [shelf s] -> WH [Box s] s

moveBoxes exitMode bs ss = do
  boxes <- mapM findBox bs
  let layers = groupBy ((==)  `on` boxGlobalPriority) $ sortBy (comparing boxGlobalPriority) boxes
  lefts <- forM layers $ \layer -> do
    let groups = groupSimilar _boxDim layer
    -- traceShowM ("GRoups", length groups, map (\(SimilarBy dim g1 g ) -> (show $ length g + 1, show . _roundDim $ dim )) groups)
    lefts <- mapM (\g -> moveSimilarBoxes exitMode g ss) groups
    return $ concatMap unSimilar $ catMaybes lefts
  return $ concat lefts

_roundDim :: Dimension -> [Int]
_roundDim (Dimension l w h) = map (round . (*100)) [l,w,h]

 
-- | Move boxes of similar size to the given shelf if possible
moveSimilarBoxes :: (Shelf' shelf) => ExitMode -> SimilarBoxes s -> [shelf s] -> WH (Maybe (SimilarBoxes s)) s
moveSimilarBoxes exitMode bs ss = moveSimilarBoxesAndRetry exitMode bs ss []

-- | moves boxes into give shelf and retry nonfull shelves until all necessary
-- useful to fill selves using ExitONTop strategy
moveSimilarBoxesAndRetry :: (Shelf' shelf) => ExitMode -> SimilarBoxes s -> [shelf s] -> [shelf s] -> WH (Maybe (SimilarBoxes s)) s
moveSimilarBoxesAndRetry _ boxes [] [] = return (Just boxes)
moveSimilarBoxesAndRetry exitMode bs [] trieds = moveSimilarBoxesAndRetry exitMode bs (reverse trieds) [] -- can loop but normally ss is null
moveSimilarBoxesAndRetry exitMode boxes  (s:ss') trieds = do
    left <- fillShelf exitMode s boxes
    case left of
        (Nothing , _) -> return Nothing
        (Just bs', Nothing)  -> moveSimilarBoxesAndRetry exitMode bs' (ss') trieds -- discard current Shelf
        (Just bs', Just _)  -> moveSimilarBoxesAndRetry exitMode bs' ss' (s:trieds)


boxRank :: Box s -> (String, Int, String, Int)
boxRank box = ( boxStyle box , boxStylePriority box, boxContent box, boxContentPriority box)
-- | get the priority of a box. At the moment it is extracted from the tag.
-- we might set it as an attribute to speed things up
-- extract number from tag
boxContentPriority :: Box s -> Int
boxContentPriority box = p where (_, _, p) = boxPriorities box

boxStylePriority  :: Box s -> Int
boxStylePriority  box = p where (_,p,_)  = boxPriorities box

-- | Same as boxPriority but used before grouping box by styles
-- look at @n in tags
boxGlobalPriority  :: Box s -> Int
boxGlobalPriority  box = p where (p, _, _) = boxPriorities box


-- | Rearrange boxes within their own shelves
-- left over are put back to 0
rearrangeShelves :: Shelf' shelf => [shelf s] -> WH [Box s] s
rearrangeShelves ss = do
    -- first we need to remove the boxes from their current location
    boxes <- concat `fmap` mapM findBoxByShelf ss
    let nothing = head $ Nothing: map Just ss -- trick to force type
    mapM_ (\b -> assignShelf  nothing b ) boxes
    left <- moveBoxes ExitLeft boxes ss
    s0 <- defaultShelf
    mapM (assignShelf (Just s0)) left

    return left

-- | Remove boxes for shelvese and rearrange
-- shelves before doing any move
-- aroundArrangement  :: WH a -> WH a
aroundArrangement arrangement boxes shelves = do
    let shelfIds = map shelfId shelves
    oldShelveIds <- findShelvesByBoxes boxes
    -- remove all boxes from their actuall shelf
    let nothing = head $ Nothing : map Just shelves -- trick to typecheck
    mapM (assignShelf nothing) boxes
    -- rearrange what's left in each individual space
    -- so that there is as much space left as possible
    let os = nub $ oldShelveIds ++ shelfIds


    mapM rearrangeShelves (map (:[]) os)

    left <- arrangement boxes shelves
    s0 <- defaultShelf
    mapM (assignShelf (Just s0)) left
    return left









updateBox :: (Box' b) =>  (Box s ->  Box s) -> b s-> WH (Box s) s
updateBox f b = do
    box <- findBox b
    let box' = f box
    lift $ writeSTRef (getRef box') box'
    return box'

updateShelf :: (Shelf' shelf) => (Shelf s -> Shelf s ) -> shelf s -> WH (Shelf s) s
updateShelf f s =  do
    shelf <- findShelf s
    let shelf' = f shelf
    lift $ writeSTRef (getRef shelf') shelf'
    return shelf'


updateShelfByName :: (Shelf s -> Shelf s) -> String -> WH [Shelf s] s
updateShelfByName f n = findShelfByName n >>= mapM (updateShelf f)


-- | Add or remove the given tags to the give box
updateBoxTags' tags box = let
  btags = boxTags box
  parse ('-':tag) = Left tag
  parse tag = Right tag
  parsed = map parse tags
  to_add = Set.fromList $ rights parsed
  to_remove = Set.fromList $ lefts parsed
  new = (btags <> to_add) Set.\\ to_remove
  in updateDimFromTags to_add $ box { boxTags = new
                                    , boxPriorities = extractPriorities new
                                    , boxBreak = extractBoxBreak new
                                    }


updateBoxTags :: [[Char]] -> Box s -> WH (Box s) s
updateBoxTags tags0 box = do
  -- remove '''
  tags1 <- mapM (expandAttribute box) tags0
  let tags = map (map replaceSlash) tags1
  updateBox (updateBoxTags' $ filter (not . null) tags) box

boxStyleAndContent :: Box s -> String
boxStyleAndContent b = case boxContent b of
  "" -> boxStyle b
  c -> boxStyle b ++ "-" ++ c
  
-- | Box coordinate as if the shelf was full of this box
-- give the offest divide by the dimension of the box + 1
boxCoordinate box  = let (Dimension ol ow oh) = boxOffset box
                         (Dimension l w h) = boxDim box
                         go o d = (o / d) + 1
                     in Dimension (go ol l) (go ow w) (go oh h)
-- | Box attribute can be used to create new tag
-- example, /pending,#previous=$shelfname on a
-- will add the tag previous=pending to all items in the pending shelf
expandAttribute :: Box s -> String -> WH String s
expandAttribute box toExpand = maybe (return toExpand) ($ box) (expandAttribute' toExpand)
-- | Workhorse for expandAttribute. The difference is it actually doesn't need a box
-- to know if it needs expansion or not
-- If an attribute is found, we can safely call expandAttribute (recursively), as we are
-- only interested in doest in need expansion or not
expandAttribute' :: String -> Maybe (Box s -> WH String s)
expandAttribute' ('$':'{':'s':'h':'e':'l':'f':'n':'a':'m':'e':'}':xs) = Just $ \box ->  do
  ex <-  expandAttribute box xs
  case boxShelf box of
    Nothing -> return ex
    Just sId -> do
      shelf <- findShelf sId
      return $ shelfName shelf ++ ex
expandAttribute' ('$':'{':'s':'h':'e':'l':'f':'t':'a':'g':'}':xs) = Just $ \box -> do
  ex <-  expandAttribute box xs
  case boxShelf box of
    Nothing -> return ex
    Just sId -> do
      shelf <- findShelf sId
      return $ fromMaybe "" (shelfTag shelf) ++ ex
expandAttribute' ('$':'{':'o':'l':'}':xs) = Just $ \box -> let (Dimension ol _ _ ) = boxCoordinate box in fmap ((show $ round ol) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'o':'w':'}':xs) = Just $ \box -> let (Dimension _ ow _ ) = boxCoordinate box in fmap ((show $ round ow) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'o':'h':'}':xs) = Just $ \box -> let (Dimension _ _ oh ) = boxCoordinate box in fmap ((show $ round oh) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'s':'t':'y':'l':'e':'}':xs) =  Just $ \box -> fmap ((boxStyle box) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'c':'o':'n':'t':'e':'n':'t':'}':xs) =  Just $ \box -> fmap ((boxContent box) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'b':'o':'x':'n':'a':'m':'e':'}':xs) =  Just $ \box -> fmap ((boxStyleAndContent box) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'c':'o':'o':'r':'d':'i':'n':'a':'t':'e':'}':xs) =  Just $ \box -> let (Dimension ol ow oh) = boxCoordinate box
                                                                                                roundi i = (round i) :: Int
                                                                                            in fmap ((printf "%d:%d:%d" (roundi ol) (roundi ow) (roundi oh)) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'o':'f':'f':'s':'e':'t':'}':xs) =  Just $ \box -> fmap ((printDim $ boxOffset box) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'d':'i':'m':'e':'n':'s':'i':'o':'n':'}':xs) =  Just $ \box -> fmap ((printDim (_boxDim box)) ++) (expandAttribute box xs)
expandAttribute' ('$':'{':'o':'r':'i':'e':'n':'t':'a':'t':'i':'o':'n':'}':xs) = Just $ \box -> fmap (showOrientation (orientation box) ++) (expandAttribute box xs)
expandAttribute' ('$':'[':xs') | (pat', _:xs')<- break (== ']') xs' = Just $ \box -> do
                               ex <- expandAttribute box xs'
                               pat <- expandAttribute box pat'
                               let pre = pat ++ "="
                               return $ maybe ex (++ex) (asum $ map (stripPrefix $ pre) (Set.toList $ boxTags box))
expandAttribute' (x:xs) = fmap (\f b -> (x:) <$> f b) (expandAttribute' xs)
expandAttribute' [] = Nothing

replaceSlash '/' = '\''
replaceSlash c  = c
   
defaultPriority :: Int
defaultPriority = 100
defaultPriorities = (defaultPriority, defaultPriority, defaultPriority)

printDim  (Dimension l w h) = printf "%0.1fx%0.1fx%0.1f" l w h
-- | Convert a set of tags to prioties
-- bare number = content priority, 
-- @number style priority
-- @@number global priority
extractPriorities :: Set String -> (Int, Int, Int)
extractPriorities tags = let
  prioritiess = map extractPriority (Set.toList tags)
  (globals, styles, contents) = unzip3 prioritiess
  go :: [Maybe Int] -> Int
  go ps = fromMaybe defaultPriority $ getLast $ mconcat (map Last (Just 100 : ps))
  in (go globals, go styles, go contents)
   


-- read 0 or more priorite. Priority have the following format
-- content@style@global. Empty priorite are allowed
-- so @@ is a shortcut for global

extractPriority :: String -> (Maybe Int, Maybe Int, Maybe Int)
extractPriority tag = let
  priorities = map readMaybe $ splitOn "@" tag
  [content, style, global]= take 3 $ priorities  <> repeat Nothing
  in (global, style, content)

newSlotTag = "@start-new-slot"
newSliceTag = "@start-new-slice"
newShelfTag = "@start-new-shelf"
extractBoxBreak :: Set String -> Maybe BoxBreak
extractBoxBreak tags | newSlotTag `elem` tags = Just StartNewSlot
                     | newSliceTag `elem` tags = Just StartNewSlice 
                     | newShelfTag `elem` tags = Just StartNewShelf 
                     | otherwise = Nothing

-- * Misc
-- | reorder box so they are ordered by column across all
-- the given shelves.
-- sortAccross :: Shelf' shelf => [shelf s] -> WH [Box s] s
-- sortAccross ss = do
--     -- first we need to remove the boxes from their current location
--     boxes <- concat `fmap` mapM findBoxByShelf ss
--     let nothing = head $ Nothing : map Just ss -- trick to typecheck
--     mapM (assignShelf  nothing) boxes
--     left <- iteration ss boxes
--     s0 <- defaultShelf
--     mapM (assignShelf  (Just s0)) left
--     return (error "FIXME") --  left
--     -- Similar is not

--     where iteration ss oldLeft = do
--               left <- foldM (\left s -> undefined) -- fillShelf ExitOnTop s (Similar left)) oldLeft  ss
--               if Prelude.length left == Prelude.length oldLeft
--               then return left
--               else iteration ss left

usedDepth :: Shelf' shelf => shelf s -> WH (String, Double) s
usedDepth s = do
  boxes <- findBoxByShelf s
  return $ maximumBy (comparing snd) (("<empty>",0)
                         :[(boxStyle b, dWidth (boxOffset' b))
                   | b <- boxes
                   ])




-- * Denormalizing
--
shelfBoxes :: WH [(Shelf s, Box s)] s
shelfBoxes = do
    ss <- mapM findShelf =<< (toList <$> gets shelves)
    sbsS <- mapM (\s -> do bs <- findBoxByShelf s ; return (s, bs)) ss

    return [(s, b) | (s, bs) <- sbsS, b <- bs]


-- * Box corners operation
  {-
extremeCorners :: [Box s] -> [(Double, Double)]
extremeCorners boxes = let
    cs =  [(l+ol, h+oh) | b <- boxes
                        , let Dimension l _ h = boxDim b
                        , let Dimension ol _ oh = boxOffset b
           ]
    -- sort corner by
    cs'  = reverse cs
    in foldr (addCorner) [(0,0)] cs'


-- | Intersect a corner to list of corner
-- The corner is "hidden" if it's smaller than the existing one
addCorner :: Corner -> [Corner] -> [Corner]
addCorner c cs = concatMap (splitCorner c) cs

-- | equivalent to the intersection of 2 rectangles with
-- a top right corner at the infinite
splitCorner :: Corner -> Corner -> [Corner]
splitCorner (cx,cy) (cx',cy')
    | cx <= cx' || cy <= cy' = [(cx', cy')] -- corner shadowed
    | otherwise = [ (cx', cy), (cx,cy')]
-}


-- * Misc

-- | Shelve or box name can have a tag, which is
-- a prefix starting with :
-- example T-shirt#shirt
extractTag :: String -> (String, Maybe String)
extractTag name = let (prefix, suffix) = break (=='#') name
             in case suffix of
                  '#':tag -> (prefix, Just tag)
                  _ -> (prefix, Nothing)

extractTags :: String -> (String, [String])
extractTags name = (style, maybe [] (splitOn "#") tagM) where
  (style, tagM) = extractTag name
