{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
module WarehousePlanner.Base
( applyNameSelector
, applyTagSelectors
, aroundArrangement 
, assignShelf
, bestArrangement
, boxCoordinate
, boxRank
, boxStyleAndContent
, buildWarehouse
, clearCache
, cornerHull
, defaultShelf
, deleteBoxes
, deleteShelf
, emptyWarehouse
, expandAttribute
, expandAttribute'
, extractTag
, extractTags
, filterBoxByTag
, filterShelfByTag
, findBoxByNameAndShelfNames
, findBoxByNameSelector
, findBoxByShelf
, findShelfBySelector
, findShelfBySelectors
, findShelvesByBoxNameAndNames
, howMany, howManyWithDiagonal
, incomingShelf
, indexToOffsetDiag, d0, r
, matchName
, maxUsedOffset
, module WarehousePlanner.Type
, moveBoxes
, negateTagOperations
, newBox
, newShelf
, orTrue
, parseBoxSelector
, parseSelector
, parseTagOperation
, parseTagOperations
, parseTagSelector
, printDim
, shelfBoxes
, stairsFromCorners
, updateBox
, updateBoxTags
, updateShelf
, updateShelfTags
, usedDepth
, withBoxOrientations
)
where
import ClassyPrelude hiding (uncons, stripPrefix)
import Text.Printf(printf)
import qualified Data.Map.Strict as Map'
import qualified Data.Map.Lazy as Map
import Control.Monad.State(gets, get, put, modify)
import Data.Map.Merge.Lazy(merge, preserveMissing, mapMaybeMissing, zipWithMaybeMatched)
-- import Data.List(sort, sortBy, groupBy, nub, (\\), union, maximumBy, delete, stripPrefix, partition)
import Data.List(cycle)
import qualified Data.List as List
import Data.STRef
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import WarehousePlanner.Type
import WarehousePlanner.SimilarBy
import Diagrams.Prelude(white, black, darkorange, royalblue, steelblue)
import Data.Text (splitOn, uncons, stripPrefix)
import Data.Char (isLetter)
import Data.Time (diffDays)

import qualified System.FilePath.Glob as Glob


-- import qualified Debug.Trace as T


-- | Internal types to deal with tag and tag operations
-- we use a parametrized type only to get fmap for free
data TagOperationF s = -- ClearTagValues  use SetValue []
                    SetTag -- no value
                  | RemoveTag 
                  | SetValues [s]
                  | AddValue s
                  | RemoveValue s
                  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type TagOperation = TagOperationF Text
type Tag'Operation = (Text, TagOperation)

-- | Computes the max length or height used within a shelf
-- | by all the boxes. Should be able to be cached.
-- maxUsedOffset :: (Dimension -> Double) -> Shelf -> WH
maxUsedOffset ::  Shelf s -> WH Dimension s
maxUsedOffset shelf = do
    boxes <- findBoxByShelf shelf
    let [l, w, h] = map (\proj -> 
                     foldr (\box r -> max r ((proj.boxOffset') box)) 0 boxes
                    ) [dLength, dWidth, dHeight]
    return $ Dimension l w h

-- | Nested groups of shelves, used for display

__unused_groupToShelfIds :: ShelfGroup s -> [ShelfId s]
__unused_groupToShelfIds (ShelfProxy sid) = [sid]
__unused_groupToShelfIds (ShelfGroup  groups _ ) = concatMap __unused_groupToShelfIds groups


rackUp :: Shelf' shelf => [shelf s] -> ShelfGroup s
rackUp ss = ShelfGroup (reverse g) Vertical where
    g = map (ShelfProxy . shelfId) ss

buildWarehouse :: [[[Text]]] -> WH (ShelfGroup s) s
buildWarehouse xsss = do
    bays <- mapM buildBay xsss
    return $ ShelfGroup bays  Vertical

buildBay :: [[Text]] -> WH (ShelfGroup s) s
buildBay xss = do
    racks <- mapM buildRack xss
    return $ ShelfGroup racks Horizontal

buildRack :: [Text] -> WH (ShelfGroup s) s
buildRack xs = do
    idsS <- mapM ( \x ->  do
            case x of
                (uncons -> Just ('-', name)) ->  fmap (map shelfId) $ updateShelfByName (\s -> s { flow = RightToLeft }) name
                name ->  findShelfBySelector (parseSelector name)
            ) xs
    shelves <- mapM findShelf (concat idsS)
    let sorted = sortBy (comparing shelfName) shelves

    return $ rackUp sorted

 -- -| shelf use as error, ie everything not fitting anywhere
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


findBoxByNameSelector :: (NameSelector (Box s)) -> WH [Box s] s
findBoxByNameSelector selector = do
  boxIds <- toList <$> gets boxes
  filterByNameSelector (mapM findBox boxIds) (boxStyle)  selector

findShelfByBox :: Box' box => box s -> WH (Maybe (ShelfId s)) s
findShelfByBox box' = do
  box <- findBox box'
  return $ boxShelf box

findShelvesByBoxes :: Box' box => [box s] -> WH ([ShelfId s]) s
findShelvesByBoxes boxes = fmap catMaybes (mapM findShelfByBox boxes)

-- | find shelf by name and tag
findShelfBySelector :: Selector (Shelf s) -> WH [ShelfId s] s
findShelfBySelector selector = map shelfId <$> findShelfBySelector' selector
findShelfBySelector' :: Selector (Shelf s) -> WH [Shelf s] s 
findShelfBySelector' (Selector nameSel tagSels ) = do
  shelfIds <- toList <$> gets shelves
  shelves0 <-  filterByNameSelector (mapM findShelf shelfIds) shelfName  nameSel
  let shelves1 = filter (applyTagSelectors tagSels shelfTag) shelves0
  return shelves1
-- | Find shelf by name and tag
-- but respect alternative order
-- ie B|A will return B before A
-- B*|A* will return Bs (sorted alphabetically) the A* (alphabetically)
findShelfBySelectors :: [Selector (Shelf s)] -> WH [ShelfId s] s
findShelfBySelectors selectors = do
  shelvess <- mapM findShelfBySelector' selectors
  return [ shelfId shelf
         | shelves <- shelvess
         , shelf <- sortOn shelfName shelves
         ]


matchName :: Text -> NameSelector s
matchName name = NameMatches [MatchFull name]

filterShelfByTag :: [TagSelector (Shelf s)] -> Shelf s -> Bool
filterShelfByTag selectors shelf = applyTagSelectors selectors shelfTag shelf

filterBoxByTag :: [TagSelector (Box s)]-> Box s -> Bool
filterBoxByTag selectors box =  applyTagSelectors selectors boxTags box

-- | Compiles a match against a glob pattern if Necessary
specials = "*?[]{}" :: String
isGlob :: Text -> Bool
isGlob s = case break (`List.elem` specials) s of
  (_, uncons -> Just _) -> True
  _ -> False

filterByNameSelector :: WH [a s] s -> (a s -> Text) -> (NameSelector (a s)) -> WH [a s] s
filterByNameSelector objects objectName selector = do
   let matcher= applyNameSelector selector objectName
   filter matcher <$> objects

-- | Syntax content^shelf^total
parseBoxNumberSelector :: Text -> BoxNumberSelector
parseBoxNumberSelector "" = BoxNumberSelector Nothing Nothing Nothing
parseBoxNumberSelector s = let
  splits = splitOn "^" s
  parsed = map readMay splits 
  [content, shelves, total] = take 3 $ parsed <> cycle [Nothing]
  in BoxNumberSelector content shelves total

-- | TODO Should be true be seems to work like that
-- this will mean, that we need a normal or 
orTrue :: [Bool] -> Bool
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
findBoxByNameAndShelfNames :: BoxSelector s -> WH [BoxId s] s
findBoxByNameAndShelfNames ( BoxSelector ( Selector (NameMatches [])
                                                    [ TagIsKeyAndValues (MatchFull prop)
                                                                        [MatchFull value]
                                                    ]
                                         ) shelfSel numSel
                           ) = do
  -- optimisation to find box by prop=value
  -- we create a Map and cache it in case it is reused later
  boxMap <- getOrCreateBoxTagMap prop 
  -- TODO factorize with below
  let allBoxes  = fromMaybe [] (lookup value boxMap)
  box'shelfms <- forM allBoxes $ \box -> do
      shelfIdM <- findShelfByBox (boxId box)
      shelfM <- traverse findShelf shelfIdM
      let shelf = case shelfM of
                    Just shelf'  -> if applyNameSelector (nameSelector shelfSel) shelfName shelf'
                                      && applyTagSelectors (tagSelectors shelfSel) shelfTag shelf'
                                    then shelfM
                                    else Nothing
                    _ -> Nothing

      return $ (box, shelf)


  -- we need the shelf name to sort box by shelves
  let box'nameS =  [ (box, shelfName shelf) | (box, Just shelf) <- box'shelfms] 
        
  -- filter boxes by number
  return . map (boxId  . fst) $ limitByNumber numSel box'nameS




findBoxByNameAndShelfNames (BoxSelector boxSel shelfSel numSel) = do
  -- all boxes matching name
  allBoxesBeforeTag <- findBoxByNameSelector (nameSelector boxSel)
  let allBoxes = filter (applyTagSelectors (tagSelectors boxSel) boxTags) allBoxesBeforeTag
  box'shelfms <- forM allBoxes $ \box -> do
      shelfIdM <- findShelfByBox (boxId box)
      shelfM <- traverse findShelf shelfIdM
      let shelf = case shelfM of
                    Just shelf'  -> if applyNameSelector (nameSelector shelfSel) shelfName shelf'
                                      && applyTagSelectors (tagSelectors shelfSel) shelfTag shelf'
                                    then shelfM
                                    else Nothing
                    _ -> Nothing

      return $ (box, shelf)


  -- we need the shelf name to sort box by shelves
  let box'nameS =  [ (box, shelfName shelf) | (box, Just shelf) <- box'shelfms] 
        
  -- filter boxes by number
  return . map (boxId  . fst) $ limitByNumber numSel box'nameS


-- | Limit a box selections by numbers
limitByNumber :: BoxNumberSelector -> [(Box s, Text)] -> [(Box s, Text)]
limitByNumber (BoxNumberSelector contentN shelfN totalN) boxes0 = let
  sorted = sortBy (comparing  $ ((,) <$> boxGlobalPriority <*> boxRank) . fst ) boxes0
  boxes1 = maybe id (limitBy (boxSku . fst)) contentN $ sorted
  boxes2 = maybe id (limitBy snd) shelfN $ boxes1
  boxes3 = maybe id take totalN $ sortBy (comparing  $ ((,) <$> boxGlobalPriority <*> boxRank) . fst ) boxes2
  in boxes3


-- limitBy :: Ord k => (Box s -> k) -> Int -> [Box s] -> [a]
limitBy key n boxes = let
  sorted = sortBy (comparing  $ ((,) <$> boxGlobalPriority <*> boxRank) . fst ) boxes
  group_ = Map'.fromListWith (flip(<>)) [(key box, [box]) | box <- sorted]
  limited = fmap (take n . sortBy (comparing $ boxRank . fst) ) group_
  in concat (Map'.elems limited)
  
-- | Use similar syntax to boxes but returns shelves instead
findShelvesByBoxNameAndNames :: ShelfSelector s -> WH [Shelf s] s
findShelvesByBoxNameAndNames (ShelfSelector SelectAnything shelfSel) = findShelfBySelector shelfSel >>= mapM findShelf
findShelvesByBoxNameAndNames (ShelfSelector (Selector boxNameSel boxTagSel) shelfSel) = do
  shelves0 <- findShelfBySelector shelfSel >>= mapM findShelf
  -- only keep shelf for which boxes are ok
  let keepShelf shelf = do
        boxes0 <- findBoxByShelf shelf
        boxes <- filterByNameSelector (return boxes0) boxStyle boxNameSel
        case boxes of
          [] -> return False -- box selector is not null we need some boxes
          _ -> do
            return $ all (filterBoxByTag boxTagSel) boxes
  filterM keepShelf shelves0



  


defaultBoxOrientations box shelf =
    let orientations = case (shelfBoxOrientator shelf)  of
            (DefaultOrientation) ->  [up, rotatedUp]
            (ForceOrientations orientations_) -> orientations_
            (BoxOrientations) -> case boxBoxOrientations box of
                                    [] ->  [up, rotatedUp]
                                    ors -> ors
            FilterOrientations orientations_ -> boxBoxOrientations box List.\\ orientations_
            AddOrientations lefts_ rights_ -> lefts_ `List.union` boxBoxOrientations box `List.union` rights_
    in map (\o -> OrientationStrategy o 0 1 Nothing Nothing True) orientations

defaultBoxStyling = BoxStyling{..} where
  foreground = black
  background = white
  background2 = Nothing
  border = Nothing
  title = []
  barTitle = Nothing
  displayBarGauge = True
  offsetBarGaugeX = Nothing
  offsetBarGaugeY = Nothing

defaultShelfStyling = ShelfStyling{..} where
  foreground = black
  background = white
  maxBackground = steelblue
  barForeground = black
  barBackground = darkorange
  border = royalblue
  title = []
  barTitle = Nothing
  displayBarGauge = True
emptyWarehouse :: Day -> Warehouse s
emptyWarehouse today = Warehouse mempty mempty mempty
                                 (const defaultBoxStyling)
                                 (const defaultShelfStyling)
                                 defaultBoxOrientations Nothing today

newShelf :: Text -> Maybe Text -> Dimension -> Dimension -> Double -> BoxOrientator -> FillingStrategy -> WH (Shelf s) s
newShelf name tagm minD maxD bottom boxOrientator fillStrat = do
        let -- tags = case splitOn "#" <$> tagm of
            --   Nothing -> mempty
            --   Just [""] -> mempty
            --   Just tags' -> fromMaybe mempty $ modifyTags (map parseTagOperation tags') mempty
            tags = fromMaybe mempty $ fmap parseTagOperations tagm >>= (flip modifyTags mempty)
        warehouse <- get
        ref <- lift (newSTRef (error "should never been called. Base.hs:327"))
        let shelf = Shelf (ShelfId ref) [] name tags minD maxD LeftToRight boxOrientator fillStrat bottom
        lift $ writeSTRef ref shelf

        put warehouse { shelves = shelves warehouse  |> ShelfId ref }
        updateShelfTags [] shelf

newBox :: Shelf' shelf => Text -> Text ->  Dimension -> Orientation -> shelf s  -> [Orientation]-> [Text] -> WH (Box s) s
newBox style content dim or_ shelf ors tagTexts = do
    warehouse <- get
    let tags' = map (parseTagOperation . omap replaceSlash) tagTexts
        dtags = dimensionTagOps dim
        contentTag = (omap replaceSlash $ cons '\'' content, SetTag)
        tags = fromMaybe mempty $ modifyTags (contentTag : tags' <> dtags) mempty
                                  --   ^ apply dimension tags after tags so dimension override tags

    ref <- lift $ newSTRef (error "should never been called. undefined. Base.hs:338")
    let box = Box (BoxId ref) (Just $ shelfId shelf) style content dim mempty or_ ors tags defaultPriorities (extractBoxBreak tags)
    shelf' <- findShelf shelf
    linkBox (BoxId ref) shelf'
    lift $ writeSTRef ref box
    put warehouse { boxes = boxes warehouse |> BoxId ref
                  }
    return box

-- | Parses a string  to Tag operation
-- the general syntax [-]tag[=[-+]value]
-- tag create the tag if n
-- -tag remove the tag
-- tag=val set the value
-- tag=+val or tag+=val add the value to the existing value
-- tag=-val or tag-=val and -tag=val remove the value
parseTagOperation :: Text -> Tag'Operation
parseTagOperation s = 
  case break (== '=') s of
    (uncons -> Just ('-', tag), "")           -> (tag, RemoveTag)
    (tag, "")               -> (tag, SetTag )
    (uncons -> Just ('-', tag), "=")      -> (tag, SetValues [] ) -- clear tag
    (uncons -> Just ('-', tag), (uncons -> Just ('=', val)))      -> (tag, RemoveValue val )
    (uncons -> Just ('-', tag), val)          -> (tag <> val, RemoveTag)
    (tag, (stripPrefix "=-" -> Just val))      -> (tag, RemoveValue val)
    -- (tag, (stripPrefix "-=" -> Just val))      -> (tag, RemoveValue val)
    (tag, (stripPrefix "=+" -> Just val))      -> (tag, AddValue val)
    -- (tag, (stripPrefix "+=" -> Just val))      -> (tag, AddValue val)
    (tag, (uncons -> Just ('=', val)))          -> (tag, SetValues $ split val)
    (tag, val)              -> (tag <> val , SetTag)
  where split = splitOn ";"
 
parseTagOperations :: Text -> [Tag'Operation]
parseTagOperations tag =
 case splitOn "#" tag of
   [] -> []
   [""] -> []
   tags -> map parseTagOperation tags
  
  
  
negateTagOperations :: [Tag'Operation] -> [Tag'Operation]
negateTagOperations tags = do
  (tag, op) <- tags
  map (tag, ) $ case op of
    RemoveTag -> [SetTag]
    SetTag -> [RemoveTag]
    SetValues values -> map RemoveValue values
    RemoveValue val -> [AddValue val]
    AddValue val -> [RemoveValue val]
  
-- | Generates tag operation for dimensions. Used when creating a box
-- to update the dimension tags. 
dimensionTagOps :: Dimension  -> [Tag'Operation]
dimensionTagOps dim = [dshow 'l' dLength, dshow 'w' dWidth, dshow 'h' dHeight]
  where dshow c f = ( pack $ '\'' : c : []
                    , SetValues [tshow (floor $ 100 * f dim)]
                    )

-- | Extract new dimensions from tags
-- use given dimension for missing elements
-- return Nothing, if nothing has changed, allowing optimisation upstream
extractDimensions :: Dimension -> Tags -> Maybe Dimension
extractDimensions dim tags = case (go "'l", go "'w", go "'h") of
  (Nothing, Nothing, Nothing) -> Nothing
  (l, w, h) -> Just dim { dLength = fromMaybe (dLength dim) l
                                , dWidth = fromMaybe (dWidth dim) w
                                , dHeight = fromMaybe (dHeight dim) h
                                }
  where go prefix = case traverse readMay $ maybe [] (Set.toList) (Map.lookup prefix tags) of
          Nothing -> Nothing
          Just values -> Just $ sum values /100

-- | Change the dimension of the box according to its tag
updateDimFromTags :: Box s -> Box s
updateDimFromTags box = case extractDimensions (_boxDim box) (boxTags box) of
  Nothing -> box
  Just dim -> box { _boxDim = dim  }

 
-- | Assign a box to shelf regardless of if there is enough space
-- left or not.
-- For a "real" move see moveBox
assignShelf :: (Box' box,  Shelf' shelf) => Maybe (shelf s) -> box s -> WH () s
assignShelf s box0 = do
    box <- findBox box0
    oldShelfM <- traverse findShelf (boxShelf box)
    newShelfM <- traverse findShelf s

    -- if box belong to a shelf
    -- we need to update the shelf to remove the link to the box
    when (oldShelfM /= newShelfM) $ do
      mapM_ (unlinkBox (boxId box)) oldShelfM
      mapM_ (linkBox (boxId box)) newShelfM
      _ <- updateBox (\box_ -> box_ { boxShelf = shelfId `fmap` s }) box
      return ()

-- | Unlink a box from a shelf without
-- checking anything. Shoudn't be exported
unlinkBox :: BoxId s -> Shelf s -> WH () s
unlinkBox box shelf = do
  let boxes = _shelfBoxes shelf
      boxes' = delete box boxes

      shelf' = shelf { _shelfBoxes = boxes' }
  _ <- updateShelf (const shelf') shelf'
  return ()

-- | link a box from a shelf without
-- checking anything. Shoudn't be exported
linkBox :: BoxId s -> Shelf s -> WH () s
linkBox box shelf = do
  let boxes = _shelfBoxes shelf
      boxes' = box:boxes

      shelf' = shelf { _shelfBoxes = boxes' }
  _ <- updateShelf (const shelf') shelf'
  return ()

deleteBoxes :: [BoxId s] -> WH [Box s] s
deleteBoxes boxIds = do
  deleted <- forM boxIds $ \boxId_ -> do
                box <- findBox boxId_
                oldShelfM <- traverse findShelf (boxShelf box)
                mapM_ (unlinkBox boxId_) oldShelfM
                return box
  wh <- get
  put wh { boxes = Seq.fromList $ (toList $ boxes wh ) List.\\ boxIds }
  return deleted
    


deleteShelf :: ShelfId s -> WH () s
deleteShelf shelfId = do
  findBoxByShelf shelfId >>= deleteBoxes . map boxId
  warehouse <- get
  put warehouse { shelves = filter (/= shelfId) $ shelves warehouse }
  

-- | find the best way to arrange some boxes within the given space
-- For the same number of boxes. use the biggest diagonal first, then  the smallest shelf
bestArrangement :: Show a => [OrientationStrategy] -> [(Dimension, Dimension, a)] -> Dimension -> (Orientation, Diagonal, Int, Int, Int, a)
bestArrangement orientations shelves box = let
    minMaybe minm x = fromMaybe x  $ fmap (min x) minm
    options = [ (o, diag, extra, (minMaybe maxLM nl, max minW (min nw maxW), minMaybe maxHM nh), sl*sw*sh)
              | (OrientationStrategy o  minW  maxW maxLM maxHM useDiag) <-   orientations
              , (minShelf, shelf, extra) <- shelves
              , let Dimension sl sw sh =  shelf
              , let ((nl, nw, nh),diag) = if useDiag
                                   then  howManyWithDiagonal minShelf shelf (rotate o box)
                                   else (howMany minShelf shelf (rotate o box), Diagonal 0)
              ]

    bests = sortBy (compare `on` fst)
                [ ( ( -nl*nh*nw
                    , nl -- ^ minimize length
                    -- , bh*bh*fromIntegral(nh*nh)+bl*bl*fromIntegral(nl*nl)
                    , vol
                    )
                  , (ori, diag, nl, nw, nh, extra)

                  )
                 | (ori, diag, extra, (nl, nw, nh), vol ) <- options
                 -- , let Dimension bl bh _bw = rotate ori box
                 ]
    in
        -- trace ({-show shelves ++ show box ++-}  show bests) $
        (snd . headEx) bests


-- | * test
{-
dx = Dimension 192 100 145
dn = Dimension 160 100 145
box = Dimension 47 39 85
os = [tiltedForward, tiltedFR]
-}


-- | How many inner rectangle can fit in the outer one ?
-- The outer min corresponds to the maximum of the n-1 box
-- This allows to set for example a maximum height for the bottom the last box
-- this is the maximum height a operator can reach the box
-- and the actual height of the shelf or ceiling, the physical limit.
--
--   max ----------------------
--           2    X   
--           2    X
--   min ____2____2___________
--           1    2
--           1    1
--           1    1
--
-- X is accepted even though it fit within max
-- but starts after min
--
howMany :: Dimension --  ^ Outer min
        -> Dimension -- ^ Out max
        -> Dimension --  ^ Inner
        -> (Int, Int, Int)
howMany (Dimension l0 w0 h0) (Dimension l w h) (Dimension lb wb hb) = ( fit l0 l lb
                                                 , fit w0 w wb
                                                 , fit h0 h hb
                                                 ) where
        fit d0 d1 db = 
          let d = min (d0+db) d1
          in floor (max 0 (d-0) /(db+0))


-- | Find how many boxes fits using a "diagnal trick", when one box is rotated the other along a diagoral.
-- For example
--    1477
--    14 8
--    2558
--    2 69
--    3369
--  3, 5 and 7 box are rotated down allowing f
howManyWithDiagonal :: Dimension -> Dimension -> Dimension -> ((Int, Int, Int), Diagonal)
howManyWithDiagonal minOuter outer@(Dimension l _ h) inner@(Dimension lb _ hb) =
  let normal@(ln, wn, hn) = howMany minOuter outer inner
      fit d db = floor (max 0 (d-0) /(db+0))
      -- how many feet for a given size of a square
      nForDiag n =
         --  | = = | = =
         --  = = | = = |     1 square x 2
         --  = | = = | =
         --  | = = | = =
         let squareL = fromIntegral (n-1)*lb+hb
             squareH = fromIntegral (n-1)*hb+lb
             sqNL = fit l squareL
             sqNH = fit h squareH
             leftL = l - squareL * fromIntegral sqNL
             leftH = h - squareH * fromIntegral sqNH
             mb = max lb hb
             -- find how many row/column can we use
             -- in a partial square
             -- We use mb because the first row/column
             -- will use both orientation for one of the box
             leftOver remaining b =
              if remaining < mb
              then 0
              else -- traceShow ("Remainiing", remaining, remaining-mb, "b" , b)
                   -- $ traceShowId
                   fit (remaining -  mb) b +1
              
         in ((sqNL * n + leftOver leftL lb
             , wn
             , sqNH * n + leftOver leftH hb
             ) , Diagonal n)
      options = [nForDiag i | i <- [2.. (1 + min ln hn)] ]
      bests = sortOn (\((nl', nw', nh'), _diag) -> (-nl'*nw'*nh', nl'))
                     $ (normal, Diagonal 0): options
  in if outer /= minOuter
     then (normal, Diagonal 0)
     else case bests of
      [] -> error "Shouldn't happen"
      (best:_) -> best


        















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
          -> PartitionMode
          -> shelf s
          -> SimilarBoxes s
          -> WH ( Maybe (SimilarBoxes s)
                , Maybe (Shelf s)
                )  s
fillShelf exitMode partitionMode s simBoxes0 = do
    let simBoxes@(SimilarBy dim box bs) = sortSimilarOn boxRank simBoxes0
    shelf <- findShelf s
    let boxes = box : bs
    -- first we need to find how much space is left
    Dimension lused wused hused <- maxUsedOffset shelf
    boxesInShelf <- findBoxByShelf shelf
    case  (boxBreak box, lused*hused*wused > 0) of
      (Just StartNewShelf, True ) -> return (Just simBoxes, Nothing ) -- shelf non empty, start new shelf
      _ -> do
        boxo <- gets boxOrientations
        let orientations = boxo box shelf
        let (bestO, diag, nl_, nw, nh, (lused', hused')) =
                            bestArrangement orientations
                                              [( minDim shelf <> used, maxDim shelf <> used, (l,h))
                                              -- [ (Dimension (max 0 (shelfL -l)) shelfW (max 0 (shelfH-h)), (l,h))
                                              -- | (Dimension shelfL shelfW shelfH) <- [ minDim shelf, maxDim shelf ]
                                            -- try min and max. Choose min if  possible
                                              | (l,h) <- let go pmode =
                                                                case pmode of
                                                                  PAboveOnly -> [(0,hused)]
                                                                  PRightOnly -> [(lused, 0)]
                                                                  PBestEffort -> case bestEffort boxesInShelf of
                                                                                  -- remove corners if more than 3 options
                                                                                  xs@(_:_:_:_) -> drop 1 $ dropEnd 1 $ xs
                                                                                  xs -> xs
                                                                  POr m1 m2 -> go m1 ++ go m2
                                                         in go partitionMode
                                              , let used = Dimension (min 0 (0-l)) 0 (min 0 (0-h))
                                              ] dim
            nl = if exitMode == ExitLeft then nl_ else min 1 nl_
            rotated@(Dimension l' w' h') = rotate bestO dim
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
                       -- \^ with the current algorithm only looking
                       -- at the length and height used (and ignoring the depth )
                       -- we need to fill the depth (width) first, whichever filling row or column first
                       -- this might not be the expected Deadzone behavior but until we try
                       -- to find the biggest brick available (instead of the biggest 2d rectangle)
                       -- it seems a better solution
                       -- if modified modify assignOffsetWithBreaks accordingly
                      ]
            -- but within the box potentially move 
            box'Offsets0 = assignOffsetWithBreaks (shelfFillingStrategy shelf) Nothing boxes offsets
            box''Offset'Diags = if isDiagonal diag
                              then map adjustDiagonal box'Offsets0
                              else map (,bestO) box'Offsets0
            adjustDiagonal (box, offset) =
              let trans = Dimension lused' 0 hused'
                  indices = offsetToIndex trans rotated offset
                  (new, turned) = indexToOffsetDiag trans rotated diag indices
                  newOrientation = if turned 
                                   then rotateO bestO
                                   else bestO
              in ((box, new), newOrientation)
                  
                                

        -- traceShowM("Found break", mapMaybe (boxBreak . fst) box'Offset, breakm)
        mapM_ (\((box, offset), ori) -> shiftBox ori box offset) box''Offset'Diags
        let leftm = dropSimilar (length box'Offsets0) simBoxes
        case (box'Offsets0, exitMode) of
            ([], _) -> return (leftm, Nothing) -- we can't fit any. Shelf is full
            (_ , ExitOnTop) -> return (leftm, Just shelf) --  ^ exit on top, we stop there, but the shelf is not full
            (_, ExitLeft) -> return (leftm, Nothing)  --  ^ pretends the shelf is full
            -- _ ->  fillShelfm exitMode  shelf leftm --  ^ try to fit what's left in the same shelf

    where shiftBox ori box' offset = do
            _ <- updateBox (\box_ -> box_ { orientation = ori
                               , boxOffset = offset}) box'
            assignShelf (Just s) box'
          -- fillShelfm x s Nothing = return (Nothing, Just s)
          -- fillShelfm x s (Just lefts_) = fillShelf x s lefts_
            

-- ^ Transform a position to a index given a translation and a box dimension
offsetToIndex :: Dimension -> Dimension -> Dimension -> (Int, Int, Int)
offsetToIndex (Dimension tl tw th) (Dimension l w h) (Dimension ol ow oh) =
  ( round ((ol - tl) / l)
  , round ((ow - tw) / w)
  , round ((oh - th) / h)
  )

_not_used_indexToOffset :: Dimension -> Dimension -> (Int, Int, Int) -> Dimension
_not_used_indexToOffset (Dimension tl tw th) (Dimension l w h) (il, iw, ih) =
  Dimension (fromIntegral il * l + tl)
            (fromIntegral iw * w + tw)
            (fromIntegral ih * h + th)

d0, r :: Dimension
d0 = Dimension 0 0 0
r = Dimension 13 1 10
-- | Computes position and orientation of a box within a "Diagonal" pattern
-- see `howManyWithDiagonal`
indexToOffsetDiag :: Dimension -> Dimension -> Diagonal -> (Int, Int, Int) -> (Dimension, Bool)
indexToOffsetDiag (Dimension tl tw th) (Dimension l w h) (Diagonal diagSize) (il, iw, ih) =
  let (lq, lr) = il `divMod`  diagSize
      (hq, hr) = ih `divMod` diagSize
      --    
      --    b   X|    X
      --      X  |  X c
      --    X   a|X  
      -- lq  0    1     2
      --    
      -- X) turned
      -- a) (il, ih) = (2,0)
      --    (lq, lr) = (0,2) 1 before
      --    (hq, hr) = (0,0) 0 below
      --    
      --
      -- c) (il, ih) = (5,1)
      --    (lq, lr) = (1,2) 2 before
      --    (hq, hr) = (0,1) 0 below
      (turnedBefore, turnedBelow, turned) =
        if lr > hr
        then -- left of the diagonal turnedBelow +1
          (lq+1, hq, False)
        else if lr ==  hr -- on the diagnoal
        then
          (lq, hq, True)
        else -- right of diagonal
          (lq, hq+1, False)
  in  (Dimension (tl + fromIntegral (il-turnedBefore) * l + fromIntegral turnedBefore*h)
                 (tw + fromIntegral iw * w)
                 (th + fromIntegral (ih - turnedBelow) * h + fromIntegral turnedBelow*l)
      , turned
      )
          

-- |  Assign offset to boxes so they can be moved
-- taking  boxBreak into account. Basically a zip but can skip some offset or break
assignOffsetWithBreaks :: FillingStrategy -> Maybe Dimension ->   [Box s] -> [Dimension] -> [(Box s, Dimension)]
assignOffsetWithBreaks _ _ [] _  = []
assignOffsetWithBreaks _ _ _ []  = []
assignOffsetWithBreaks strat Nothing (box:bs) (offset:os) =  (box, offset) : assignOffsetWithBreaks strat (Just offset) bs os -- ignore the first break
assignOffsetWithBreaks strat (Just previous) bs@(box:_) os@(__offset:_) = case boxBreak box of
  Nothing -> assignOffsetWithBreaks strat Nothing bs os
  Just StartNewShelf -> [] -- break
  Just StartNewSlice -> assignOffsetWithBreaks strat Nothing bs (dropWhile sameRow os)
  Just StartNewSlot -> assignOffsetWithBreaks strat Nothing bs (dropWhile sameSlot os)
  where sameSlot o = case strat of
          _ColumnFirst -> dHeight o <= dHeight previous  && dLength o <= dLength previous
          -- RowFirst -> dHeight o <= dHeight previous  && dWidth o <= dWidth previous
        sameRow o =  case strat of
          ColumnFirst -> dLength o <= dLength previous -- -|| dWidth o <= dWidth previous
          RowFirst -> dHeight o <= dHeight previous -- -|| dWidth o <= dWidth previous







-- Try to Move a block of boxes  into a block of shelves.
-- Boxes are move in sequence and and try to fill shelve
-- in sequence. If they are not enough space the left boxes
-- are returned.
moveBoxes :: (Box' box , Shelf' shelf) => ExitMode -> PartitionMode -> [box s] -> [shelf s] -> WH [Box s] s

moveBoxes exitMode partitionMode bs ss = do
  boxes <- mapM findBox bs
  let layers = groupBy ((==)  `on` boxBreak) $ sortBy (comparing boxGlobalRank) boxes
      boxGlobalRank box = (boxGlobalPriority box, boxStyle box, boxStylePriority box,  _boxDim box)
      boxBreak box = (boxStyle box, _boxDim box)
      -- \^ we need to regroup box by style and size
      -- However we take into the account priority within the style before the dimension
      -- so that we can set the priority
        
  lefts_ <- forM layers $ \layer -> do
    let groups = groupSimilar _boxDim layer
    -- forM groups $ \(SimilarBy dim _ boxes) -> traceShowM ("  GROUP", dim, 1 + length boxes)
    -- traceShowM ("GRoups", length groups, map (\(SimilarBy dim g1 g ) -> (show $ length g + 1, show . _roundDim $ dim )) groups)
    lefts' <- mapM (\g -> moveSimilarBoxes exitMode partitionMode g ss) groups
    return $ concatMap unSimilar $ catMaybes lefts'
  return $ concat lefts_

_roundDim :: Dimension -> [Int]
_roundDim (Dimension l w h) = map (round . (*100)) [l,w,h]

 
-- | Move boxes of similar size to the given shelf if possible
moveSimilarBoxes :: (Shelf' shelf) => ExitMode -> PartitionMode -> SimilarBoxes s -> [shelf s] -> WH (Maybe (SimilarBoxes s)) s
moveSimilarBoxes exitMode partitionMode bs ss = moveSimilarBoxesAndRetry exitMode partitionMode bs ss []

-- | moves boxes into give shelf and retry nonfull shelves until all necessary
-- useful to fill selves using ExitONTop strategy
moveSimilarBoxesAndRetry :: (Shelf' shelf) => ExitMode -> PartitionMode -> SimilarBoxes s -> [shelf s] -> [shelf s] -> WH (Maybe (SimilarBoxes s)) s
moveSimilarBoxesAndRetry _ _ boxes [] [] = return (Just boxes)
moveSimilarBoxesAndRetry exitMode partitionMode bs [] trieds = moveSimilarBoxesAndRetry exitMode partitionMode bs (reverse trieds) [] -- can loop but normally ss is null
moveSimilarBoxesAndRetry exitMode partitionMode boxes  (s:ss') trieds = do
    left <- fillShelf exitMode partitionMode s boxes
    case left of
        (Nothing , _) -> return Nothing
        (Just bs', Nothing)  -> moveSimilarBoxesAndRetry exitMode partitionMode bs' (ss') trieds -- discard current Shelf
        (Just bs', Just _)  -> moveSimilarBoxesAndRetry exitMode partitionMode bs' ss' (s:trieds)


boxRank :: Box s -> (Text, Int, Text, Int)
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
rearrangeShelves :: PartitionMode -> Shelf' shelf => [shelf s] -> WH [Box s] s
rearrangeShelves pmode ss = do
    -- first we need to remove the boxes from their current location
    boxes <- concat `fmap` mapM findBoxByShelf ss
    let nothing = headEx $ Nothing: map Just ss -- trick to force type
    mapM_ (\box -> assignShelf  nothing box ) boxes
    left <- moveBoxes ExitLeft pmode boxes ss
    s0 <- defaultShelf
    mapM_ (assignShelf (Just s0)) left

    return left

-- | Remove boxes for shelvese and rearrange
-- shelves before doing any move
-- aroundArrangement  :: WH a -> WH a
aroundArrangement :: (Shelf' shelf, Box' box, Box' box2)
                  => Maybe PartitionMode
                  -> ([box s] -> [shelf s] -> WH [box2 s] s)
                  -> [box s] -> [shelf s] -> WH [box2 s] s
aroundArrangement pmodeM arrangement boxes shelves = do
    let shelfIds = map shelfId shelves
    oldShelveIds <- findShelvesByBoxes boxes
    -- remove all boxes from their actuall shelf
    let nothing = headEx $ Nothing : map Just shelves -- trick to typecheck
    mapM_ (assignShelf nothing) boxes
    -- rearrange what's left in each individual space
    -- so that there is as much space left as possible
    let os = List.nub $ oldShelveIds ++ shelfIds


    mapM_ (\pmode -> 
      mapM_ (\s -> rearrangeShelves pmode s >> return ()) (map (:[]) os)
          ) pmodeM

    left <- arrangement boxes shelves
    s0 <- defaultShelf
    mapM_ (assignShelf (Just s0)) left
    return left









updateBox :: (Box' box) =>  (Box s ->  Box s) -> box s-> WH (Box s) s
updateBox f box0 = do
    box <- findBox box0
    let box' = f box
    lift $ writeSTRef (getRef box') box'
    return box'

updateShelf :: (Shelf' shelf) => (Shelf s -> Shelf s ) -> shelf s -> WH (Shelf s) s
updateShelf f s =  do
    shelf <- findShelf s
    let shelf' = f shelf
    lift $ writeSTRef (getRef shelf') shelf'
    return shelf'


updateShelfByName :: (Shelf s -> Shelf s) -> Text -> WH [Shelf s] s
updateShelfByName f n = findShelfBySelector (Selector (NameMatches [MatchFull n]) [] ) >>= mapM (updateShelf f)


-- | Add or remove the given tags to the give box
updateBoxTags' :: [Tag'Operation] -> Box s -> Box s
updateBoxTags' [] box = box -- no needed but faster, because we don't have to destruct and 
updateBoxTags' tag'ops box = case modifyTags tag'ops (boxTags box) of
  Nothing -> box
  Just new -> updateDimFromTags box { boxTags = new
                                    , boxPriorities = extractPriorities new (boxPriorities box)
                                    , boxBreak = extractBoxBreak new
                                    }

updateShelfTags' :: [Tag'Operation] -> Shelf s -> Shelf s
updateShelfTags' [] shelf = shelf -- no needed but faster, because we don't have to destruct and 
updateShelfTags' tag'ops shelf = case modifyTags tag'ops (shelfTag shelf) of
  Nothing -> shelf
  Just new -> updateCeiling $ shelf { shelfTag = new }
  where updateCeiling s =
          let heightMax  = dHeight (maxDim s)
              reduce extra dim = if dHeight dim <= extra
                                 then -- too small, we can't set the height to 0
                                      -- so we need to set width to 0 to make it
                                      -- unusable
                                    dim { dWidth = 0, dHeight = 1 }
                                 else
                                    dim {dHeight = dHeight dim - extra }
          in case getTagValuem s "ceiling" >>= readMay of 
              Just ceiling | ceiling < heightMax + bottomOffset s
                    ->   let extra  = heightMax + bottomOffset s - ceiling
                         in  s { shelfTag = Map.insert "'tooHigh" mempty (shelfTag s) 
                               , maxDim = reduce extra $ maxDim s
                               , minDim = reduce extra $ minDim s
                               }
              _ -> s

-- | Update the value associateda to a tag operation. Return Nothing if the tag needs to be destroyed
applyTagOperation :: TagOperation -> (Set Text) -> Maybe (Set Text)
applyTagOperation RemoveTag _ = Nothing 
applyTagOperation SetTag olds = Just olds
applyTagOperation (SetValues news) _ = Just $ Set.fromList news
applyTagOperation (AddValue value) olds = Just $ Set.insert value olds 
applyTagOperation (RemoveValue value) olds = Just $ Set.delete value olds

applyTagOperations :: [TagOperation] -> (Set Text) -> Maybe (Set Text)
applyTagOperations tag'ops tags = foldM (flip applyTagOperation) tags tag'ops


-- | Apply tag operations to a set of tags. Return nothing
-- if nothing has changed. Knowing nothing has changed should
-- allow some optimization upstream
modifyTags :: [Tag'Operation] -> Tags -> Maybe Tags
modifyTags [] __tags = Nothing
modifyTags tag'ops tags = Just $ merge  opsOnly tagsOnly tagsAndOp tag'opsMap tags where
    tagsOnly = preserveMissing
    opsOnly = mapMaybeMissing $ \_ ops -> applyTagOperations ops mempty
    tagsAndOp = zipWithMaybeMatched $ \_ -> applyTagOperations
    -- tagoperation should be applied in order so we should be able to put them in a map
    -- however every key works independently of the othere, so at least
    -- as we group_ each operation by key and respect the order, this should be
    tag'opsMap :: Map.Map Text [TagOperation]
    tag'opsMap = Map.fromListWith (<>)  (map (fmap (:[])) tag'ops)

updateBoxTags :: [Tag'Operation] -> Box s -> WH (Box s) s
updateBoxTags tags0 box = do
  -- remove '''
  tags1 <- mapM (mapM $ mapM (expandAttribute box)) tags0
       --                 ^--  each value in Operation
       --     ^    ^-- each value of the TagOperation
       --     +------ snd of the (,)
  let tags = [ (replaceSlashes tag, fmap replaceSlashes values )
             | (tag, values) <- tags1
             ]
      replaceSlashes = omap replaceSlash
  updateBox (updateBoxTags' tags) box

updateShelfTags :: [Tag'Operation] -> Shelf s -> WH (Shelf s) s
updateShelfTags tags0 shelf =  do
  let tags = [ (replaceSlashes tag, fmap replaceSlashes values )
             | (tag, values) <- tags0
             ]
      replaceSlashes = omap replaceSlash
  updateShelf (updateShelfTags' tags) shelf

boxStyleAndContent :: Box s -> Text
boxStyleAndContent box = case boxContent box of
  "" -> boxStyle box
  c -> boxStyle box ++ "-" ++ c
  
-- | Box coordinate as if the shelf was full of this box
-- give the offest divide by the dimension of the box + 1
boxCoordinate :: Box s -> Dimension
boxCoordinate box  = let (Dimension ol ow oh) = boxOffset box
                         (Dimension l w h) = boxDim box
                         go o d = (o / d) + 1
                     in Dimension (go ol l) (go ow w) (go oh h)
-- | Box attribute can be used to create new tag
-- example, /pending,#previous=$shelfname on a
-- will add the tag previous=pending to all items in the pending shelf
expandAttribute :: Box s -> Text -> WH Text s
expandAttribute box toExpand = maybe (return toExpand) ($ box) (expandAttribute' toExpand)
-- | Workhorse for expandAttribute. The difference is it actually doesn't need a box
-- to know if it needs expansion or not
-- If an attribute is found, we can safely call expandAttribute (recursively), as we are
-- only interested in doest in need expansion or not
expandAttribute' :: Text -> Maybe (Box s -> WH Text s)
expandAttribute' (stripPrefix "${shelfname}" -> Just xs) = Just $ \box ->  do
  ex <-  expandAttribute box xs
  case boxShelf box of
    Nothing -> return ex
    Just sId -> do
      shelf <- findShelf sId
      return $ shelfName shelf ++ ex
expandAttribute' (stripPrefix "${shelftags}" -> Just xs) = Just $ \box -> do
  ex <-  expandAttribute box xs
  case boxShelf box of
    Nothing -> return ex
    Just sId -> do
      shelf <- findShelf sId
      return $ (intercalate "#" . flattenTags $ shelfTag shelf) <> ex
expandAttribute' (stripPrefix "${fit}" -> Just xs) = Just $ \box -> do
  ex <-  expandAttribute box xs
  case boxShelf box of
    Nothing -> return ex
    Just sId -> do
      shelf <- findShelf sId
      let   Dimension xn yn zn = minDim shelf
            Dimension xx yx zx = maxDim shelf
            -- Dimension xx yx zx = maxDim shelf
            Dimension l w h = boxDim box
            Dimension ox oy oz = boxOffset box
            fit = case ( (ox+l) > xn || (oy+w) > yn || (oz+h) > zn  
                          , (ox+l) > xx || (oy+w) > yx || (oz+h) > zx
                          ) of
                      (_, True) -> "out"
                      (True, False) -> "tight"
                      (False, False) -> "fit"
      return $ fit ++ ex

expandAttribute' (stripPrefix "${ol}" -> Just xs) = Just $ \box -> let (Dimension ol _ _ ) = boxCoordinate box in fmap ((tshow $ round ol) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${ow}" -> Just xs) = Just $ \box -> let (Dimension _ ow _ ) = boxCoordinate box in fmap ((tshow $ round ow) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${oh}" -> Just xs) = Just $ \box -> let (Dimension _ _ oh ) = boxCoordinate box in fmap ((tshow $ round oh) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${@}" -> Just xs) = Just $ \box -> let (global, style, content) = boxPriorities box in fmap ((tshow content <> "@" <> tshow style <> "@" <> tshow global) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${@content}" -> Just xs) = Just $ \box -> fmap ((tshow $ boxContentPriority box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${@style}" -> Just xs) = Just $ \box -> fmap ((tshow $ boxStylePriority box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${@global}" -> Just xs) = Just $ \box -> fmap ((tshow $ boxGlobalPriority box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${style}" -> Just xs) =  Just $ \box -> fmap ((boxStyle box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${content}" -> Just xs) =  Just $ \box -> fmap ((boxContent box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${boxname}" -> Just xs) =  Just $ \box -> fmap ((boxStyleAndContent box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${coordinate}" -> Just xs) =  Just $ \box -> let (Dimension ol ow oh) = boxCoordinate box
                                                                                roundi i = (round i) :: Int
                                                                            in fmap ((pack $ printf "%d:%d:%d" (roundi ol) (roundi ow) (roundi oh)) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${offset}" -> Just xs) =  Just $ \box -> fmap ((printDim $ boxOffset box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${dimension}" -> Just xs) =  Just $ \box -> fmap ((printDim $ _boxDim box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "${orientation}" -> Just xs) = Just $ \box -> fmap (showOrientation (orientation box) <>) (expandAttribute box xs)
expandAttribute' (stripPrefix "$[" -> Just xs'') | (pat', uncons -> Just (_,xs'))<- break (== ']') xs'' = Just $ \box -> do
                               ex <- expandAttribute box xs'
                               pat <- expandAttribute box pat'
                               return $ maybe ex (<> ex) (getTagValuem box pat)
-- get the rank of the tag value
expandAttribute' (stripStatFunction "$rank" -> Just (arg, prop, xs)) = Just $ \box -> do
  expandStatistic valueRank arg box prop xs
expandAttribute' (stripStatFunction  "$index" -> Just (arg, prop, xs)) = Just $ \box -> do
  expandStatistic valueIndex arg box prop xs
-- | convert date to days since today
expandAttribute' (stripStatFunction "$ago" -> Just (arg, prop, xs) ) = Just  $ \box -> do
  maxDate <- gets whDay
  let valuem = getTagValuem box prop
  stats <- propertyStatsFor prop
  let dates = keys (valueIndex stats)
  ex <- expandAttribute box xs
  return $ case (readMay =<< minimumMay dates, readMay =<< valuem) of
    (Just minDate, Just currentDate) -> let
      daysAgo = diffDays maxDate currentDate
      range = diffDays maxDate minDate
      d = case arg of
        Just ('-', 0) -> -- normalize a year to n
               (daysAgo `div` 365) + 1
        Just ('-', n) -> -- normalize range to n
               min (daysAgo * fromIntegral n `div` range + 1) (fromIntegral n)
        Just ('%', 0) -> -- normalize a year to n
               (daysAgo -1) `mod` 365 + 1
        Just ('%', n) -> -- normalize a year to n
               (daysAgo -1) * fromIntegral n `mod` range + 1
        Just ('^', 0) -> case daysAgo of -- log day, week etc
          d' | d' <= 7 -> 1 -- last week
          d' | d' <= 31 -> 2 -- last month
          d' | d' <= 91 -> 3 -- last quarter
          d' | d' <= 183 -> 4 -- last 6 months
          d' | d' <= 365 -> 5 -- last year
          d' | d' <= 3*365 -> 6 -- last 3 years
          _ -> 7
        Just ('^', n) -> let -- log 
                 daysAgo' = fromIntegral daysAgo
                 n' = fromIntegral n
                 range' = fromIntegral range
                 lambda = range' / log n' :: Double
                 in min (fromIntegral n) $ round $ exp (lambda *  daysAgo')
        _ -> daysAgo
      in tshow (d :: Integer) <> ex
    _ -> "<not a date>" <> ex


expandAttribute' (uncons -> Just (x, xs)) = fmap (\f box -> (cons x) <$> f box) (expandAttribute' xs)
expandAttribute' ("") = Nothing
expandAttribute' _ = error "Bug. All pattern should be caught"

expandStatistic :: (PropertyStats -> Map Text Int) -> Maybe (Char, Int) -> Box s -> Text -> Text -> WH Text s
expandStatistic fn arg box prop xs = do
  let values = getTagValues box prop
  stats <- propertyStatsFor prop
  ex <- expandAttribute box xs
  return $ case values of
    [] -> ex
    (value:_) -> let
      adjust i = case arg of
        Just ('-', n) -> min i n
        Just ('%', n) -> (i-1) `mod` n + 1
        Just ('^', n) -> round $ fromIntegral i / fromIntegral (totalCount stats) * fromIntegral n
        _ -> i
      in maybe ex (\i -> tshow (adjust i) <> ex) (lookup value $ fn stats) 

replaceSlash '/' = '\''
replaceSlash c  = c
   
defaultPriority :: Int
defaultPriority = 100
defaultPriorities = (defaultPriority, defaultPriority, defaultPriority)

-- | Parse strat function name and its parameter
-- example
--  - $rank
--  - $rank-100 - normalize to 100
--  - $rank^100 - cut to 99 and everything above = 100
--  - $rank%100 - modulo 100

stripStatFunction :: Text --  ^ prefix
                  -> Text --  ^ text  to parse
                  -> Maybe (Maybe (Char, Int) -- operator number
                           , Text
                           , Text) -- left over
stripStatFunction prefix (stripPrefix prefix -> Just xs')  = do
  let (t', uncons -> Just (_, xs)) = break (==']') xs'
      (pre, prop0) = break (=='[') t'
  (_, prop) <- uncons prop0
  case pre of
    (uncons -> Just (op, argtext)) | not (isLetter op) , Just arg <- readMay argtext -> Just (Just (op, arg), prop, xs)
    "" -> Just (Nothing, prop, xs)
    _ -> Nothing
stripStatFunction _ _ = Nothing

printDim :: Dimension -> Text
printDim  (Dimension l w h) = pack $ printf "%0.1fx%0.1fx%0.1f" l w h
-- | Convert a set of tags to prioties
-- bare number = content priority, 
-- @number style priority
-- @@number global priority
extractPriorities :: Tags -> (Int, Int, Int) -> (Int, Int, Int)
extractPriorities tags (g0, s0, c0) = let
  go key p0 = fromMaybe p0 $ extractPriority key tags 
  in (go "@global" g0 , go "@style" s0, go "@content" c0 )



extractPriority :: Text -> Tags -> Maybe Int
extractPriority key tags = do
  set <- Map.lookup key tags
  case mapMaybe readMay (toList set) of
    (n:_) -> Just n
    _ -> Nothing


extractBoxBreak :: Tags -> Maybe BoxBreak
extractBoxBreak tags = case maybe [] (Set.toList) (Map.lookup "@start" tags) of
  ["new-slot"] -> Just StartNewSlot
  ["new-slice"] -> Just StartNewSlice 
  ["new-shelf"] ->Just StartNewShelf 
  _ -> Nothing

-- * Misc 
-- | reorder box so they are ordered by column across all
-- the given shelves.
-- sortAccross :: Shelf' shelf => [shelf s] -> WH [Box s] s
-- sortAccross ss = do
--     -- first we need to remove the boxes from their current location
--     boxes <- concat `fmap` mapM findBoxByShelf ss
--     let nothing = headEx $ Nothing : map Just ss -- trick to typecheck
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

usedDepth :: Shelf' shelf => shelf s -> WH (Text, Double) s
usedDepth s = do
  boxes <- findBoxByShelf s
  return $ List.maximumBy (comparing snd) (("<empty>",0)
                         :[(boxStyle box, dWidth (boxOffset' box))
                   | box <- boxes
                   ])




-- * Denormalizing 
--
shelfBoxes :: WH [(Shelf s, Box s)] s
shelfBoxes = do
    ss <- mapM findShelf =<< (toList <$> gets shelves)
    sbsS <- mapM (\s -> do bs <- findBoxByShelf s ; return (s, bs)) ss

    return [(s, box) | (s, bs) <- sbsS, box <- bs]


-- * Box corners operation 
  {-
extremeCorners :: [Box s] -> [(Double, Double)]
extremeCorners boxes = let
    cs =  [(l+ol, h+oh) | box <- boxes
                        , let Dimension l _ h = boxDim box
                        , let Dimension ol _ oh = boxOffset box
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
extractTag :: Text -> (Text, Maybe Text)
extractTag name = let (prefix, suffix) = break (=='#') name
             in case suffix of
                  (uncons -> Just ('#', tag)) -> (prefix, Just tag)
                  _ -> (prefix, Nothing)

extractTags :: Text -> (Text, [Text])
extractTags name = (style, maybe [] (splitOn "#") tagM) where
  (style, tagM) = extractTag name


withBoxOrientations :: [OrientationStrategy] -> WH a s -> WH a s
withBoxOrientations [] action =  action
withBoxOrientations strategies action =  do
  oldStrategy <- gets boxOrientations
  let newStrategy _ _ = strategies
  modify (\wh -> wh { boxOrientations = newStrategy })
  result <- action
  modify (\wh -> wh { boxOrientations = oldStrategy })
  return result

-- * Selectors 
-- ** Applying 
-- | The phantom type guarantie that we are selecting the correct item
applyNameSelector :: NameSelector a -> (a -> Text) -> a -> Bool
applyNameSelector (NameMatches []) _ _ = True
applyNameSelector (NameMatches pats) name o = any (flip applyPattern (name o)) pats
applyNameSelector (NameDoesNotMatch pats) name o = not $ any (flip applyPattern (name o)) pats


applyTagSelector :: TagSelector s -> Tags -> Bool
applyTagSelector (TagHasKey pat) tags = case pat of
  MatchFull key -> key `member` tags
  MatchAnything -> True
  MatchGlob glob -> not (null ks) where ks = filter (Glob.match glob . unpack) (keys tags)
applyTagSelector (TagHasNotKey pat) tags = not $ applyTagSelector (TagHasKey pat) tags
applyTagSelector (TagIsKey pat) tags = case pat of
  MatchFull key -> lookup key tags == Just mempty
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [__one] -> True
    _ -> False
applyTagSelector (TagIsKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndAll valuePats  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndAll valuePats values
    _ -> False
  _ -> False
applyTagSelector (TagHasKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndSome valuePats  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndSome valuePats values
    _ -> False
  _ -> False
applyTagSelector (TagHasValues valuePat) tags = let
  tagValues = mconcat (Map.elems tags)
  in matchesAllAndSome valuePat tagValues
applyTagSelector (TagHasNotValues valuePat) tags = not $ applyTagSelector (TagHasValues valuePat) tags
applyTagSelector (TagHasKeyAndNotValues key valuePat) tags = not (applyTagSelector (TagHasKeyAndValues key valuePat) tags)

applyTagSelectors :: [TagSelector s] -> (s -> Tags) -> s -> Bool
applyTagSelectors [] _ _ = True
applyTagSelectors selectors tags o = all (flip applyTagSelector (tags o)) selectors

-- | Check all pattern are matched and matches all values
matchesAllAndAll :: [MatchPattern] -> Set Text -> Bool
matchesAllAndAll pats vals = case unmatched pats vals of
  ([], []) -> True
  _ -> False 
-- | Check all pattern matches a value (but not all values have to be matches)
matchesAllAndSome :: [MatchPattern] -> Set Text -> Bool
matchesAllAndSome pats val = case unmatched pats val of
  ([], _) -> True
  _ -> False

unmatched :: [MatchPattern] -> Set Text -> ([MatchPattern], [Text])
unmatched pats0 valSet = go [] pats0 (Set.toList valSet) where
  go unused pats [] = (pats <> unused , [])
  go unused [] vals = (unused, vals)
  go unused (pat:pats) vals = case List.partition (applyPattern pat) vals of
    ([], _) -> go (pat:unused) pats vals
    -- \^ doesn't match anything, add to unused
    (_, vals') -> go unused pats vals'

-- ** Parsing 
-- | split on |
parseSelector :: Text -> Selector a
parseSelector s = case splitOn "#" s of
  [] -> Selector(NameMatches []) []
  (name:tags) -> Selector (parseNameSelector name) (mapMaybe parseTagSelector tags)

parseNameSelector selector = let
  (constr, pat) = case uncons selector of
       Just ('!', sel) -> (,) NameDoesNotMatch sel
       _ ->  (,) NameMatches selector
  in constr $ map parseMatchPattern (splitOn "|" pat)

parseTagSelector :: Text -> Maybe (TagSelector s)
parseTagSelector tag | null tag =  Nothing
parseTagSelector tag = Just $ case break ('='  ==) tag of
  (key, "")  -> case uncons key of
                Just ('-', nokey) -> TagHasNotKey $ parseMatchPattern nokey
                Just ('!', nokey) -> TagHasNotKey $ parseMatchPattern nokey
                _ -> TagIsKey $ parseMatchPattern key
  ("", stripPrefix "=-" -> Just values) -> TagHasNotValues  (mkValues values)
  ("", stripPrefix "=!" -> Just values) -> TagHasNotValues  (mkValues values)
  ("", stripPrefix "=" -> Just values) -> TagHasValues  (mkValues values)
  -- ("", stripPrefix "=!" -> Just values) -> TagHasNotValues  (mkValues values)
  (key, stripPrefix "=+" -> Just values) -> TagHasKeyAndValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=-" -> Just values) -> TagHasKeyAndNotValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=!" -> Just values) -> TagHasKeyAndNotValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=" -> Just values) -> TagIsKeyAndValues (parseMatchPattern key) (mkValues values)
  _ -> error "Bug. Result of break = should start with = or being captured earlier"
  where mkValues = map parseMatchPattern . fromList . splitOn ";"
  
parseMatchPattern :: Text -> MatchPattern
parseMatchPattern "" = MatchAnything
parseMatchPattern pat | isGlob pat= MatchGlob (Glob.compile $ unpack pat)
parseMatchPattern pat = MatchFull pat
  

parseBoxSelector :: Text -> BoxSelector s
parseBoxSelector selector = let
  (box'numbers, drop 1 -> location) = break (=='/') selector
  (box, drop 1 ->numbers) = break (=='^') box'numbers
  in BoxSelector (parseSelector box)
              (parseSelector location)
              (parseBoxNumberSelector numbers)

applyPattern :: MatchPattern -> Text -> Bool
applyPattern pat value = case pat of
  MatchAnything -> True
  MatchFull value0 -> value == value0
  MatchGlob glob -> Glob.match glob (unpack value)

-- * Warehouse Cache 
-- ** Property stats 
-- | Retrieve property stats (and compute if needed)
propertyStatsFor :: Text -> WH PropertyStats s
propertyStatsFor prop = do
  cacheRef <- whCache
  cache <- lift $ readSTRef cacheRef
  case lookup prop (propertyStats cache) of
    Just stat -> return stat
    Nothing -> computePropertyStats prop

-- | Computes or refresh the statistics for the given property
computePropertyStats :: Text -> WH PropertyStats s
computePropertyStats prop = do
  -- scann all object and make a map of the different values
  wh <- get
  boxList <- mapM findBox $ toList (boxes wh)
  let values = [ value
               | box <- boxList
               , value <- getTagValues box prop
               ]
      stats = mkPropertyStats values
  -- update cache
  cacheRef <- whCache
  lift $ modifySTRef cacheRef  (\cache -> cache {propertyStats = Map.insert prop stats (propertyStats cache) })
  return stats
  
mkPropertyStats :: [Text] -> PropertyStats
mkPropertyStats values = PropertyStats{..} where
  totalCount = length valueRank
  value'count = Map.fromListWith (+) $ map (,1) values
  valueRank = Map.fromList $ zip (map fst $ sortOn (Down . snd )
                                            (Map.toList value'count)
                                 )
                                 [1..]
  valueIndex = Map.fromList $ zip (sort $ keys valueRank) [1..]
  
clearCache :: WH () s
clearCache = do
  cachem <- gets whCacheM
  case cachem of
    Nothing -> return ()
    Just cache -> lift $ writeSTRef cache (emptyOperationCache)
  
-- | Create an empty cache if necessary
whCache :: WH (STRef s (OperationCache s)) s
whCache = do
  cachem <- gets whCacheM
  case cachem of
    Just cache -> return cache
    Nothing -> do
      cacheRef <- lift $ newSTRef emptyOperationCache 
      wh <- get
      put wh {whCacheM = Just cacheRef}
      return $ cacheRef
      
  
getOrCreateBoxTagMap :: Text -> WH (Map Text [Box s])  s
getOrCreateBoxTagMap prop = do
  cacheRef <- whCache
  cache <- lift $ readSTRef cacheRef 
  case lookup prop (boxTagMapMap cache) of
    Just boxMap -> {-traceShow ("Reuse cache for prop: " <> prop) $ -} return boxMap
    Nothing -> do
      boxMap <- __getBoxTagMap prop
      lift $ modifySTRef cacheRef (\cache' -> cache' {boxTagMapMap = Map.insert prop boxMap (boxTagMapMap cache')})
      return boxMap

__getBoxTagMap :: Text -> WH (Map Text [Box s]) s
__getBoxTagMap prop = do
  boxIds <- gets boxes
  boxes <- mapM findBox boxIds
  return $ Map.fromListWith (<>) [ (value, [box])
                                 | box <- toList boxes
                                 , value <- getTagValues box prop
                                 ]

-- * Find  the corners of the boxes which are enough
-- to describe the "stair" hull of all of the top right corner
-- of the given boxes.
-- For example
--
--       B
--    A
--            C
--            D
--
-- Will return B nd C
cornerHull :: [(Double, Double)] -> [(Double, Double)]
cornerHull corners = let
  -- allCorners = map boxCorner boxes
  -- boxCorner box = boxDim box <> boxOffset box
  -- we sort them in reverse order
  -- in exapmle C D B A
  sorted = sortOn Down corners
  go corner [] = [corner]
  go (x, y)  s@((x0,y0):_) = 
    if y > y0 && x < x0
    then ((x, y): s)
    else s
  in List.foldl (flip go) [] sorted


-- | Creates the "inner" corners of a "stairs"
--  (the Xs fro the '.')
-- X   .
--     X     .
--
--           X     .
--                 X
stairsFromCorners :: [(Double, Double)] -> [(Double, Double)]
stairsFromCorners corners =
  let (xs, ys) = unzip corners
  in zipWith (,) (0:xs) (ys ++ [0]) 


bestEffort :: [Box s] -> [(Double, Double)]
bestEffort boxes = let
  allCorners = map boxCorner boxes
  boxCorner box = let (Dimension x _ y ) = boxDim box <> boxOffset box
                  in (x,y)
  in stairsFromCorners $ cornerHull allCorners
  
