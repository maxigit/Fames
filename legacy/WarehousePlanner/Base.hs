{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module WarehousePlanner.Base where
import Prelude
import Data.Vector(Vector)
import Control.Monad.State
import Data.Monoid
import qualified Data.Map.Strict as Map'
import Data.List(sort, sortBy, groupBy, nub, (\\), union, maximumBy, delete)
import Data.Maybe(catMaybes, fromMaybe)
import Control.Applicative
import Data.Ord (comparing, Down(..))
import Data.List.Split(splitOn)
import Data.Function(on)
import Diagrams.Prelude(Colour, white)
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

import qualified System.FilePath.Glob as Glob
import Text.Read (readMaybe)


import Debug.Trace

data Dimension = Dimension { dLength :: !Double
                           , dWidth  :: !Double
                           , dHeight :: !Double
                           } deriving (Show, Eq, Ord)

volume :: Dimension -> Double
volume (Dimension l w h) = l*w*h
floorSpace :: Dimension -> Double
floorSpace (Dimension l w _) = l*w


instance Monoid Dimension where
    mempty = Dimension 0 0 0
    mappend (Dimension l w h) (Dimension l' w' h') =
            Dimension (l+l') (w+w') (h+h')

data Direction = Vertical | Depth | Horizontal deriving (Show, Eq, Ord, Enum)

data Flow = LeftToRight | RightToLeft deriving (Show, Eq, Ord, Enum)
defaultFlow = LeftToRight

-- | How to filter box by number.
-- When selecting boxes, we first limit the number of boxes per content, then shelves then total
-- Filtering by content means only n boxes of the same style content will be selected.
-- This is use full to for example only keep one box of each variations and move them on top
data BoxNumberSelector = BoxNumberSelector
   { nsPerContent :: Maybe Int
   , nsPerShelf :: Maybe Int
   , nsTotal :: Maybe Int
   } deriving (Show, Read)
           

-- | How something is oriented. It indicates  the direction of
-- the normal of the given face.
data Orientation = Orientation {  top :: !Direction, front :: !Direction } deriving (Show, Eq, Ord)
up = Orientation Vertical Depth
tiltedForward = Orientation Depth Vertical
tiltedRight = Orientation Horizontal Depth
tiltedFR = Orientation Depth Horizontal
rotatedSide = Orientation Horizontal Vertical
rotatedUp = Orientation Vertical Horizontal

showOrientation :: Orientation -> String
showOrientation o | o == up             =  "^ "
                  | o == tiltedForward  =  "= "
                  | o == tiltedRight    =  "> "
                  | o == tiltedFR       =  "| "
                  | o == rotatedUp      =  "' "
                  | o == rotatedSide      =  "@ "
                  | otherwise           =  "tA "

readOrientation c = case c of
    '^' -> up
    '=' -> tiltedForward
    '>' -> tiltedRight
    '|' -> tiltedFR
    '\'' -> rotatedUp
    '@' -> rotatedSide
    _ -> error ("can't parse orientation '" ++ show c )

allOrientations = [ up
                  , rotatedUp
                  , tiltedForward
                  , tiltedFR
                  , tiltedRight
                  , rotatedSide
                  , rotatedUp
                  ]

rotate :: Orientation -> Dimension -> Dimension
rotate o (Dimension l w h)
    | o == up             =  Dimension l w h
    | o == tiltedForward  =  Dimension l h w
    | o == tiltedRight    =  Dimension h w l
    | o == tiltedFR       =  Dimension w h l
    | o == rotatedUp    =  Dimension w l h
    | o == rotatedSide    =  Dimension h l w
    | True  = error $ "Unexpected rotation" <> show o

-- | Every box belongs to a shelf.
-- Non placed boxes belongs to the special default shelf
data BoxId s = BoxId (STRef s (Box s)) deriving (Eq)
instance Show (BoxId s) where
  show _ = "<<Boxref>>"

data Box s = Box { _boxId      :: BoxId s
               , boxShelf :: Maybe (ShelfId s)
               , boxStyle    :: !String
               , boxContent  :: !String
               , _boxDim     :: !Dimension
               , boxOffset   :: !Dimension
               , orientation :: !Orientation -- ^ orientation of the box
               , boxBoxOrientations :: [Orientation]  -- ^ allowed orientation
               , boxTags :: [String] --
               , boxPriorities :: (Int, Int, Int ) -- Global, within style, within content , default is 100
               } deriving (Show, Eq)

boxKey :: Box s -> [Char]
boxKey b = (boxStyle b) ++ (boxContent b)
boxSku b = boxStyle b ++ "-" ++ boxContent b

-- | Return global dimension according
-- to box orientation
boxDim :: Box s -> Dimension
boxDim box = rotate (orientation box) (_boxDim box)


boxVolume :: Box s -> Double
boxVolume = volume . boxDim

-- | Returns box offset + box itself
boxOffset' :: Box s -> Dimension
boxOffset' b = boxOffset b <> boxDim b
class BoxIdable a where
    boxId :: a s -> BoxId s

instance BoxIdable BoxId where
    boxId b = b

instance BoxIdable Box where
    boxId = _boxId

class (BoxIdable b) => Box' b where
  findBox :: b s  -> WH (Box s) s
instance Box' Box where
  findBox b = findBox (boxId b) -- "reload" the box in caes it has been modified
instance Box' BoxId where
  findBox (BoxId ref) = lift $ readSTRef ref


class Referable a where
  type Ref a :: *
  getRef :: a -> Ref a

instance Referable (Box s) where
  type Ref (Box s) = STRef s (Box s)
  getRef box = getRef (boxId box)

instance Referable (BoxId s) where
  type Ref (BoxId s) = STRef s (Box s)
  getRef (BoxId ref) = ref

instance Referable (Shelf s) where
  type Ref (Shelf s) = STRef s (Shelf s)
  getRef shelf = getRef (shelfId shelf)

instance Referable (ShelfId s) where
  type Ref (ShelfId s) = STRef s (Shelf s)
  getRef (ShelfId ref) = ref



-- | Shelf have a min and max dimension. This allows shelf to be overloaded
-- or boxes to stick out.
data ShelfId s = ShelfId (STRef s (Shelf s))  deriving (Eq)
instance Show (ShelfId s) where
  show _ = "<<ShelfId>>"
data Shelf s = Shelf { _shelfId  :: ShelfId s
                   , _shelfBoxes :: [BoxId s]
                   , shelfName :: !String
                   , shelfTag :: Maybe String
                   , minDim    :: !Dimension
                   , maxDim    :: !Dimension
                   , flow      :: !Flow
                   , shelfBoxOrientator :: !BoxOrientator
                   , shelfFillingStrategy :: !FillingStrategy
                   } deriving (Show, Eq)

shelfVolume :: Shelf s -> Double
shelfVolume = volume . minDim

shelfNameTag :: Shelf s -> String
shelfNameTag s = shelfName s ++ maybe "" ("#"++) (shelfTag s)
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


-- | Gives orientation to a box
data BoxOrientator = DefaultOrientation
    | ForceOrientations ![Orientation]
    | BoxOrientations
    | FilterOrientations ![Orientation]
    | AddOrientations ![Orientation] ![Orientation]
    deriving (Show, Eq, Ord)

-- | Which way fill shelves

data FillingStrategy = RowFirst | ColumnFirst deriving (Show, Eq, Enum, Ord)

class ShelfIdable a where
    shelfId :: a s -> ShelfId s

instance ShelfIdable ShelfId where
    shelfId b = b

instance ShelfIdable Shelf where
    shelfId = _shelfId


class (ShelfIdable b) => Shelf' b where
  findShelf :: b s  -> WH (Shelf s) s
instance Shelf' Shelf where
  findShelf s = findShelf (shelfId s) -- reload the shef
instance Shelf' ShelfId where
  findShelf (ShelfId ref) = lift $ readSTRef ref


-- | Nested groups of shelves, used for display
data ShelfGroup' s = ShelfGroup [ShelfGroup' s] Direction
                | ShelfProxy (s)
                deriving (Show)
type ShelfGroup s = ShelfGroup' (ShelfId s)

groupToShelfIds :: ShelfGroup s -> [ShelfId s]
groupToShelfIds (ShelfProxy sid) = [sid]
groupToShelfIds (ShelfGroup  groups _ ) = concatMap groupToShelfIds groups

instance Monoid (ShelfGroup s) where
    mempty = ShelfGroup [] Vertical
    mappend sg@(ShelfGroup g d) sg'@(ShelfGroup g' d')
        | d == d' = ShelfGroup (g `mappend` g') d
        | otherwise = ShelfGroup [sg, sg'] Vertical

    mappend sg@(ShelfGroup g d) s = ShelfGroup (g++[s]) d
    mappend s sg@(ShelfGroup g d) = ShelfGroup (s:g) d
    mappend sg sg' = ShelfGroup [sg, sg'] Vertical


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

-- | State containing bays of boxes
-- boxOrientations : function returning a list
-- of possible box orientiations within a shelf for a given box.
-- as well as the number of boxes which can be used for the depth (min and max)
-- setting min to 1, allow forcing boxes stick out
data Warehouse s = Warehouse { boxes :: Seq (BoxId s)
                           , shelves :: Seq (ShelfId s)
                           , shelfGroup :: ShelfGroup s
                           , colors :: Box s -> Colour Double
                           , shelfColors :: Shelf s -> (Maybe (Colour Double), Maybe (Colour Double))
                           , boxOrientations :: Box s -> Shelf s -> [(Orientation, Int, Int)]
             } -- deriving Show

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
filterShelfByTag tagM shelf = filterByTag  tagM (maybeToList $ shelfTag shelf)

filterBoxByTag :: Maybe String -> Box s -> Bool
filterBoxByTag tagM box =  filterByTag tagM (boxTags box)

-- | all tags from the left must belong to the right
-- ex if box as tag A B C, A#B should match but not A#E
filterByTag Nothing names = True
filterByTag _ [] = False
filterByTag (Just tag) names = let
  tags = map ok $ splitOn "#" tag
  ok ('-':t) = Left t
  ok t = Right t
  on = rights tags
  off = lefts tags
  in  length (on \\ names) == 0 -- all tags are found
     && length (off \\ names) == length off -- not negative tags are found


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
                  Just _ -> do
                    filter (filterBoxByTag boxtag) allBoxesBeforeTag

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
  


emptyWarehouse = Warehouse mempty mempty mempty (const white) (const (Nothing, Nothing)) defaultBoxOrientations
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


type WH a s = StateT  (Warehouse s) (ST s) a
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
    let tags' = case content of
          "" -> tags
          _ -> ('\'':content):tags
    -- create tag of dimension
        dtags = [dshow 'l' dLength, dshow 'w' dWidth, dshow 'h' dHeight]
        dshow c f = '\'' : c : show (floor $ 100 * f dim)

    ref <- lift $ newSTRef (error "should never been called. undefined. Base.hs:338")
    let box = Box (BoxId ref) (Just $ shelfId shelf) style content dim mempty or ors (tags' <> dtags) defaultPriorities
    shelf' <- findShelf shelf
    linkBox (BoxId ref) shelf'
    lift $ writeSTRef ref box
    put warehouse { boxes = boxes warehouse |> BoxId ref
                  }
    return box

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


-- Move "physicaly" a box into a shelf if there is enough space
-- Boxes are supposed to be the same size.
-- Sometime we want to arrange boxes in column across boxes
-- In that case, instead of filling the bottom shelf as much as we can
-- we need to fill only one column, go to the next shelves, then
-- start again with the bottom boxes with the remaining shelves.
-- This the purpose of the ExitOnTop mode.
data ExitMode = ExitOnTop | ExitLeft deriving (Show, Eq, Ord, Enum)
-- | List of "similar" object. In case of boxes of the same dimension.
-- This type is just for information.
-- There is nothign to enforce the object similarity.
-- return the shelf itself if not empty
newtype Similar b = Similar [b]
fillShelf :: (Box' b, Shelf' shelf) => ExitMode -> shelf s -> Similar (b s) -> WH ([Box s], Maybe (Shelf s))  s
fillShelf _ _ (Similar []) = return ([], Nothing)
fillShelf exitMode  s (Similar bs) = do
    shelf <- findShelf s
    boxes <- T.mapM findBox bs
    -- first we need to find how much space is left
    boxesIn <- findBoxByShelf s
    lused <- lengthUsed shelf
    hused <- heightUsed shelf

    let groups = Map'.toList $ Map'.fromListWith (flip(<>)) [(_boxDim b, [b]) | b <- boxes]
        -- we assume here that all boxes with the same dimension share the same orientation policy.

        (dim, firstGroup) = head groups

    boxo <- gets boxOrientations
    let orientations = boxo (head firstGroup) shelf
    let (bestO, nl_, nw, nh, (lused', hused')) =
                        bestArrangement orientations
                                          [ (Dimension (max 0 (shelfL -l)) shelfW (max 0 (shelfH-h)), (l,h))
                                          | (Dimension shelfL shelfW shelfH) <- [ minDim shelf, maxDim shelf ]
                                        -- try min and max. Choose min if  possible
                                          -- , (l,h) <- extremeCorners  boxesIn -- alternative algo
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
                                    , iw <- [0..nw-1]
                                    , il <- [0..nl-1]
                                    ]
                  ]
     --  turn all boxes
        orderedBox = sortBy (compare `on` boxRank) firstGroup
    zipWithM (shiftBox bestO) orderedBox offsets
    let otherBoxes = concat [bs | (_, bs) <- tail groups ]

        left = (drop (nl*nh*nw)) orderedBox  ++ otherBoxes

    case (nl*nw*nh, exitMode) of
         (0, _ ) -> return (left, Nothing)
         (_ , ExitOnTop) -> return (left, Just shelf)
         _ ->  fillShelf exitMode  s (Similar (map boxId left))

    where shiftBox ori box offset = do
            updateBox (\b -> b { orientation = ori
                               , boxOffset = offset}) box
            assignShelf (Just s) box






-- Try to Move a block of boxes  into a block of shelves.
-- Boxes are move in sequence and and try to fill shelve
-- in sequence. If they are not enough space the left boxes
-- are returned.
moveBoxes :: (Box' b , Shelf' shelf) => ExitMode -> [b s] -> [shelf s] -> WH [Box s] s

moveBoxes exitMode bs ss = do
  boxes <- mapM findBox bs
  let layers = groupBy ((==)  `on` boxGlobalPriority) $ sortBy (comparing boxGlobalPriority) boxes
  lefts <- forM layers $ \layer -> do
    let groups = map Similar (groupBy ((==) `on` _boxDim) $ sortBy (comparing _boxDim) layer )
    -- traceShowM ("GRoups", length groups, map (\(Similar g@(g1:_)) -> (show $ length g, show $ _boxDim g1, show . roundDim $ boxDim g1 )) groups)
    lefts <- mapM (\g -> moveSimilarBoxes exitMode g ss) groups
    return $ concat lefts
  return $ concat lefts

roundDim :: Dimension -> [Int]
roundDim (Dimension l w h) = map (round . (*100)) [l,w,h]

 

moveSimilarBoxes :: (Box' b , Shelf' shelf) => ExitMode -> Similar (b s) -> [shelf s] -> WH [Box s] s
moveSimilarBoxes exitMode bs ss = moveSimilarBoxes' exitMode bs ss []


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

moveSimilarBoxes' :: (Box' b , Shelf' shelf) => ExitMode -> Similar (b s) -> [shelf s] -> [shelf s] -> WH [Box s] s
moveSimilarBoxes' _ (Similar bs) [] [] = mapM findBox bs
moveSimilarBoxes' exitMode bs [] ss = moveSimilarBoxes' exitMode bs (reverse ss) [] -- can loop but normal ss is null
moveSimilarBoxes' exitMode (Similar []) _  _ = return []
moveSimilarBoxes' exitMode (Similar bs) (s:ss') ss'' = do
    left <- fillShelf exitMode s (Similar bs)
    case left of
        ([], Nothing) -> return []
        (bs', Nothing)  -> moveSimilarBoxes' exitMode (Similar bs') (ss') ss'' -- discard current box
        (bs', Just _)  -> moveSimilarBoxes' exitMode (Similar bs') ss' (s:ss'')


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
  to_add = rights parsed
  to_remove = lefts parsed
  new = (btags <> to_add) \\ to_remove
  in box {boxTags = new, boxPriorities = extractPriorities new}

updateBoxTags tags = let
  -- remove '''
  -- tags = map (map replaceSlash) tags0
  in updateBox (updateBoxTags' $ filter (not . null) tags)

replaceSlash '/' = '\''
replaceSlash c  = c
   
defaultPriority :: Int
defaultPriority = 100
defaultPriorities = (defaultPriority, defaultPriority, defaultPriority)

-- | Convert a set of tags to prioties
-- bare number = content priority, 
-- @number style priority
-- @@number global priority
extractPriorities :: [String] -> (Int, Int, Int)
extractPriorities tags = let
  prioritiess = map extractPriority tags
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
extremeCorners :: [Box s] -> [(Double, Double)]
extremeCorners boxes = let
    cs =  [(l+ol, h+oh) | b <- boxes
                        , let Dimension l _ h = boxDim b
                        , let Dimension ol _ oh = boxOffset b
           ]
    -- sort corner by
    cs'  = reverse cs
    in foldr (addCorner) [(0,0)] cs'


type Corner = (Double, Double)
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
