{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module WarehousePlanner.Type where
import Prelude
import Data.NonNull
import Control.Monad.State
import Diagrams.Prelude(Colour)
import Data.STRef
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Sequence (Seq)
import qualified System.FilePath.Glob as Glob
-- import Data.List(intercalate)
import Data.Text hiding(map)
import Data.Time (Day)
import Data.Semigroup(Arg(..))

-- * Types 
data Dimension = Dimension { dLength :: !Double
                           , dWidth  :: !Double
                           , dHeight :: !Double
                           } deriving (Show, Eq, Ord)

invert :: Dimension -> Dimension
invert (Dimension l w h) = Dimension (negate l) (negate w) (negate h)

data Direction = Vertical | Depth | Horizontal deriving (Show, Eq, Ord, Enum)
data Flow = LeftToRight | RightToLeft deriving (Show, Eq, Ord, Enum)

defaultFlow :: Flow
defaultFlow = LeftToRight

-- | How to filter box by number.
-- When selecting boxes, we first limit the number of boxes per content, then shelves then total
-- Filtering by content means only n boxes of the same style content will be selected.
-- This is use full to for example only keep one box of each variations and move them on top
data BoxNumberSelector = BoxNumberSelector
   { nsPerContent :: !(Maybe Limit)
   , nsPerShelf :: !(Maybe Limit)
   , nsTotal :: !(Maybe Limit)
   } deriving (Show)

-- | How to take slice of a selection
data Limit = Limit 
  { liStart :: !Int -- ^ first box to take, starts a 1
  , liEnd :: !Int -- ^ last box to take
  , liOrderTag :: !(Maybe Text) -- ^ which tag to use to sort boxes
  , liReverse :: !Bool -- ^ if true reverse the sorting order
  } deriving (Show ,Read)

-- | How something is oriented. It indicates  the direction of
-- the normal of the given face.
data Orientation = Orientation {  top :: !Direction, front :: !Direction } deriving (Show, Eq, Ord)

data HowMany = HowMany 
             { perShelf :: Int
             , perLength :: Int
             , perDepth :: Int
             , perHeight :: Int
             } deriving (Show) --  manual => , Eq, Ord)
             
instance Eq HowMany where a == b = perShelf a == perShelf b
instance Ord HowMany where compare a b = compare (perShelf a) (perShelf b)
         
mkHowMany :: Int -> Int -> Int -> HowMany
mkHowMany l w h = HowMany (l*w*h) l w h


-- | Way boxes are arranged.
-- For Diagonal, Size of the square to repeat
-- in diagonal configuration
-- example
--     - - |
--     - | -
--     | - -
-- 0 (or 1) means no diagonal
data TilingMode = Regular HowMany
                | Diagonal HowMany Int
                | TilingCombo Direction TilingMode TilingMode
                deriving (Eq, Show)
                
instance Ord TilingMode where
         compare a b = case tmTotal b `compare` tmTotal a of
                            EQ -> rank a `compare` (rank b :: Int)
                            c -> c
                       where rank Regular{} = 1 :: Int
                             rank (TilingCombo _ m n) = rank m + rank n
                             rank Diagonal{} = 3
                
showOrientationWithDiag :: Orientation -> TilingMode -> Text
showOrientationWithDiag or tilingMode = 
  case tilingMode of
       (Regular _) -> showOrientation' or
       (Diagonal  _ n ) -> "[" <> showOrientation' (rotateO or) <> Data.Text.replicate (n-1) (showOrientation' or) <> "]"
       (TilingCombo dir m1 m2) -> showOrientationWithDiag or m1 <> showD dir <> showOrientationWithDiag (rotateO or) m2
  where showD dir = case dir of
                      Horizontal -> "→"
                      Vertical -> "↑"
                      Depth -> "⊙"

-- | Possible orientations plus min max depth
data OrientationStrategy  = OrientationStrategy
  { osOrientations :: Orientation
  , osMinDepth :: Int
  , osMaxDepth :: Int
  , osMaxLenght:: Maybe Int
  , osMaxHeight :: Maybe Int
  , osUseDiagonal :: Bool -- ^ see `howManyWithDiagonal`
  } deriving (Show, Eq, Ord)

-- | Every box belongs to a shelf.
-- Non placed boxes belongs to the special default shelf
newtype BoxId s = BoxId_ (Arg Int (STRef s (Box s))) deriving (Eq, Ord)
{-# COMPLETE BoxId #-}
pattern BoxId s <- BoxId_ (Arg _ s)

instance Show (BoxId s) where
  show _ = "<<Boxref>>"

-- Tag equivalent to a page break in a document
-- Indicate if the given box should start a new row or a new shelf
data BoxBreak = StartNewSlot
              | StartNewSlice -- new row or column according to shelf strategy
              | StartNewShelf deriving (Eq, Show, Ord)
-- | Tags with optionals value
type Tags = Map Text (Set Text)
data Box s = Box { _boxId      :: BoxId s
               , boxShelf :: Maybe (ShelfId s)
               , boxStyle    :: !Text
               , boxContent  :: !Text
               , _boxDim     :: !Dimension
               , boxOffset   :: !Dimension
               , orientation :: !Orientation -- ^ orientation of the box
               , boxBoxOrientations :: [Orientation]  -- ^ allowed orientation
               , boxTags :: !Tags -- ^ tags with optional values
               , boxPriorities :: !(Int, Int, Int ) -- Global, within style, within content , default is 100
               , boxBreak :: !(Maybe BoxBreak)
               } deriving (Eq)
               
instance Show (Box s) where
  show box = unpack (boxStyle box <> "-" <> boxContent box <> "#") -- <> show (boxTags box)
newtype ShelfId s = ShelfId (STRef s (Shelf s))  deriving (Eq)
instance Ord (Box s) where
  compare a b = compare (_boxId a) (_boxId b)

-- | Shelf have a min and max dimension. This allows shelf to be overloaded
-- or boxes to stick out.
data Shelf s = Shelf { _shelfId  :: !(ShelfId s)
                   , _shelfBoxes :: Seq (BoxId s)
                   , shelfName :: !Text
                   , shelfTag :: !Tags
                   , minDim    :: !Dimension
                   , maxDim    :: !Dimension
                   , flow      :: !Flow
                   , shelfBoxOrientator :: !BoxOrientator
                   , shelfFillingStrategy :: !FillingStrategy
                   , bottomOffset :: !Double -- ^ "altitute" of where the usable part of the shelf starts
                   } deriving (Show, Eq)
-- | Gives orientation to a box
data BoxOrientator = DefaultOrientation
    | ForceOrientations ![Orientation]
    | BoxOrientations
    | FilterOrientations ![Orientation]
    | AddOrientations ![Orientation] ![Orientation]
    deriving (Show, Eq, Ord)

-- | Which way fill shelves

data FillingStrategy = RowFirst | ColumnFirst deriving (Show, Eq, Enum, Ord)
data ShelfGroup' s = ShelfGroup [ShelfGroup' s] Direction
                | ShelfProxy (s)
                deriving (Show)
type ShelfGroup s = ShelfGroup' (ShelfId s)

-- | State containing bays of boxes
-- boxOrientations : function returning a list
-- of possible box orientiations within a shelf for a given box.
-- as well as the number of boxes which can be used for the depth (min and max)
-- setting min to 1, allow forcing boxes stick out
data Warehouse s = Warehouse { boxes :: Seq (BoxId s)
                           , shelves :: Seq (ShelfId s)
                           , shelfGroup :: ShelfGroup s
                           , boxStyling :: Box s -> BoxStyling
                           , shelfStyling :: Shelf s -> ShelfStyling
                           , boxOrientations :: Box s -> Shelf s -> [OrientationStrategy]
                           , whCacheM :: Maybe (STRef s (OperationCache s))
                           , whDay :: Day -- Today usefull to compute date operation
                           -- \^ a cache. We use maybe to that an empty warehouse can be created "purely"
                           -- Should probably be part of of the WH 
                           , whUnique :: Int
                           
             } -- deriving Show
type WH a s = StateT  (Warehouse s) (ST s) a
data ExitMode = ExitOnTop | ExitLeft deriving (Show, Eq, Ord, Enum)
-- | Strategry to find available rectangle to fill in
data PartitionMode
  = PAboveOnly -- ^ max used height only
  | PRightOnly -- ^ max used weight only
  | PBestEffort -- ^ try to find all available rectangles
  | POr PartitionMode PartitionMode -- ^ combination
  deriving (Show, Eq, Ord)

data AddOldBoxes = NewBoxesOnly | AddOldBoxes deriving (Show, Eq, Ord, Enum)


-- | Misc data to speed up warehouse operations
-- depending on the value, it should be setup by the caller or the callee
data OperationCache s  = OperationCache
  { propertyStats :: Map Text PropertyStats
  , boxTagMapMap :: Map Text (Map Text [Box s])
  }
emptyOperationCache :: OperationCache s
emptyOperationCache = OperationCache mempty mempty

-- | Statistics relative to a property 
-- used valued and number of values
-- this is used to convert a property value to its rank or index
-- The rank can be used to chose a colour in a color scale.
data PropertyStats = PropertyStats
   { totalCount :: Int
   , valueRank :: Map Text Int -- sorted by occurence
   , valueIndex :: Map Text Int -- sorted alpha
   } deriving Show

type Corner = (Double, Double)

-- | how to render boxes
data BoxStyling = BoxStyling
  { foreground :: Colour Double --  ^ Text colour
  , background :: Colour Double -- ^ Background colour
  , circleBgs :: [Colour Double] -- ^ 2nd Background colour
  , border :: Maybe (Colour Double)  -- ^ border colour if different from foreground
  , title :: [ Text ] -- ^ text to display 
  , barTitle :: Maybe Text -- ^ text to display in the bar
  , displayBarGauge :: Bool -- ^ to display or the bar gauge
  , offsetBarGaugeX :: Maybe Double
  , offsetBarGaugeY :: Maybe Double
  } deriving (Show, Eq)
  
data ShelfStyling = ShelfStyling
  { foreground :: Colour Double --  ^ Text colour
  , background :: Colour Double -- ^ Background colour
  , maxBackground :: Colour Double -- ^ Background colour
  , barForeground :: Colour Double -- ^ Text colour
  , barBackground :: Colour Double -- ^ Background colour
  , border :: (Colour Double)  -- ^ border colour 
  , title :: [ Text ] -- ^ text to display 
  , barTitle :: Maybe Text -- ^ text to display in the bar
  , displayBarGauge :: Bool -- ^ to display or the bar gauge
  } deriving (Show, Eq)
-- * Classes 
class ShelfIdable a where
    shelfId :: a s -> ShelfId s
class (ShelfIdable b) => Shelf' b where
  findShelf :: b s  -> WH (Shelf s) s
class BoxIdable a where
    boxId :: a s -> BoxId s
class (BoxIdable b) => Box' b where
  findBox :: b s  -> WH (Box s) s
class Referable a where
  type Ref a :: *
  getRef :: a -> Ref a

class HasTags a where
  getTags :: a -> Tags

-- * Instances 
instance Semigroup Dimension where
    (Dimension l w h) <> (Dimension l' w' h') =
            Dimension (l+l') (w+w') (h+h')
instance Monoid Dimension where
    mempty = Dimension 0 0 0

instance BoxIdable BoxId where
    boxId b = b

instance BoxIdable Box where
    boxId = _boxId

instance Box' Box where

  findBox b = findBox (boxId b) -- "reload" the box in caes it has been modified
instance Box' BoxId where
  findBox (BoxId ref) = lift $ readSTRef ref


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
instance Show (ShelfId s) where
  show _ = "<<ShelfId>>"

instance ShelfIdable ShelfId where
    shelfId b = b

instance ShelfIdable Shelf where
    shelfId = _shelfId
instance Shelf' Shelf where
  findShelf s = findShelf (shelfId s) -- reload the shef
instance Shelf' ShelfId where
  findShelf (ShelfId ref) = lift $ readSTRef ref

instance HasTags (Box s) where getTags = boxTags
instance HasTags (Shelf s) where getTags = shelfTag
instance HasTags (Tags) where getTags = id

instance Semigroup (ShelfGroup s) where
    sg@(ShelfGroup g d) <> sg'@(ShelfGroup g' d')
        | d == d' = ShelfGroup (g <> g') d
        | otherwise = ShelfGroup [sg, sg'] Vertical

    (ShelfGroup g d) <> s = ShelfGroup (g<>[s]) d
    s <> (ShelfGroup g d) = ShelfGroup (s:g) d
    sg <> sg' = ShelfGroup [sg, sg'] Vertical

instance Monoid (ShelfGroup s) where
    mempty = ShelfGroup [] Vertical

-- * Utilities 
-- ** Dimensions 
volume :: Dimension -> Double
volume (Dimension l w h) = l*w*h
floorSpace :: Dimension -> Double
floorSpace (Dimension l w _) = l*w
-- ** Tiling
tmTotal :: TilingMode -> Int
tmTotal (Regular hmany) = perShelf hmany
tmTotal (Diagonal hmany _) = perShelf hmany
tmTotal (TilingCombo _ t1 t2) = tmTotal t1 + tmTotal t2

tmLength :: TilingMode -> Int
tmLength (Regular hmany) = perLength hmany
tmLength (Diagonal hmany _) = perLength hmany
tmLength (TilingCombo Horizontal t1 t2) = tmLength t1 + tmLength t2
tmLength (TilingCombo _ t1 t2) = tmLength t1 `max` tmLength t2

tmDepth :: TilingMode -> Int
tmDepth (Regular hmany) = perDepth hmany
tmDepth (Diagonal hmany _) = perDepth hmany
tmDepth (TilingCombo Depth t1 t2) = tmDepth t1 + tmDepth t2
tmDepth (TilingCombo _ t1 t2) = tmDepth t1 `max` tmDepth t2

tmHeight :: TilingMode -> Int
tmHeight (Regular hmany) = perHeight hmany
tmHeight (Diagonal hmany _) = perHeight hmany
tmHeight (TilingCombo Vertical t1 t2) = tmHeight t1 + tmHeight t2
tmHeight (TilingCombo _ t1 t2) = tmHeight t1 `max` tmHeight t2

tmHowManys :: TilingMode -> NonNull [HowMany]
tmHowManys (Regular hmany) = ncons hmany []
tmHowManys (Diagonal hmany _) = ncons hmany []
tmHowManys (TilingCombo _ m1 m2) = tmHowManys m1 <> tmHowManys m2

-- ** Boxes 

-- ** Orientations 
up, tiltedForward, tiltedRight, tiltedFR, rotatedSide, rotatedUp :: Orientation
up = Orientation Vertical Depth
tiltedForward = Orientation Depth Vertical
tiltedRight = Orientation Horizontal Depth
tiltedFR = Orientation Depth Horizontal
rotatedSide = Orientation Horizontal Vertical
rotatedUp = Orientation Vertical Horizontal

showOrientation :: Orientation -> Text
showOrientation o = showOrientation' o <> " "
showOrientation' :: Orientation -> Text
showOrientation' o | o == up             =  "^"
                  | o == tiltedForward  =  "="
                  | o == tiltedRight    =  ">"
                  | o == tiltedFR       =  "|"
                  | o == rotatedUp      =  "'"
                  | o == rotatedSide      =  "@"
                  | otherwise           =  "tA"

readOrientation :: Char -> Orientation
readOrientation c = case c of
    '^' -> up
    '=' -> tiltedForward
    '>' -> tiltedRight
    '|' -> tiltedFR
    '\'' -> rotatedUp
    '@' -> rotatedSide
    _ -> error ("can't parse orientation '" <> show c )

allOrientations :: [Orientation]
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

-- | Rotate an Orientation by 90 facing the depth
rotateO :: Orientation -> Orientation
rotateO (Orientation{..}) = Orientation (rot top) (rot front)
  where 
    rot Vertical = Horizontal
    rot Horizontal = Vertical
    rot Depth = Depth


-- ** Boxes 
boxKey :: Box s -> Text
boxKey b = (boxStyle b) <> (boxContent b)
boxSku :: Box s -> Text
boxSku b = boxStyle b <> "-" <> boxContent b
getTagList :: HasTags tagged => tagged -> [Text]
getTagList = map flattenTag'Values . Map.toList . getTags
-- | convenience function transforming  the result to list
-- Can be easier to pattern match
getTagValues :: HasTags tagged => tagged -> Text -> [ Text ]
getTagValues box tag = maybe [] (Set.toList) (Map.lookup tag (getTags box))
getTagValuem :: HasTags tagged => tagged -> Text -> Maybe Text
getTagValuem box tag = fmap flattenTagValues (Map.lookup tag (getTags box))
tagIsPresent :: HasTags tagged => tagged -> Text -> Bool
tagIsPresent box tag = Map.member tag (getTags box)

-- | Return global dimension according
-- to box orientation
boxDim :: Box s -> Dimension
boxDim box = rotate (orientation box) (_boxDim box)


boxVolume :: Box s -> Double
boxVolume = volume . boxDim

-- | Returns box offset <> box itself
boxOffset' :: Box s -> Dimension
boxOffset' b = boxOffset b <> boxDim b

-- ** Tags 
flattenTagValues :: Set Text -> Text
flattenTagValues = intercalate ";" . Set.toList
flattenTag'Values :: (Text, Set Text) -> Text
flattenTag'Values (tag, values) = case flattenTagValues values of
                      "" ->tag
                      flat -> tag <> "=" <> flat

flattenTags :: Tags -> [Text]
flattenTags = map flattenTag'Values . Map.toList 
-- ** Shelves 
shelfVolume :: Shelf s -> Double
shelfVolume = volume . minDim

shelfNameTag :: Shelf s -> Text
shelfNameTag s = intercalate "#" ( shelfName s
                                 : (flattenTags (shelfTag s))
                                 )
-- ** Selectors 
-- | Some cases are overlapping but it is on purpose
-- so that we can speed up 
data TagSelector  a
          = TagHasKey !MatchPattern --  ^ check present of key not is value
          | TagHasNotKey !MatchPattern --  ^ check present of key not is value
          | TagIsKey !MatchPattern --  ^ present with no value
          -- -| TagHasNotKey !MatchPattern --  ^ not present  (in all values)
          -- -| TagIsKeyAndValue Text Text --  ^ check key and value
          | TagIsKeyAndValues !MatchPattern ![MatchPattern] --  ^ check key and all values are exact
          | TagHasKeyAndValues !MatchPattern ![MatchPattern] --  ^ check key and at least all values are present
          | TagHasValues ![MatchPattern]
          | TagHasNotValues ![MatchPattern]
          | TagHasKeyAndNotValues !MatchPattern ![MatchPattern] --  ^ check key and at least all values are present
          deriving Show
          -- deriving (Eq,Show,Ord)

data MatchPattern
   = MatchFull !Text
   | MatchAnything
   | MatchGlob !Glob.Pattern
   deriving (Eq, Show)
   -- -| MatchRegext Text

data NameSelector a = NameMatches [MatchPattern] -- matches one of
                    | NameDoesNotMatch [MatchPattern] -- matche none of
  deriving (Eq, Show)

data Selector a  = Selector 
  { nameSelector :: !(NameSelector a)   -- disjunctions OR betweens names
  , tagSelectors :: ![TagSelector a] -- conjunctions AND
  }
  deriving (Show)


pattern SelectAnything :: Selector a
pattern SelectAnything <- Selector (matchAnyNames -> True) [] where
        SelectAnything = Selector (NameMatches []) []


matchAnyNames :: NameSelector a -> Bool
matchAnyNames (NameMatches []) = True
matchAnyNames (NameMatches [MatchAnything]) = True
matchAnyNames (NameDoesNotMatch []) = True
matchAnyNames _ = False


data BoxSelector s = BoxSelector
  { boxSelectors :: !(Selector (Box s))
  , shelfSelectors :: !(Selector (Shelf s))
  , numberSelector :: !(BoxNumberSelector)
  } deriving (Show)

data ShelfSelector s = ShelfSelector
  { sBoxSelectors :: !(Selector (Box s))
  , sShelfSelectors :: !(Selector (Shelf s))
  } deriving (Show)

selectAllBoxes :: BoxSelector s
selectAllBoxes = BoxSelector SelectAnything
                             SelectAnything
                             (BoxNumberSelector Nothing Nothing Nothing)

-- ** Warehouse 

-- ** Similar 
