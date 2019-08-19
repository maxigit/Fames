{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoOverloadedStrings #-}
module WarehousePlanner.Type where
import Prelude
import Data.Vector(Vector)
import Data.Monoid
import Control.Monad.State
import Diagrams.Prelude(Colour)
import Data.STRef
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.List(intercalate)

-- * Types
data Dimension = Dimension { dLength :: !Double
                           , dWidth  :: !Double
                           , dHeight :: !Double
                           } deriving (Show, Eq, Ord)


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
-- | Every box belongs to a shelf.
-- Non placed boxes belongs to the special default shelf
data BoxId s = BoxId (STRef s (Box s)) deriving (Eq)
instance Show (BoxId s) where
  show _ = "<<Boxref>>"

-- Tag equivalent to a page break in a document
-- Indicate if the given box should start a new row or a new shelf
data BoxBreak = StartNewSlot
              | StartNewSlice -- new row or column according to shelf strategy
              | StartNewShelf deriving (Eq, Show, Read)
-- | Tags with optionals value
type Tags = Map String (Set String)
data Box s = Box { _boxId      :: BoxId s
               , boxShelf :: Maybe (ShelfId s)
               , boxStyle    :: !String
               , boxContent  :: !String
               , _boxDim     :: !Dimension
               , boxOffset   :: !Dimension
               , orientation :: !Orientation -- ^ orientation of the box
               , boxBoxOrientations :: [Orientation]  -- ^ allowed orientation
               , boxTags :: Tags -- ^ tags with optional values
               , boxPriorities :: (Int, Int, Int ) -- Global, within style, within content , default is 100
               , boxBreak :: Maybe BoxBreak
               } deriving (Show, Eq)
data ShelfId s = ShelfId (STRef s (Shelf s))  deriving (Eq)

-- | Shelf have a min and max dimension. This allows shelf to be overloaded
-- or boxes to stick out.
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
                           , shelfColors :: Shelf s -> (Maybe (Colour Double), Maybe (Colour Double))
                           , boxOrientations :: Box s -> Shelf s -> [(Orientation, Int, Int)]
             } -- deriving Show
type WH a s = StateT  (Warehouse s) (ST s) a
data ExitMode = ExitOnTop | ExitLeft deriving (Show, Eq, Ord, Enum)

type Corner = (Double, Double)

-- | how to render boxes
data BoxStyling = BoxStyling
  { foreground :: Colour Double -- ^ Text colour
  , background :: Colour Double -- ^ Background colour
  , border :: Maybe (Colour Double)  -- ^ border colour if different from foreground
  } deriving (Show, Eq, Read)
  
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

instance Semigroup (ShelfGroup s) where
    sg@(ShelfGroup g d) <> sg'@(ShelfGroup g' d')
        | d == d' = ShelfGroup (g <> g') d
        | otherwise = ShelfGroup [sg, sg'] Vertical

    sg@(ShelfGroup g d) <> s = ShelfGroup (g++[s]) d
    s <> sg@(ShelfGroup g d) = ShelfGroup (s:g) d
    sg <> sg' = ShelfGroup [sg, sg'] Vertical

instance Monoid (ShelfGroup s) where
    mempty = ShelfGroup [] Vertical

-- * Utilities
-- ** Dimensions
volume :: Dimension -> Double
volume (Dimension l w h) = l*w*h
floorSpace :: Dimension -> Double
floorSpace (Dimension l w _) = l*w
-- ** Boxes

-- ** Orientations
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

-- ** Boxes
boxKey :: Box s -> [Char]
boxKey b = (boxStyle b) ++ (boxContent b)
boxSku b = boxStyle b ++ "-" ++ boxContent b
boxTagList :: Box s -> [String]
boxTagList = map flattenTag'Values . Map.toList . boxTags
-- | convenience function transforming  the result to list
-- Can be easier to pattern match
boxTagValues :: Box s -> String -> [ String ]
boxTagValues box tag = maybe [] (Set.toList) (Map.lookup tag (boxTags box))
boxTagValuem :: Box s -> String -> Maybe String
boxTagValuem box tag = fmap flattenTagValues (Map.lookup tag (boxTags box))
boxTagIsPresent :: Box s -> String -> Bool
boxTagIsPresent box tag = Map.member tag (boxTags box)

-- | Return global dimension according
-- to box orientation
boxDim :: Box s -> Dimension
boxDim box = rotate (orientation box) (_boxDim box)


boxVolume :: Box s -> Double
boxVolume = volume . boxDim

-- | Returns box offset + box itself
boxOffset' :: Box s -> Dimension
boxOffset' b = boxOffset b <> boxDim b

-- ** Tags
flattenTagValues :: Set String -> String
flattenTagValues = intercalate ";" . Set.toList
flattenTag'Values :: (String, Set String) -> String
flattenTag'Values (tag, values) = case flattenTagValues values of
                      "" ->tag
                      flat -> tag ++ "=" ++ flat

-- ** Shelves
shelfVolume :: Shelf s -> Double
shelfVolume = volume . minDim

shelfNameTag :: Shelf s -> String
shelfNameTag s = shelfName s ++ maybe "" ("#"++) (shelfTag s)

-- ** Warehouse

-- ** Similar
