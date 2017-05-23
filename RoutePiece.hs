-- | Miscelaneous Types used as route piec.
module RoutePiece where
import ClassyPrelude
import Web.PathPieces

-- * Warehouse
-- ** PackingList
-- | Different way of viewing a packing list.
data PLViewMode = Details 
                | Textcart 
                | Stickers 
                | StickerCsv 
                | Chalk 
                | Planner 
                | PlannerColourless
                | EditDetails 
                | Edit
                | Deliver
  deriving (Eq, Read, Show, Enum, Bounded)

instance PathPiece PLViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece
