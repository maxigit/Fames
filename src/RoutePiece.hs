-- | Miscelaneous Types used as route piec.
module RoutePiece where
import ClassyPrelude
import Web.PathPieces

-- * General

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
                | StocktakePL
  deriving (Eq, Read, Show, Enum, Bounded)

instance PathPiece PLViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece


-- ** ItemIndex
data ItemViewMode = ItemGLView
                   | ItemPriceView
                   | ItemPurchaseView
                   | ItemWebStatusView
                   | ItemAllView -- Not supposed to be used as a view
  deriving (Eq, Read, Show, Enum, Bounded)
instance PathPiece ItemViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece


-- * Planner
data PlannerViewMode = PlannerSummaryView
                     | PlannerGraphicCompactView
                     | PlannerGraphicBigView
                     | PlannerShelvesReport
                     | PlannerShelvesGroupReport
  deriving (Eq, Read, Show, Enum, Bounded)
instance PathPiece PlannerViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece
