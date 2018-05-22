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


-- * Items
-- ** ItemIndex
data ItemViewMode = ItemGLView
                   | ItemPriceView
                   | ItemPurchaseView
                   | ItemFAStatusView
                   | ItemWebStatusView
                   | ItemCategoryView
                   | ItemAllView -- Not supposed to be used as a view
  deriving (Eq, Read, Show, Enum, Bounded)
instance PathPiece ItemViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece
-- ** Reports
data ReportMode = ReportTable
                | ReportChart
                | ReportCsv
                | ReportRaw
     deriving (Eq, Read, Show, Enum, Bounded)
instance PathPiece ReportMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece
                


-- * Planner
data PlannerViewMode = PlannerSummaryView
                     | PlannerGraphicCompactView
                     | PlannerGraphicBigView
                     | PlannerShelvesReport
                     | PlannerShelvesGroupReport
                     | PlannerAllReport
                     | PlannerBestBoxesFor 
                     | PlannerBestShelvesFor 
                     | PlannerBestAvailableShelvesFor 
                     | PlannerGenerateMoves
                     | PlannerScenarioHistory
  deriving (Eq, Read, Show, Enum, Bounded)
instance PathPiece PlannerViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece
