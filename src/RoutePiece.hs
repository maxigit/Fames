-- | Miscelaneous Types used as route piec.
module RoutePiece where
import ClassyPrelude
import Web.PathPieces

-- * General
-- | Encoding of the file being uploaded.
data Encoding = UTF8 | Latin1 deriving (Show, Read, Eq, Enum, Bounded)

instance PathPiece Encoding where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece

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
                | EditInvoices
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
                | ReportPivot
                | ReportCsv
                | ReportBubble  -- punch card. scatter chart using category (serie) as column
                | ReportScatter -- scatter chart using any measure sales 3 as colum
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
                     | PlannerGenerateMovesWithTags
                     | PlannerGenerateMOPLocations
                     | PlannerGenericReport
                     | PlannerScenarioHistory
                     | PlannerBoxGroupReport -- ^ box dimensions and number summary per style
                     -- | PlannerBoxGroupWarningReport -- ^ only group with different size Boxes
                     -- | PlannerExport -- ^ Reexport a planner. Can be usefull to see where is what
  deriving (Eq, Read, Show, Enum, Bounded)
instance PathPiece PlannerViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece

-- * GL
-- ** Tax Report
data TaxReportViewMode = TaxReportPendingView -- ^ display transaction to collect
                       | TaxReportCollectedView -- ^ transaction  already collected
                       | TaxReportBucketView 
                       | TaxReportBoxesView
                       | TaxReportConfigChecker
     deriving (Eq, Read, Show, Enum, Bounded)

instance PathPiece TaxReportViewMode where
  fromPathPiece = readFromPathPiece
  toPathPiece = showToPathPiece
