module Handler.Planner.FamesImport
( importFamesDispatch
) where
-- Import methods connecting Fames live database to the planner
import Import
import Planner.Types
import qualified Handler.WH.PackingList as PL
import qualified Handler.WH.Boxtake as Box
import Database.Persist.Sql (toSqlKey)
import Data.Conduit.List (consume)
import Data.Text(splitOn)
-- * Type
-- data FamesImport
--   = ImportPackingList (Key PackingList) -- ^ import packing list
--   -- | BoxTag
--   deriving (Eq, Show)

-- | Create a Route type for each type of import
-- This is a just a quick way of generate `parseRoute`
-- which helps us to convert a URI to data type
-- that we can dispatch on later
data FI
mkYesodSubData "FI" [parseRoutes|
/packingList/#Int64 FIPackingList
/activeBoxes FIActiveBoxes
/boxStatus/active/#Text FIBoxStatusActive
/boxStatus/all/#Text FIBoxStatusAll
|]
-- *  Dispatcher
importFamesDispatch :: Section -> Handler (Either Text [Section])
importFamesDispatch (Section ImportH (Right content) _) = do
  sections <- forM content $ \uri ->  do
    let (main:tags) = splitOn "#" uri
        pieces = splitOn "/" main
    case (parseRoute (pieces, [])) of
      Nothing -> return $ Left $ uri <> " is not a valid import"
      Just fi -> Right <$> case fi of
          FIPackingList plId -> importPackingList (toSqlKey plId) tags
          FIActiveBoxes -> importActiveBoxtakes tags
          FIBoxStatusActive prefix -> importBoxStatus ActiveBoxes prefix tags
          FIBoxStatusAll prefix -> importBoxStatus AllBoxes prefix tags
  return $ sequence sections
importFamesDispatch section = return $ Right [section]


-- * Importers
-- | Imports undelivered boxes from packing list.
-- We only import undeliverd one because the delivered one are probablly in the planner somewhere
importPackingList :: Key PackingList -> [Text] -> Handler Section
importPackingList key tags =  runDB $ do
  packingList <- getJust key
  detailEs <- selectList [PackingListDetailPackingList ==. key, PackingListDetailDelivered ==. False]
                         [Asc PackingListDetailId]
  let texts = PL.toPlanner PL.WithDetails packingList detailEs
  return $ Section (BoxesH tags) (Right texts) ("** Import Packing List " <> tshow key)


-- | Create a STOCKTAKE sections
-- will all the boxes alives
importActiveBoxtakes :: [Text] -> Handler Section
importActiveBoxtakes tags = do
  today <- todayH
  let source = Box.boxSourceToCsv Box.plannerSource
  content <- (runDB $ runConduit $ source .| consume)
  return $ Section (StocktakeH tags) (Right content) ("* Stocktake from Fames DB [" <> tshow today <> "]")

  


-- | Generate a TAG files from barcode
-- with scan and status information.
-- The main difference with ActiveBoxtakes
-- is that it doesn't create boxes but only tags
data WhichBoxes = AllBoxes | ActiveBoxes
importBoxStatus whichBoxes prefix a_tags = do
  today <- todayH
  operators <- allOperators
  let source = case whichBoxes of
        AllBoxes -> selectSource [] []
        ActiveBoxes -> Box.plannerSource
      -- prefix = fromMaybe "" a_prefix
      getTags (Entity _ Boxtake{..}) = "#barcode=" <> boxtakeBarcode <> "," <> (intercalate "#" tags)
              where
                tags = [ prefix <> "location="  <> boxtakeLocation
                       , prefix <> "status=" <> if boxtakeActive then "active" else "inactive"
                       , prefix <> "date=" <> tshow boxtakeDate
                       , prefix <> "reference=" <> boxtakeReference
                       , prefix <> "operator=" <> maybe (tshow $ unOperatorKey $ boxtakeOperator)
                                                        operatorNickname
                                                        (lookup boxtakeOperator operators)
                       ] <> a_tags
      header = "selector,tags"
  rows <- (runDB $ runConduit $ source .| mapC getTags .| consume)
  let content = header:rows
  return $ Section (TagsH) (Right content) ("* Tags from Fames DB [" <> tshow today <> "]")
  
  
  

  



