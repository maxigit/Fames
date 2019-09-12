module Handler.Planner.FamesImport
( importFamesDispatch
) where
-- Import methods connecting Fames live database to the planner
import Import
import Planner.Types
import Planner.Internal
import WarehousePlanner.Report
import qualified Handler.WH.PackingList as PL
import qualified Handler.WH.Boxtake as Box
import qualified Handler.Planner.Exec as Exec
import Database.Persist.Sql (toSqlKey)
import Data.Conduit.List (consume)
import Data.Text(splitOn)
import System.FilePath.Glob(globDir1, compile, match)
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
/file/+[FilePath] FILocalFile
/plannerReport FIPlannerReport:
  /tags/#FilePath/#Text TagReport
  /boxes/#FilePath/#Text BoxReport
  /movesAndTags/#FilePath/#Text MATReport
  /orientations/#FilePath/#Text OrientationReport
  /clone/#FilePath/#Text CloneReport
  /delete/#FilePath/#Text DeleteReport
  /stocktake/#FilePath/#Text StocktakeReport
|]
-- *  Dispatcher
importFamesDispatch :: Section -> Handler (Either Text [Section])
importFamesDispatch (Section ImportH (Right content) _) = do
  sectionss <- forM content $ \uri ->  do
    let (main:tags) = splitOn "#" uri
        pieces = splitOn "/" main
        ret :: Handler Section -> Handler (Either Text [Section])
        ret m = Right . (:[]) <$> m
    case (parseRoute (pieces, [])) of
      Nothing -> return $ Left $ uri <> " is not a valid import"
      Just fi -> case fi of
          FIPackingList plId -> ret $ importPackingList (toSqlKey plId) tags
          FIActiveBoxes -> ret $ importActiveBoxtakes tags
          FIBoxStatusActive prefix -> ret $ importBoxStatus ActiveBoxes prefix tags
          FIBoxStatusAll prefix -> ret $ importBoxStatus AllBoxes prefix tags
          FILocalFile path -> readLocalFiles (intercalate "/" path) tags
          FIPlannerReport report -> case report of
            TagReport path reportParam -> executeReport TagsH path reportParam
            BoxReport path reportParam -> executeReport (BoxesH tags) path reportParam
            MATReport path reportParam -> executeReport (MovesAndTagsH tags) path reportParam
            OrientationReport path reportParam -> executeReport OrientationsH path reportParam
            CloneReport path reportParam -> executeReport (ClonesH tags) path reportParam
            DeleteReport path reportParam -> executeReport DeletesH path reportParam
            StocktakeReport path reportParam -> executeReport (StocktakeH tags) path reportParam
  return $ (fmap concat) $  sequence sectionss
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
  

-- | Read local files using glob pattern(s)
-- Uses the same directory as the planner
readLocalFiles :: FilePath -> [Text] -> Handler (Either Text [Section])
readLocalFiles pat excluded = do
  plannerDir <- appPlannerDir <$> getsYesod appSettings
  files <- liftIO $ globDir1 (compile pat) plannerDir
  let orgs = filter valid files
      exPats = map (compile . unpack . ("/**/" <>) ) excluded
      valid f =  all ($ f) $ fileValid : map (\p -> not . match p) exPats
  contents <- mapM readFile orgs
  let sectionss = traverse (parseScenarioFile . decodeUtf8)  contents
  return $ fmap concat sectionss

--  | Execute the report from another planner
executeReport :: HeaderType -> FilePath -> Text -> Handler (Either Text [Section])
executeReport headerType path reportParam0 = do
  let reportParam = if null reportParam0 then "report" else reportParam0
  scenarioE <- readScenariosFromDir importFamesDispatch path
  today <- todayH
  content <- case scenarioE of
    Left _ ->  error $ "Scenario: " <> unpack path <> " doesn't exist"
    Right scenarios -> do
      let report = generateGenericReport today (unpack reportParam)
          scenario = mconcat scenarios
      rows <- Exec.renderReport scenario report 
      return $ map pack rows
  -- add a dummy csv header if needed
  let addHeader content =
        if writeHeader headerType `elem` ["Boxes", "Moves", "Tags", "MovesAndTags"
                                         , "Orientations", "TransformTags", "Stocktake"
                                         ]
        then ("selector,action":content)
        else content
  return $ Right [Section headerType
                          (Right $ addHeader content)
                          ("* " <> writeHeader headerType
                            <> " from " <> pack path <>"[" <> tshow today <> "]"
                          )
                 ]
      


  



