module Planner.Internal where

import ClassyPrelude.Yesod hiding (Content)
import WarehousePlanner.Display
import WarehousePlanner.Base
import WarehousePlanner.Csv
import Planner.Types
import Control.Monad.ST (runST, stToIO, RealWorld)
import Control.Monad.State (evalStateT,runStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Writer (tell, execWriter)
import Data.Text(strip)
import qualified Data.Text as Text
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension)

import Unsafe.Coerce (unsafeCoerce)
import Model.DocumentKey

-- * Example
warehouseExamble :: WH (ShelfGroup s) s
warehouseExamble  = do
  let dim0 = Dimension 270 80 145
  let dim1 = Dimension 31 34 72
  shelves <- mapM (  \i -> newShelf ("A1" <> show i) Nothing dim0  dim0 DefaultOrientation ColumnFirst ) [1..50]
  let shelfid = shelfId (headEx shelves)
  boxes <- mapM (\i -> newBox "style" (show i) dim1 up shelfid [up] []) [1..300]
  moveBoxes ExitLeft boxes shelves
  -- rearrangeShelves [shelf, shelf2]
  
  return $ ShelfGroup (map (ShelfProxy .shelfId) shelves) Vertical


-- * Parsing
-- | Read and cut a scenario file into different component
parseScenarioFile :: Text -> Either Text [Section]
parseScenarioFile text = do -- Either
  lineTypes <- traverse parseLine (map strip $ lines text) 
  sequence $ linesToSections lineTypes
  

-- | Transform a line of text to a typed line
parseLine :: Text -> Either Text TypedLine
parseLine line | "-- END " `isPrefixOf` line          = Right EndL
               | ":END:" `isPrefixOf` line            = Right CommentL
               | "-- " `isPrefixOf` line              = Right $ CommentL
               | Just drawer <- extractDrawer line      = drawer
               | Just sha <- stripPrefix "@" line     = Right $ HashL (DocumentHash $ strip sha)
               | Just headerType <- extractHeader line=  Right $ HeaderL headerType line
               | strip line                 == ""     = Right $ CommentL
               | otherwise                            = Right $ TextL line


-- | Regroup lines into section
linesToSections :: [TypedLine] -> [Either Text Section]
linesToSections lines = reverse $ go lines (MovesH, "") ([]) [] where
  go :: [TypedLine] -> (HeaderType, Text) -> [Text] -> [Either Text Section] -> [Either Text Section]
  go [] header current sections = merge header current sections
  go ((CommentL):ls) header current sections = go ls header current sections
  go ((EndL):_) header current sections = go [] header current sections
  go ((HeaderL newHeader title):ls) header current sections = -- start new section
        go ls (newHeader, title) [] (merge header current sections)
  go ((TextL txt):ls) header current sections = go ls header (txt:current) sections
  -- Only one hash can be in a section. Close the previous section and open a new one at the same time
  go ((HashL sha):ls) header@(ht,title) [] sections = go ls header [] (Right (Section ht (Left sha) title) :sections)
  go ls@((HashL _):_) header current sections = go ls header [] (go [] header current sections)
  merge :: (HeaderType, Text)  -> [Text] -> [Either Text Section] -> [Either Text Section]
  merge (TitleH, title) [] sections = Right (Section TitleH (Right []) title): sections
  merge header [] sections = sections
  merge header@(ht,title) current sections = Right (Section ht (Right (reverse current)) title) : sections

-- | Header is like "*** [HEADER] title"
extractHeader :: Text -> Maybe HeaderType
extractHeader line = case words line of
  (stars:section:_) | isStars stars -> Just TitleH
  _ -> Nothing
  where isStars = all ((==) '*') 
  
extractDrawer :: Text -> Maybe (Either Text TypedLine)
extractDrawer s = do
  drawerE <- stripPrefix ":" s >>= stripSuffix ":" >>= Just . parseDrawer
  return $ fmap (\d -> HeaderL d "") drawerE

parseDrawer :: Text -> Either Text HeaderType
parseDrawer h = case toLower (strip h) of
  "layout" -> Right LayoutH
  "shelves" -> Right ShelvesH
  "initial" -> Right InitialH
  "stocktake" -> Right StocktakeH
  "boxes" -> Right BoxesH
  "moves" -> Right MovesH
  "tags" -> Right TagsH
  "moves and tags" -> Right MovesAndTagsH
  "movesandtags" -> Right MovesAndTagsH
  "mat" -> Right MovesAndTagsH
  "tags and moves" -> Right MovesAndTagsH
  "tagsandmoves" -> Right MovesAndTagsH
  "tam" -> Right MovesAndTagsH
  "transform" -> Right TransformTagsH
  "transform tags" -> Right TransformTagsH
  "orientations" -> Right OrientationsH
  _ -> Left $ h <> " is not a valid drawer."
  

writeHeader :: HeaderType -> Text
writeHeader header = let
  u = toUpper (tshow header)
  in Text.init u

-- | Read a scenario text file. Needs IO to cache each sections into
-- in tempory file. 
-- The reason for that is the we are only doing a wrapper over
-- the legacy planner which works by parsing files using cassava.
-- To work, we need to save each section to a temporary file which
-- can be read by the legacy planner.
-- To avoid creating the same temporary file over and over, which
-- just cache them once and use a SHA identify them.
readScenario :: MonadIO m => Text -> m (Either Text Scenario)
readScenario text = do
  runExceptT $ do
    sections <-   ExceptT . return $ (parseScenarioFile text)
    steps' <- mapM (\s -> ExceptT $ cacheSection s) sections
    ExceptT . return $ makeScenario steps' 

-- | Read a sceanrio from a directory. Only read '.org'
-- Concatene all files in alphabetical order
-- readScenarioDir :: MonadIO m => FilePath -> m Either [Step]
readScenariosFromDir :: MonadIO io => FilePath -> io (Either Text [Scenario])
readScenariosFromDir path = liftIO $ do
  entries0 <- listDirectory path
  let entries = map (path </>) (sort  entries0)
      isOrg = (== ".org") . takeExtension
  files <- filterM  doesFileExist (filter isOrg entries)
  contents <-  mapM readFile files
  scenariosE <- mapM (readScenario . decodeUtf8) contents
  return $ sequence scenariosE
  
savePointScenario = Scenario Nothing [SavingPoint] Nothing

-- | Save a content to a temporary file if needed
cacheContent :: MonadIO m => Content -> m (Either Text DocumentHash)
cacheContent (Left sha) = do
  -- check it exists
  exist <- liftIO $ doesFileExist (contentPath sha)
  return $ if exist
           then Right sha
           else Left $ "No file found for SHA: " <> (unDocumentHash sha)
cacheContent (Right texts) = do
  let bs = encodeUtf8 $ Text.unlines texts
      key = computeDocumentKey bs
  writeFile (contentPath key) bs

  return (Right key)

contentPath :: DocumentHash -> FilePath
contentPath (DocumentHash file) = "/tmp/DocumentCache/planner-" <> unpack file


-- | Saves files and replace them by their hash
cacheSection :: MonadIO m => Section -> m (Either Text (HeaderType, (DocumentHash, Text)))
cacheSection (Section InitialH content _) = do
  case content of
    Left sha -> do
      -- TODO: 
      -- wh <- cacheWarehouseOut sha
      -- return $ case wh of
        -- Nothing -> Left "Initial state doesn't exits anymore"
        return $ Right (InitialH, (sha, "* Initial"))
    Right  _ -> return $ Left "Initial section needs a SHA (@...)"
cacheSection Section{..} = runExceptT $ do
  sha <- ExceptT $ cacheContent sectionContent
  return $ (sectionType, (sha, sectionTitle))
  
-- | Load files and replace them by their text content
unCacheSection :: MonadIO m => Section -> m Section
unCacheSection section = do
  content <- case sectionContent section of
                Right text -> return $ Right text
                Left sha -> do
                  contentM <- retrieveContent sha
                  return $ maybe (Left sha) (Right . lines) contentM
  return $ section{sectionContent=content}

retrieveContent :: MonadIO m => DocumentHash -> m (Maybe Text)
retrieveContent key = do
  let path = contentPath key
  exist <- liftIO $ doesFileExist  path
  if exist
    then (Just . decodeUtf8) <$> readFile path
    else return Nothing

makeScenario :: [(HeaderType, (DocumentHash, Text))]  -> Either Text Scenario
makeScenario sections0 = do -- Either
  let (initials, sections1) = partition (( InitialH==). fst) sections0
      (layouts, sections2) = partition (( LayoutH==). fst) sections1
      firstOrNone _ [] = Right Nothing
      firstOrNone _ [x] = Right (Just $ snd x)
      firstOrNone err _ = Left err

  initial <- firstOrNone "Too many INITIAL sections" initials
  layout <- firstOrNone "Too many LAYOUT sections" (take 1 $ reverse layouts) -- take last one

  let steps = map (\(header , (sha, title)) ->  Step header sha title) sections2

  Right $ Scenario (map fst initial) steps (map fst layout)

-- * Pretty Printing
scenarioToTextWithHash :: Scenario -> Text
scenarioToTextWithHash scenario = sectionsToText $ scenarioToSections scenario

sectionsToText :: [Section] -> Text
sectionsToText sections = unlines $ concatMap sectionToText sections

emptyHash = computeDocumentKey ""
sectionToText :: Section -> [Text]
sectionToText Section{..} = execWriter $ do
    tell [sectionTitle] --  $ ["* " <> writeHeader sectionType]
    case sectionContent of
       Left k@(DocumentHash key) -> if k /= emptyHash then tell ["@" <> key] else return ()
       Right texts -> tell texts
    return ()

scenarioToFullText :: MonadIO m => Scenario -> m Text
scenarioToFullText scenario =  do
  let sections = scenarioToSections scenario
  expanded <- mapM unCacheSection sections
  return $ sectionsToText expanded


scenarioToSections :: Scenario -> [Section]
scenarioToSections Scenario{..} = execWriter $ do  -- []
  forM sInitialState (\state -> tell [Section InitialH (Left state) "* INITIAL"])
  forM sLayout (\layout -> tell [Section LayoutH (Left layout) "* LAYOUT"])
  forM sSteps (\s -> case s of
                  Step header sha title -> tell [Section header (Left sha) title]
                  SavingPoint -> return ()
              )

-- | Key identifying the scenario. Takes all document and has them.
scenarioKey :: Scenario -> DocumentHash
-- if a scenario is an empty initial state we use the same key as the original warehouse/scenario
scenarioKey (Scenario (Just key) [] Nothing) = key
scenarioKey sc = computeDocumentKey .  encodeUtf8 $ scenarioToTextWithHash  sc

-- | Key indentifying the warehouse scenario, i.e. not taking the layout into account
warehouseScenarioKey ::  Scenario -> DocumentHash
warehouseScenarioKey sc = scenarioKey (sc {sLayout = Nothing, sSteps = sSortedSteps sc} )

-- some steps need to be done before other to make sense
-- shelves first, then orientations rules, then in inital order
sSortedSteps :: Scenario -> [Step]
sSortedSteps Scenario{..} = let
  steps = zipWith key sSteps  [1..]
  key step i = ((priority step, i), step)
  priority step = case step of
                    Step ShelvesH _ _  -> 1
                    Step OrientationsH _ _  -> 2
                    _ -> 3
  sorted = sortBy (comparing fst) steps
  in  map snd sorted

-- * Rendering


warehouseToDiagram warehouse = do
  let exec = do
        group <- warehouse
        renderGroup group
  diag <- execWH0 warehouse exec
  return diag

execWH0 wh = execWH emptyWarehouse

execWH warehouse0 wh = liftIO $ stToIO $ evalStateT wh warehouse0

-- runWH :: MonadIO m => Warehouse s -> WH a s -> m (a, Warehouse s)
runWH warehouse0 wh = liftIO $ stToIO $ runStateT wh warehouse0

executeStep :: Step -> IO (WH () s)
executeStep SavingPoint = return (return ())
executeStep (Step header sha _) =
  let path = contentPath sha
      defaultOrientations = [tiltedForward, tiltedFR]
      splitStyle s = let (style, colour) = splitAt 8 s
                       in (style, drop 1 colour)
      execute step = do
        s <- step
        return (s >> return ())
  in case header of
          LayoutH -> return $ return ()
          ShelvesH -> execute $ readShelves2 BoxOrientations path
          InitialH -> return $ return ()
          StocktakeH -> execute $ readStockTake defaultOrientations splitStyle path
          BoxesH -> execute $ readBoxes defaultOrientations splitStyle path
          MovesH -> execute $ readMoves path
          TagsH -> execute $ readTags path
          MovesAndTagsH -> execute $ readMovesAndTags path
          OrientationsH -> execute $ setOrientationRules defaultOrientations path
          TransformTagsH -> execute $ readTransformTags path
          TitleH -> return $ return ()

-- | Retrieve the number of line in the layout file
scenarioLayoutSize :: MonadIO m => Scenario -> m Int
scenarioLayoutSize Scenario{..} = 
  case sLayout of
    Nothing -> return 0
    Just layout -> do
      l <- retrieveContent layout
      return (maybe 0 (length . lines) l)
