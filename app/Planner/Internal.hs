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
import System.Directory (doesFileExist)

import Unsafe.Coerce (unsafeCoerce)
import Model.DocumentKey

-- * Example
warehouseExamble :: WH (ShelfGroup s) s
warehouseExamble  = do
  let dim0 = Dimension 270 80 145
  let dim1 = Dimension 31 34 72
  shelves <- mapM (  \i -> newShelf ("A1" <> show i) Nothing dim0  dim0 DefaultOrientation ColumnFirst ) [1..50]
  let shelfid = shelfId (headEx shelves)
  boxes <- mapM (\i -> newBox "style" (show i) dim1 up shelfid [up] ) [1..300]
  moveBoxes boxes shelves
  -- rearrangeShelves [shelf, shelf2]
  
  return $ ShelfGroup (map (ShelfProxy .shelfId) shelves) Vertical

-- * Parsing
-- | Read and cut a scenario file into different component
parseScenarioFile :: Text -> Either Text [Section]
parseScenarioFile text = do -- Either
  lineTypes <- traverse parseLine (lines text) 
  sequence $ linesToSections lineTypes
  

-- | Transform a line of text to a typed line
parseLine :: Text -> Either Text TypedLine
parseLine line | "-- END " `isPrefixOf` line          = Right EndL
               | "-- " `isPrefixOf` line              = Right $ CommentL
               | Just sha <- stripPrefix "@" line = Right $ HashL (DocumentHash sha)
               | Just header <- stripPrefix "* " line  = case parseHeader header of
                   Nothing -> Left $ line <> " is an invalid header type."
                   Just h -> Right (HeaderL h)
               | strip line                 == "" = Right $ CommentL
               | otherwise                        = Right $ TextL line

-- | Regroup lines into section
linesToSections :: [TypedLine] -> [Either Text Section]
linesToSections lines = reverse $ go lines MovesH ([]) [] where
  go :: [TypedLine] -> HeaderType -> [Text] -> [Either Text Section] -> [Either Text Section]
  go [] header current sections = merge header current sections
  go ((CommentL):ls) header current sections = go ls header current sections
  go ((EndL):_) header current sections = go [] header current sections
  go ((HeaderL newHeader):ls) header current sections = -- start new section
        go ls newHeader [] (merge header current sections)
  go ((TextL txt):ls) header current sections = go ls header (txt:current) sections
  -- Only one hash can be in a section. Close the previous section and open a new one at the same time
  go ((HashL sha):ls) header [] sections = go ls header [] (Right (Section header (Left sha)) :sections)
  go ls@((HashL _):_) header current sections = go ls header [] (go [] header current sections)
  merge :: HeaderType  -> [Text] -> [Either Text Section] -> [Either Text Section]
  merge header [] sections = sections
  merge header current sections = Right (Section header (Right (reverse current))) : sections


parseHeader :: Text -> Maybe HeaderType
parseHeader h = case toLower (strip h) of
  "layout" -> Just LayoutH
  "shelves" -> Just ShelvesH
  "initial" -> Just InitialH
  "stocktake" -> Just StocktakeH
  "boxes" -> Just BoxesH
  "moves" -> Just MovesH
  _ -> Nothing
  
writeHeader :: HeaderType -> Text
writeHeader header = let
  u = toUpper (tshow header)
  in Text.init u

-- | Read a scenario text file. Needs IO to cache the section to
-- tempory file
readScenario :: MonadIO m => Text -> m (Either Text Scenario)
readScenario text = do
  runExceptT $ do
    sections <-   ExceptT . return $ (parseScenarioFile text)
    steps' <- mapM (\s -> ExceptT $ cacheSection s) sections
    ExceptT . return $ makeScenario steps'
  
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
contentPath (DocumentHash file) = "/tmp/" <> unpack file


cacheSection :: MonadIO m => Section -> m (Either Text (HeaderType, DocumentHash))
cacheSection Section{..} = runExceptT $ do
  sha <- ExceptT $ cacheContent sectionContent
  return $ (sectionType, sha)
  

retrieveContent :: MonadIO m => DocumentHash -> m (Maybe Text)
retrieveContent key = do
  let path = contentPath key
  exist <- liftIO $ doesFileExist  path
  if exist
    then (Just . decodeUtf8) <$> readFile path
    else return Nothing


makeScenario :: [(HeaderType, DocumentHash)]  -> Either Text Scenario
makeScenario sections0 = do -- Either
  let (initials, sections1) = partition (( InitialH==). fst) sections0
      (layouts, sections2) = partition (( LayoutH==). fst) sections1
      firstOrNone _ [] = Right Nothing
      firstOrNone _ [x] = Right (Just $ snd x)
      firstOrNone err _ = Left err

  initial <- firstOrNone "Too many INITIAL sections" initials
  layout <- firstOrNone "Too many LAYOUT sections" layouts

  let steps = map (uncurry Step) sections2

  Right $ Scenario initial steps layout

-- warehouseFromOrg :: WH (ShelfGroup s)
warehouseFromOrg text = warehouseExamble

-- * Pretty Printing
scenarioToTextWithHash :: Scenario -> Text
scenarioToTextWithHash Scenario{..} = execWriter $ do -- []
  let printSha (DocumentHash key) = tell ("@" <> key)
  forM sInitialState $ \state -> do
    tell $ "* INITIAL"
    printSha state

  forM sLayout $ \layout -> do
    tell "* LAYOUT"
    printSha layout

  forM sSteps $ \(Step header step) -> do
    tell $ "* " <> writeHeader header
    printSha step
  

-- | Key identifying the scenario
scenarioKey :: Scenario -> DocumentHash
scenarioKey sc = computeDocumentKey .  encodeUtf8 $ scenarioToTextWithHash  sc

-- | Key indentifying the warehouse scenario, i.e. not taking the layout into account
warehouseScenarioKey ::  Scenario -> DocumentHash
warehouseScenarioKey sc = scenarioKey (sc {sLayout = Nothing})


-- * Rendering


warehouseToDiagram warehouse = do
  let exec = do
        group <- warehouse
        renderGroup group
  diag <- execWH0 warehouse exec
  return diag

execWH0 wh = execWH emptyWarehouse

execWH warehouse0 wh = lift $ stToIO $ evalStateT wh warehouse0

-- runWH :: MonadIO m => Warehouse s -> WH a s -> m (a, Warehouse s)
runWH warehouse0 wh = lift $ stToIO $ runStateT wh warehouse0

executeStep :: Step -> IO (WH () s)
executeStep (Step header sha) =
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
