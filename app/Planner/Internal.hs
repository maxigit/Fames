{-# LANGUAGE TypeOperators #-}
module Planner.Internal
( scenarioKey
, scenarioLayoutSize
, runWH
, warehouseScenarioKey
, executeStep
, execWH
, contentPath
, sSortedSteps
, readScenariosFromDir
, readScenarioFromPath
, readScenario
, savePointScenario
, scenarioToTextWithHash
, parseScenarioFile
, fileValid
, writeHeader
, scenarioToFullText
)
where 

import ClassyPrelude.Yesod hiding (Content)
import WarehousePlanner.Base
import WarehousePlanner.Csv
import Planner.Types
import Control.Monad.ST (stToIO)
import Control.Monad.State (evalStateT,runStateT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Writer (tell, execWriter)
import Data.Text(strip,splitOn)
import qualified Data.Text as Text
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension)

import Model.DocumentKey
import GHC.Generics

-- * Example
warehouseExamble :: WH (ShelfGroup s) s
warehouseExamble  = do
  let dim0 = Dimension 270 80 145
  let dim1 = Dimension 31 34 72
  shelves <- mapM (  \i -> newShelf ("A1" <> tshow i) Nothing dim0  dim0 DefaultOrientation ColumnFirst ) [1..50]
  let shelfid = shelfId (headEx shelves)
  boxes <- mapM (\i -> newBox "style" (tshow i) dim1 up shelfid [up] []) [1..300]
  _ <- moveBoxes ExitLeft boxes shelves
  -- rearrangeShelves [shelf, shelf2]
  
  return $ ShelfGroup (map (ShelfProxy .shelfId) shelves) Vertical

__use_binding = warehouseExamble

-- * Parsing
-- | Read and cut a scenario file into different component
parseScenarioFile :: Text -> Either Text [Section]
parseScenarioFile text = do -- Either
  lineTypes <- traverse parseLine (map strip $ lines text) 
  sequence $ linesToSections lineTypes
  

-- | Transform a line of text to a typed line
parseLine :: Text -> Either Text TypedLine
parseLine line | "-- END " `isPrefixOf` line          = Right EndL
               | ":END:" `isPrefixOf` line            = Right EndSectionL
               | "-- " `isPrefixOf` line              = Right $ CommentL
               | Just drawer <- extractDrawer line      = drawer
               | Just sha <- stripPrefix "@" line     = Right $ HashL (DocumentHash $ strip sha)
               | Just headerType <- extractHeader line=  Right $ HeaderL headerType line
               | strip line                 == ""     = Right $ CommentL
               | otherwise                            = Right $ TextL line


-- | Regroup lines into section
linesToSections :: [TypedLine] -> [Either Text Section]
linesToSections lines_ = reverse $ go lines_ Nothing [] [] where
  go :: [TypedLine] -- ^ lines_ to parse
     -> Maybe (HeaderType, Text) --  ^ header for current body
     -> [Text] -- ^ line of the current body in reverse order
     -> [Either Text Section] -- ^ previously parsed in reverse order
     -> [Either Text Section]
  go ((HeaderL newHeader title):ls) header current sections = -- start new section
    go ls (Just (newHeader, title)) [] (merge header current sections)
  go [] Nothing __current sections = sections
  go [] header current sections = merge header current sections
  go ((CommentL):ls) header current sections = go ls header current sections
  go ((EndL):_) header current sections = go [] header current sections
  go ((EndSectionL):ls) header current sections = -- close section 
        go ls Nothing [] (merge header current sections)
  -- not in section, skip
  go ((TextL _):ls) Nothing current sections = go ls Nothing current sections
  go ((TextL txt):ls) header current sections = go ls header (txt:current) sections
  -- Only one hash can be in a section. Close the previous section and open a new one at the same time
  go ((HashL sha):ls) header@(Just (ht,title)) [] sections = go ls header [] (Right (Section ht (Left sha) title) :sections)
  go ls@((HashL _):_) header current sections = go ls header [] (go [] header current sections)
  -- 
  -- Close the current section and  merge similar section if possible
  merge :: Maybe (HeaderType, Text)  -> [Text] -> [Either Text Section] -> [Either Text Section]
  merge Nothing _ sections = sections
  merge (Just (TitleH, title)) [] sections = Right (Section TitleH (Right []) title): sections
  merge __header [] sections = sections
  merge (Just (ht,title)) current sections = Right (Section ht (Right (reverse current)) title) : sections

-- | Header is like "*** [HEADER] title"
extractHeader :: Text -> Maybe HeaderType
extractHeader line = case words line of
  (stars:__section:_) | isStars stars -> Just TitleH
  _ -> Nothing
  where isStars = all ((==) '*') 
  
extractDrawer :: Text -> Maybe (Either Text TypedLine)
extractDrawer s = do
  drawerE <- stripPrefix ":" s >>= stripSuffix ":" >>= Just . parseDrawer
  return $ fmap (\d -> HeaderL d "") drawerE

parseDrawer :: Text -> Either Text HeaderType
parseDrawer h = case splitOn "_" h of
  [] -> Left $ "'' not a valid drawer."
  (x:xs) -> case toLower (strip x):xs of
    ("layout":[]) -> Right LayoutH
    ("shelves":[]) -> Right ShelvesH
    ("initial":[]) -> Right InitialH
    ("stocktake":tags) -> Right $ StocktakeH tags
    ("boxes":tags) -> Right $ BoxesH tags
    ("moves":"and":"tags":tags) -> Right $ MovesAndTagsH tags
    ("moves":tags) -> Right $ MovesH tags
    ("tags":[]) -> Right TagsH
    ("moves and tags":tags) -> Right $ MovesAndTagsH tags
    ("movesandtags":tags) -> Right $ MovesAndTagsH tags
    ("mat":tags) -> Right $ MovesAndTagsH tags
    ("tags and moves":tags) -> Right $ MovesAndTagsH tags
    ("tagsandmoves":tags) -> Right $ MovesAndTagsH tags
    ("tam":tags) -> Right $ MovesAndTagsH tags
    ("shelf tags":[]) -> Right $ ShelfTagsH
    ("shelftags":[]) -> Right $ ShelfTagsH
    ("shelf":"tags":[]) -> Right $ ShelfTagsH
    ("tag shelves":[]) -> Right $ ShelfTagsH
    ("transform":[]) -> Right TransformTagsH
    ("transform tags":[]) -> Right TransformTagsH
    ("orientations":[]) -> Right OrientationsH
    ("clone":tags) -> Right $ ClonesH tags
    ("clones":tags) -> Right $ ClonesH tags
    ("delete":[]) -> Right DeletesH
    ("deletes":[]) -> Right DeletesH
    ("import":[]) -> Right ImportH
    _parsed -> Left $ h <> " is not a valid drawer."
  

class GWriteHeader f where
  gwriteHeader :: f a -> Text

instance GWriteHeader U1 where -- Single constructor
  gwriteHeader _ = ""
instance (GWriteHeader a , GWriteHeader b) => GWriteHeader (a :+: b) where -- Single constructor
  gwriteHeader (L1 x) = gwriteHeader x
  gwriteHeader (R1 x) = gwriteHeader x
instance GWriteHeader (K1 i [Text] ) where
  gwriteHeader (K1 []) = ""
  gwriteHeader (K1 xs) = "_" <> intercalate "_" xs
instance GWriteHeader a => GWriteHeader (D1 c a) where
  gwriteHeader (M1 x) = gwriteHeader x 
-- | get the constructor and remove the trailing H
instance (Constructor c, GWriteHeader a) => GWriteHeader (C1 c a) where
  gwriteHeader m@(M1 x) = (Text.init . pack $ conName m) <>  gwriteHeader x
instance GWriteHeader a => GWriteHeader (S1 c a) where
  gwriteHeader (M1 x) = gwriteHeader x

    
writeHeader :: HeaderType -> Text
writeHeader = gwriteHeader . from
  -- case header of
  --       StocktakeH tags -> intercalate "#" ("Stockake":tags)
  --       BoxesH tags -> intercalate "#" ("Boxes":tags)
  --       ClonesH tags -> intercalate "#" ("Clones":tags)
  --       _ -> Text.init $ toUpper (tshow header)
  --       --    ^ remove the trailing H

-- | Read a scenario text file. Needs IO to cache each sections into
-- in tempory file. 
-- The reason for that is the we are only doing a wrapper over
-- the legacy planner which works by parsing files using cassava.
-- To work, we need to save each section to a temporary file which
-- can be read by the legacy planner.
-- To avoid creating the same temporary file over and over, which
-- just cache them once and use a SHA identify them.
readScenario :: MonadIO m
             => (Section -> m (Either Text [Section])) -- ^ section expander, mainly to import sections for URI
             -> Text
             -> m (Either Text Scenario)
readScenario expandSection text = do
  runExceptT $ do
    sections0 <-   ExceptT . return $ (parseScenarioFile text)
    sections <- concat <$> ExceptT ( sequence <$> mapM expandSection sections0)
    steps' <- mapM (\s -> ExceptT $ cacheSection s) sections
    ExceptT . return $ makeScenario steps' 

-- | Read a sceanrio from a directory. Only read '.org'
-- Concatenate all files in alphabetical order
-- readScenarioDir :: MonadIO m => FilePath -> m Either [Step]
readScenariosFromDir :: MonadIO io
                     => (Section -> io (Either Text [Section]))
                     -- ^ section expander, mainly to import sections for URI
                     -> FilePath -> io (Either Text [Scenario])
readScenariosFromDir expandSection path = do
  contents <- liftIO $ do
    entries0 <- listDirectory path
    let entries = map (path </>) (sort  entries0)
    files <- filterM  doesFileExist (filter fileValid entries)
    mapM readFile files
  scenariosE <- mapM (readScenario expandSection . decodeUtf8) contents
  return $ sequence scenariosE

-- | Read one scenario file
readScenarioFromPath :: MonadIO io
                     => (Section -> io (Either Text [Section]))
                     -- ^ section expander, mainly to import sections for URI
                     -> FilePath -> io (Either Text Scenario)
readScenarioFromPath expandSection path = do
  exists <-  liftIO $ doesFileExist path
  if exists
    then do
      content <- liftIO $ readFile path
      readScenario expandSection $ decodeUtf8 content
    else
      return $ Left $ "File " <> tshow path <> "doesn't exist."

  
fileValid :: FilePath -> Bool
fileValid = (== ".org") . takeExtension

savePointScenario :: Scenario
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

emptyHash :: DocumentHash
emptyHash = computeDocumentKey ""
sectionToText :: Section -> [Text]
sectionToText Section{..} = execWriter $ do
    tell [sectionTitle] --  $ ["* " <> writeHeader sectionType]
    case sectionContent of
       Left k@(DocumentHash key) -> if k /= emptyHash then tell ["@" <> key] else return ()
       Right texts -> tell [":" <> writeHeader sectionType <> ":"] >> tell texts >> tell [":END:"]
    return ()

scenarioToFullText :: MonadIO m => Scenario -> m Text
scenarioToFullText scenario =  do
  let sections = scenarioToSections scenario
  expanded <- mapM unCacheSection sections
  return $ sectionsToText expanded


scenarioToSections :: Scenario -> [Section]
scenarioToSections Scenario{..} = execWriter $ do  -- []
  _ <- forM sInitialState (\state -> tell [Section InitialH (Left state) "* INITIAL"])
  _ <- forM sLayout (\layout -> tell [Section LayoutH (Left layout) "* LAYOUT"])
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
execWH :: MonadIO m => Warehouse RealWorld -> WH a RealWorld -> m a
execWH warehouse0 wh = liftIO $ stToIO $ evalStateT wh warehouse0

runWH :: MonadIO m => Warehouse RealWorld -> WH a RealWorld -> m (a, Warehouse RealWorld)
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
        return (s >> clearCache)
  in case header of
          LayoutH -> return $ return ()
          ShelvesH -> execute $ readShelves2 BoxOrientations path
          InitialH -> return $ return ()
          StocktakeH tags -> execute $ readStockTake tags defaultOrientations splitStyle path
          BoxesH tags -> execute $ readBoxes tags defaultOrientations splitStyle path
          MovesH tags -> execute $ readMoves tags path
          TagsH -> execute $ readTags path
          MovesAndTagsH tags -> execute $ readMovesAndTags tags path
          ShelfTagsH -> execute $ readShelfTags path
          OrientationsH -> execute $ setOrientationRules defaultOrientations path
          TransformTagsH -> execute $ readTransformTags path
          ClonesH tags -> execute $ readClones (tags) path
          DeletesH -> execute $ readDeletes path
          TitleH -> return $ return ()
          ImportH -> return $ return ()

-- | Retrieve the number of line in the layout file
scenarioLayoutSize :: MonadIO m => Scenario -> m Int
scenarioLayoutSize Scenario{..} = 
  case sLayout of
    Nothing -> return 0
    Just layout -> do
      l <- retrieveContent layout
      return (maybe 0 (length . lines) l)





