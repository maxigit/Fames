{-# LANGUAGE ImplicitParams #-}
module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
, getPDocR
, getPScenarioImageR
, getPScenarioImageForR
)

where

import Data.List((!!), intersect)
import Data.Dynamic
import Diagrams.Prelude hiding(iso, width, size)
import Handler.Planner.Exec
import WarehousePlanner.Csv(readLayout, readWarehouse)
import Handler.Planner.FamesImport(importFamesDispatch)
import Import hiding(intersect)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import Util.Cache
import WarehousePlanner.Base
import WarehousePlanner.Selector
import WarehousePlanner.Report hiding(report)
import Yesod.Form.Bootstrap3 (bfs)
import qualified Yesod.Media.Simple as M
import Data.Text(splitOn, breakOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import System.Directory (listDirectory)
import GHC.Prim(coerce)

-- * Type 
data ScenarioDisplayMode = NormalM | CompactM | InitialM | ExpandedM deriving (Eq, Show, Read)
data FormParam = FormParam
  { pPlannerPath :: Maybe FilePath --  ^ planner file to load
  , pOrgfile :: Maybe Textarea -- ^ text to add to the content of pPlannerPath
  , pTAMSection :: Maybe Textarea -- ^ Tags and Moves section, don't need header and drawer decoration
  , pDeleteSection :: Maybe Textarea-- ^ Delete section, don't need header and drawer decoration
  , pDisplayMode :: Maybe ScenarioDisplayMode -- ^ How to re-render the scenario in the textarea field
  , pViewMode :: Maybe PlannerViewMode -- ^ How to view the warehouse
  , pParameter :: Maybe Text
  } deriving (Show)

defaultParam :: FormParam
defaultParam = FormParam Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- * Handler 
setInfoToDoc :: Handler ()
setInfoToDoc = do
  doc <- widgetToPageContent plannerDoc
  html <- withUrlRenderer (pageBody doc)
  setInfo $ html
  
{-# NOINLINE getPViewR #-}
getPViewR :: Maybe FilePath -> Maybe PlannerViewMode -> Handler TypedContent
getPViewR pathm viewMode = do
  setInfoToDoc
  renderView defaultParam {pViewMode = viewMode, pPlannerPath = pathm}

{-# NOINLINE postPViewR #-}
postPViewR :: Maybe FilePath -> Maybe PlannerViewMode -> Handler TypedContent
postPViewR _ viewMode = do
  ((resp, _), _) <- runFormPost $ paramForm Nothing
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderView param { pViewMode = viewMode <|> pViewMode param }

{-# NOINLINE getPImageR #-}
getPImageR :: Text -> Int64 -> Int64 -> Handler TypedContent
getPImageR sha width i = do
  today <- todayH
  let ?cache = memoryCache
      ?today = today
  scenarioM <- cacheScenarioOut sha
  -- traceShowM ("IMAGE", scenarioM)
  case scenarioM of
    Nothing -> do
      error "No matching scenario"
    Just (scenario, layoutSize) -> do
        diagE <- renderScenario scenario Nothing
        case diagE of
          Left e -> error e
          Right diags -> cache0 False (cacheMinute 10) ("DIAG", width, i, sha) $
                                sendResponseDiag width (diags !! min (fromIntegral i) (layoutSize-1))
  
getPDocR :: Handler Html
getPDocR = do
  let expanded = True
  let plannerDoc' = $(widgetFile "Planner/doc")
  defaultLayout $ primaryPanel "Planner Quick Reference" plannerDoc'
  -- defaultLayout [whamlet|
  --  <h1.jumbotron>Planner Quick Reference
  --  <div.well>
  --    ^{plannerDoc'}
  --                       |]
{-# NOINLINE getPScenarioImageR #-}
getScenarioImageByNumber :: Text -> Int64 -> Int64 -> Handler TypedContent
getScenarioImageByNumber path width i = do
  let ?cache = memoryCache
  Right scenario <-  readScenarioFromPath False importFamesDispatch $ unpack path
  (sha, _) <- cacheScenarioIn scenario
  getPImageR sha width i
  
-- | Find layout image matching shelf name
getScenarioImageByPattern :: Text -> Int64 -> Text -> Handler TypedContent
getScenarioImageByPattern path width pattern_ = do
  Right scenario <- readScenarioFromPath False importFamesDispatch $ unpack path
  case sLayout scenario of
    Nothing     ->  error $ "No Layout provided"
    Just layout -> do
           contentPath <- contentPathM
           shelvesss <- liftIO $ readLayout $ contentPath layout
           case filter good $ zip (toList shelvesss) [0..] of
                (_, i):_ -> getScenarioImageByNumber path width i
                _        -> getScenarioImageByNumber path width 0
  where good (shelvess, _i) = any (any (applyNameSelector (coerce toMatch) runIdentity)) $ map (map Identity) shelvess
        toMatch = parseNameSelector (pattern_ <> "|" <> pattern_ <> "*.*/*")


getPScenarioImageR :: Text -> Int64 -> Text -> Handler TypedContent
getPScenarioImageR path width iOrPattern = do
  case readMay iOrPattern of
    Just i -> getScenarioImageByNumber path width i
    _      -> getScenarioImageByPattern path width iOrPattern
      


-- | Displays all images which contains boxes or given shelves
getPScenarioImageForR :: Text -> Text -> [Text] -> Handler Html
getPScenarioImageForR path forBox forShelf = do
  today <- todayH
  let ?today = today
      ?cache = memoryCache
  Right scenario <- readScenarioFromPath False importFamesDispatch $ unpack path
  case sLayout scenario of 
    Nothing -> error $ "No Layout provide"
    Just layout -> do
      today <- todayH
      let ?today = today
      wh0 <- execWithCache scenario
      let imgRoute w i = PlannerR $ PScenarioImageR path w (tshow i)
      contentPath <- contentPathM
      groupW <- liftIO $ readWarehouse (contentPath layout)
      is <- execWH wh0 do
                  groups <- groupW -- ShelfGroup groups _ <- groupW
                  -- find shelves to display
                  let shelfSelector = parseShelfSelector $ forBox <> "/" <> intercalate "/" forShelf
                      
                  goodShelves <- map shelfId <$> findShelvesByBoxNameAndNames shelfSelector
                  return [ i
                         | (i, group) <- zip [0..] $ toList groups
                         , let ids = concatMap toList group
                         , not $ null ( ids `intersect` goodShelves )
                         ]
      defaultLayout [whamlet|
         <table>
            $forall i <- is
              <tr><td><a href="@{imgRoute 4000 i}"><img src=@{imgRoute 750 i} style="width:800;">
      |]

  
                          

-- * Form 

paramForm :: Maybe FormParam -> Html -> MForm Handler (FormResult FormParam, Widget)
paramForm param extra = do
  plannerPathOptions <- lift getPlannerPathOptions
  (fPlannerPath, vPlannerPath) <- mopt (selectFieldList plannerPathOptions) "Planner File" (pPlannerPath <$> param)
  (fOrgfile, vOrgfile) <- mopt textareaField (bfs ("Scenario" :: Text)) (pOrgfile <$> param)
  (fTAM, vTAM) <- mopt textareaField (bfs ("Tag and Move" :: Text)) (pTAMSection <$> param)
  (fDel, vDel) <- mopt textareaField (bfs ("Delete" :: Text)) (pDeleteSection <$> param)
  -- (fDis, vDis) <- pure (pDisplayMode =<< param)
  -- (f)pure (pViewMode =<< param)
  (fReportParam, vReportParam) <- mopt textField (bfs ("report parameter" :: Text)) (pParameter <$> param)

  let form = FormParam
          <$> fPlannerPath
          <*> fOrgfile
          <*> fTAM
          <*> fDel
          <*> pure (pDisplayMode =<< param)
          <*> pure (pViewMode =<< param)
          <*> fReportParam
      groupId = "planner-param-group" :: Text
  let widget = [whamlet|
    #{extra}
    <div.form-inline>
     ^{renderField vPlannerPath}
     <span.data-toggler.collapsed data-toggle="collapse" data-target="##{groupId}"> More
    <div.row.collapse id=#{groupId}>
      ^{renderField vOrgfile}
      ^{renderField vTAM}
      ^{renderField vDel}
      ^{renderField vReportParam}
                       |]
  return (form, widget)

__getPlannerDirOptions :: Handler [(Text, FilePath)]
__getPlannerDirOptions = getSubdirOptions appPlannerDir

getPlannerPathOptions :: Handler [(Text, FilePath)]
getPlannerPathOptions = do
  dir <- appPlannerDir <$> getsYesod appSettings
  entries <- liftIO $ listDirectory dir
  -- only keep ".org" files
  return $ catMaybes [(,dir </> path) <$> stripSuffix ".org" (pack path)
                    | path <- entries
                    ]


-- * Rendering 
-- ** General 
plannerDoc :: Widget
plannerDoc = do
  let expanded = False
  let plannerDoc' = $(widgetFile "Planner/doc")
  [whamlet|
<h2>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-main"> Planner quick reference 
<div.pre.collapse id=info-section-main>
  ^{plannerDoc'}
|]

-- |  Concatenate all section of the section of the org file into one
fullOrgfile :: FormParam -> Maybe Text
fullOrgfile param = case catMaybes [orgfile, tam, del] of
  [] -> Nothing
  texts -> Just (unlines texts)
  where orgfile = unTextarea <$> pOrgfile param
        tam :: Maybe Text
        tam =  pTAMSection param <&> \t -> unlines
                    [ ":Tags and Moves:"
                    , "stock_id,tagsAndmoves"
                    , unTextarea t
                    , ":END:"
                    ]
        del =  pDeleteSection param <&> \t -> unlines
                    [ ":Delete:" :: Text
                    , unTextarea t
                    , ":END:"
                    ]
        
        
     
renderView :: FormParam -> Handler TypedContent
renderView param0 = do
  today <- todayH
  modeS <- lookupPostParam "mode"
  let mode = modeS >>=readMay
      vmode = pViewMode param0
      imageRoute = case (pPlannerPath param0, fullOrgfile param0) of
        (Just path, Nothing) -> \_ width i -> PScenarioImageR (pack path) width (tshow i)
        _ -> \sha -> PImageR sha
              
  scenarioFromFileEM <-  forM (pPlannerPath param0) (readScenarioFromPath False importFamesDispatch) 
  scenarioEM <- forM (fullOrgfile param0) (readScenario importFamesDispatch Nothing)
  Right extra <- readScenario importFamesDispatch Nothing "* Best Available shelves for"
  (param, widget) <- case liftA2 (,) (sequence scenarioFromFileEM) (sequence scenarioEM) of
      Left err -> setInfoToDoc >> setError (toHtml err) >> return (param0, "Invalid scenario")
      Right (scenarioFromFileM, scenarioM) ->  do
          -- param <- expandScenario (param0 {pDisplayMode = mode}) scenario
          -- we intersperce before the catMaybe so that there a savingPoint after  the last file even
          -- if there is no extra scenario
          let scenario = mconcat . catMaybes $ (intersperse (Just savePointScenario) $ (scenarioFromFileM : [scenarioM]))
          let param = param0 {pDisplayMode = mode}
              (boxSelectorM, tagFilter) = case pParameter param of
                  Nothing -> (Nothing, id)
                  Just selector -> let (selWithExclude, include) = breakOn "@include" selector
                                       (sel, exclude) = breakOn "@exclude" selWithExclude
                                       tagFilter = if 
                                                   | not (null include) ->  let includes = Set.fromList $ splitOn "#" include
                                                                          in Map.filterWithKey (\k _ -> k `member` includes)
                                                   | not (null exclude) ->  let excludes = Set.fromList $ splitOn "#" exclude
                                                        in Map.filterWithKey (\k _ -> not $ k `member` excludes)
                                                   | otherwise -> id
                                   in (Just $ parseBoxSelector sel, tagFilter)
              filterTagBox box = box {boxTags = tagFilter (boxTags box) }
          w <- case fromMaybe PlannerSummaryView (pViewMode param) of
              PlannerSummaryView-> renderSummaryReport scenario
              PlannerGraphicCompactView-> renderGraphicCompactView imageRoute scenario
              PlannerGraphicBigView-> renderGraphicBigView imageRoute scenario
              PlannerShelvesReport-> renderShelvesReport scenario
              PlannerShelvesGroupReport-> renderShelvesGroupReport scenario
              PlannerAllReport -> renderConsoleReport reportAll scenario
              PlannerBestBoxesFor -> renderConsoleReport (bestBoxesFor (fromMaybe "" (pParameter param))) scenario
              PlannerBestShelvesFor -> renderConsoleReport (bestShelvesFor (fromMaybe "" (pParameter param))) scenario
              PlannerBestAvailableShelvesFor -> let
                -- needed to use a different key to cache the warehouse
                -- as this report modify the warehouse. We don't want it to modify the cached one
                (styles, (_, pmodem,_,_)) = extractModes $ fromMaybe "" (pParameter param)
                pmode = fromMaybe PRightOnly pmodem
                in renderConsoleReport (bestAvailableShelvesFor pmode styles) (scenario `mappend` extra)

              PlannerGenerateMoves -> renderConsoleReport (generateMoves SortBoxes boxSelectorM boxStyle) scenario
              PlannerGenerateMovesWithTags -> renderConsoleReport (generateMoves DontSortBoxes boxSelectorM (boxStyleWithTags . filterTagBox)) scenario
              PlannerGenerateMOPLocations -> renderConsoleReport (generateMOPLocations boxSelectorM) scenario
              PlannerGenericReport -> renderConsoleReport (generateGenericReport today (fromMaybe "report" $ pParameter param)) scenario
              PlannerBoxGroupReport -> renderBoxGroupReport (pParameter param) scenario
              PlannerExport -> do
                fulltext <- scenarioToFullText scenario
                return [whamlet|#{toHtmlWithBreak fulltext}|]

              -- PlannerBoxGroupReport -> renderBoxGroupReport
              PlannerStocktake -> renderConsoleReport (generateStockTakes boxSelectorM) scenario
          return (param, w)
    
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  let navs = [PlannerSummaryView .. ]
      navClass nav = if vmode == Just nav then "active" else "" :: Html
      fay = $(fayFile "PlannerView") -- js to post form when tab change
      mainW = [whamlet|
<form #planner-view-form role=form method=post action="@{PlannerR (PViewR (pPlannerPath param) (pViewMode param))}" encType="#{encType}">
  <div.well>
    ^{formW}
    <button type="submit" .btn .btn-default name="mode" value="NormalM">Submit
  <ul.nav.nav-tabs>
    $forall nav <- navs
      <li class="#{navClass nav}">
        <a.view-mode href="#" data-url="@{PlannerR (PViewR (pPlannerPath param) (Just nav))}">#{splitSnake $ drop 7 (tshow nav)}
$maybe path <- pPlannerPath param
       <a href="@{PlannerR (PScenarioImageForR (pack path) "" [])}">Image for
<div#planner-view-view>
  ^{widget}
|]
    -- typed response needed for ajax
  selectRep $ do
    provideRep  $ do
      html <- defaultLayout (mainW >> fay)
      return (html :: Html)
    provideRep $ do -- Ajax. return table
      l_div <- widgetToPageContent widget
      html <- withUrlRenderer (pageBody l_div)
      returnJson (renderHtml html)

-- _unused_expandScenario :: FormParam -> Scenario -> Handler FormParam
-- _unused_expandScenario param scenario = do
--     text <- case pDisplayMode param of
--             Just CompactM -> return $ scenarioToTextWithHash scenario 
--             Just InitialM -> let s' = Scenario (Just (warehouseScenarioKey scenario))
--                                                []
--                                                (sLayout scenario)
--                              in return $ scenarioToTextWithHash s'
--             Just ExpandedM -> liftIO $ scenarioToFullText scenario
--             _ -> return $ unTextarea (pOrgfile param)
--     return $ param {pOrgfile = Textarea text} 

sendResponseDiag :: Int64 -> _ -> Handler TypedContent
sendResponseDiag width diag =  do
  let size = dims2D w (w/4)
      w = fromIntegral width
  M.renderContent (M.SizedDiagram size diag)

-- ** Graphical View 
renderGraphicCompactView :: (Text -> _) ->  Scenario -> Handler Widget
renderGraphicCompactView imageRoute scenario = do
  let ?cache = memoryCache
  (sha, layoutSize) <- cacheScenarioIn scenario
  let imgRoute i width = PlannerR (imageRoute sha i width)
      is = [0..fromIntegral (layoutSize-1)]
  return [whamlet|
<div>
  $forall i <- is
    <tr><td><a href="@{imgRoute 4000 i}"><img src=@{PlannerR $ PImageR sha 350 i} style="width:800;">
|]

renderGraphicBigView :: (Text-> _) -> Scenario -> Handler Widget
renderGraphicBigView imageRoute scenario = do
  let ?cache = memoryCache
  (sha, layoutSize) <- cacheScenarioIn scenario
  let imgRoute i width = PlannerR (imageRoute sha i width)
      is = [0..fromIntegral (layoutSize-1)]
  return [whamlet|
<table>
  $forall i <- is
    <tr><td><a href="@{imgRoute 4000 i}"><img src=@{PlannerR $ PImageR sha 750 i} style="width:800;">
|]

-- ** Summary report 
renderSummaryReport :: Scenario -> Handler Widget
renderSummaryReport scenario = do
  today <- todayH
  let ?today = today
      ?cache = memoryCache
  (header:rows, total) <- renderReport scenario summary
  
  return $ [whamlet|
<table.table.table-striped.table-hover>
  <tr>
    $forall h <- header
        <th> #{h}
  $forall row <- rows
    <tr>
      $forall col <- row 
        <td> #{col}
  <tr>
    <th> Total
    $forall col <- total 
      <th> #{col}
                 |]

  
renderShelvesReport :: Scenario -> Handler Widget
renderShelvesReport scenario = do
  today <- todayH
  let ?cache = memoryCache
      ?today = today
  (header':rows') <- renderReport scenario shelvesReport
  let header = splitOn "," header'
      rows = map (splitOn ",") rows'
  return [whamlet|
<table.table.table-striped.table-hover>
  <tr>
    $forall h <- header
        <th> #{h}
  $forall row <- rows
    <tr>
      $forall col <- row 
        <td> #{col}
                 |]

renderShelvesGroupReport :: Scenario -> Handler Widget
renderShelvesGroupReport = renderConsoleReport groupShelvesReport


renderBoxGroupReport :: Maybe Text ->  Scenario -> Handler Widget
renderBoxGroupReport selectorM = renderConsoleReport report where
  report = do
    boxes <- case selectorM of
      Nothing -> do
          boxIds <- toList <$> gets boxes
          mapM findBox boxIds
      Just pat -> findBoxByNameAndShelfNames (parseBoxSelector pat)
    groupBoxesReport boxes
  

renderConsoleReport ::  WH [Text] __RealWord -> Scenario -> Handler Widget
renderConsoleReport report scenario = do
  today <- todayH
  let ?cache = memoryCache
      ?today = today
  (rows) <- renderReport scenario report
  return [whamlet|
$forall row <- rows
  <samp>
    #{row}<br>
                 |]

-- | Show scenario in cache
__unused_renderHistory :: Handler Widget
__unused_renderHistory = do
  cvar <- getsYesod appCache
  cache <- toCacheMap <$> readMVar cvar
  info <- mapM (\km ->traverse readMVar km) (Map.toList cache)
  let sorted = sortBy (comparing $ Down . fst) info
      scenarios = [ (t,s)
                  | (__k, (d, t)) <- sorted
                  , let smm = Data.Dynamic.fromDynamic d :: Maybe (Maybe (Scenario, Int))
                  , Just (Just (s, _)) <- return $  smm
                  ]
                           

  return [whamlet|
<table.table.table-striped.table-hover>
  <tr>
    <th> Expiry time
    <th> Scenario
  $forall (t, scenario) <- scenarios
    <tr>
      <td> #{tshow t}
      <td> <samp>
        $forall l <- lines (scenarioToTextWithHash scenario)
          #{l}<br>
|]
