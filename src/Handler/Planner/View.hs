module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
, getPDocR
)

where

import Data.List((!!))
import Data.Dynamic
import Diagrams.Prelude hiding(iso, width, size)
import Handler.Planner.Exec
import Handler.Planner.FamesImport(importFamesDispatch)
import Import
import Planner.Internal
import Planner.Types
import Text.Blaze.Html.Renderer.Text(renderHtml)
import Util.Cache
import WarehousePlanner.Base
import WarehousePlanner.Report hiding(report)
import Yesod.Form.Bootstrap3 (bfs)
import qualified Yesod.Media.Simple as M
import Data.Text(splitOn)
import qualified Data.Map as Map
import Control.Monad.State
import System.Directory (listDirectory)

-- * Type
data ScenarioDisplayMode = NormalM | CompactM | InitialM | ExpandedM deriving (Eq, Show, Read)
data FormParam = FormParam
  { pPlannerPath :: Maybe FilePath -- ^ planner file to load
  , pOrgfile :: Maybe Textarea -- ^ text to add to the content of pPlannerPath
  , pTAMSection :: Maybe Textarea -- ^ Tags and Moves section, don't need header and drawer decoration
  , pDeleteSection :: Maybe Textarea-- ^ Delete section, don't need header and drawer decoration
  , pDisplayMode :: Maybe ScenarioDisplayMode -- ^ How to re-render the scenario in the textarea field
  , pViewMode :: Maybe PlannerViewMode -- ^ How to view the warehouse
  , pParameter :: Maybe Text
  } deriving (Show, Read)

defaultParam :: FormParam
defaultParam = FormParam Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- * Handler
setInfoToDoc :: Handler ()
setInfoToDoc = do
  doc <- widgetToPageContent plannerDoc
  html <- withUrlRenderer (pageBody doc)
  setInfo $ html
  
{-# NOINLINE getPViewR #-}
getPViewR :: Maybe PlannerViewMode -> Handler TypedContent
getPViewR viewMode = do
  setInfoToDoc
  renderView defaultParam {pViewMode = viewMode}

{-# NOINLINE postPViewR #-}
postPViewR :: Maybe PlannerViewMode -> Handler TypedContent
postPViewR viewMode = do
  ((resp, _), _) <- runFormPost $ paramForm Nothing
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderView param {pViewMode = viewMode <|> pViewMode param }

{-# NOINLINE getPImageR #-}
getPImageR :: Text -> Int64 -> Int64 -> Handler TypedContent
getPImageR sha i width = do
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
  scenarioFromFileEM <-  forM (pPlannerPath param0) (readScenarioFromPath importFamesDispatch) 
  scenarioEM <- forM (fullOrgfile param0) (readScenario importFamesDispatch)
  Right extra <- readScenario importFamesDispatch "* Best Available shelves for"
  (param, widget) <- case liftA2 (,) (sequence scenarioFromFileEM) (sequence scenarioEM) of
      Left err -> setInfoToDoc >> setError (toHtml err) >> return (param0, "Invalid scenario")
      Right (scenarioFromFileM, scenarioM) ->  do
          -- param <- expandScenario (param0 {pDisplayMode = mode}) scenario
          -- we intersperce before the catMaybe so that there a savingPoint after  the last file even
          -- if there is no extra scenario
          let scenario = mconcat . catMaybes $ (intersperse (Just savePointScenario) $ (scenarioFromFileM : [scenarioM]))
          let param = param0 {pDisplayMode = mode}
          w <- case fromMaybe PlannerSummaryView (pViewMode param) of
              PlannerSummaryView-> renderSummaryReport scenario
              PlannerGraphicCompactView-> renderGraphicCompactView scenario
              PlannerGraphicBigView-> renderGraphicBigView scenario
              PlannerShelvesReport-> renderShelvesReport scenario
              PlannerShelvesGroupReport-> renderShelvesGroupReport scenario
              PlannerAllReport -> renderConsoleReport reportAll scenario
              PlannerBestBoxesFor -> renderConsoleReport (bestBoxesFor (fromMaybe "" (pParameter param))) scenario
              PlannerBestShelvesFor -> renderConsoleReport (bestShelvesFor (fromMaybe "" (pParameter param))) scenario
              PlannerBestAvailableShelvesFor -> let
                -- needed to use a different key to cache the warehouse
                -- as this report modify the warehouse. We don't want it to modify the cached one
                in renderConsoleReport (bestAvailableShelvesFor (fromMaybe "" (pParameter param))) (scenario `mappend` extra)

              PlannerGenerateMoves -> renderConsoleReport (generateMoves boxStyle) scenario
              PlannerGenerateMovesWithTags -> renderConsoleReport (generateMoves boxStyleWithTags) scenario
              PlannerGenerateMOPLocations -> renderConsoleReport (generateMOPLocations) scenario
              PlannerGenericReport -> renderConsoleReport (generateGenericReport today (fromMaybe "report" $ pParameter param)) scenario
              PlannerBoxGroupReport -> renderBoxGroupReport (pParameter param) scenario
              PlannerExport -> do
                fulltext <- scenarioToFullText scenario
                return [whamlet|#{toHtmlWithBreak fulltext}|]

              -- PlannerBoxGroupReport -> renderBoxGroupReport
          return (param, w)
    
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  let navs = [PlannerSummaryView .. ]
      navClass nav = if vmode == Just nav then "active" else "" :: Html
      fay = $(fayFile "PlannerView") -- js to post form when tab change
      mainW = [whamlet|
<form #planner-view-form role=form method=post action="@{PlannerR (PViewR (pViewMode param))}" encType="#{encType}">
  <div.well>
    ^{formW}
    <button type="submit" .btn .btn-default name="mode" value="NormalM">Submit
  <ul.nav.nav-tabs>
    $forall nav <- navs
      <li class="#{navClass nav}">
        <a.view-mode href="#" data-url="@{PlannerR (PViewR (Just nav))}">#{splitSnake $ drop 7 (tshow nav)}
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
renderGraphicCompactView :: Scenario -> Handler Widget
renderGraphicCompactView scenario = do
  (sha, layoutSize) <- cacheScenarioIn scenario
  let imgRoute i width = PlannerR (PImageR (sha) i width)
      is = [0..fromIntegral (layoutSize-1)]
  return [whamlet|
<div>
  $forall i <- is
    <tr><td><a href="@{imgRoute i 8000}" ><img src=@{imgRoute i 350} style="width:800;">
|]

renderGraphicBigView :: Scenario -> Handler Widget
renderGraphicBigView scenario = do
  (sha, layoutSize) <- cacheScenarioIn scenario
  let imgRoute i width = PlannerR (PImageR (sha) i width)
      is = [0..fromIntegral (layoutSize-1)]
  return [whamlet|
<table>
  $forall i <- is
    <tr><td><a href="@{imgRoute i 8000}" ><img src=@{imgRoute i 750} style="width:800;">
|]

-- ** Summary report
renderSummaryReport :: Scenario -> Handler Widget
renderSummaryReport scenario = do
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
    boxIds <- case selectorM of
      Nothing -> toList <$> gets boxes
      Just pat -> findBoxByNameAndShelfNames (parseBoxSelector pat)
    boxes <- mapM findBox boxIds
    groupBoxesReport boxes
  

renderConsoleReport ::  WH [Text] __RealWord -> Scenario -> Handler Widget
renderConsoleReport report scenario = do
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
