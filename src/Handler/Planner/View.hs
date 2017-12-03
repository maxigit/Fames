{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
)

where

import Data.List((!!))
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding(iso)
import Handler.Planner.Exec
import Import
import Planner.Internal
import Planner.Types
import Text.Blaze.Html.Renderer.Text(renderHtml)
import Util.Cache
import WarehousePlanner.Base
import WarehousePlanner.Report
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import qualified Yesod.Media.Simple as M
import Data.Text(strip, splitOn)

-- * Type
data ScenarioDisplayMode = NormalM | CompactM | InitialM | ExpandedM deriving (Eq, Show, Read)
data FormParam = FormParam
  { pOrgfile :: Textarea
  , pDisplayMode :: Maybe ScenarioDisplayMode -- ^ How to re-render the scenario in the textarea field
  , pViewMode :: Maybe PlannerViewMode -- ^ How to view the warehouse
  , pParameter :: Maybe Text
  } deriving (Show, Read)

defaultParam = FormParam (Textarea "") Nothing Nothing Nothing

-- * Handler
getPViewR :: Maybe PlannerViewMode -> Handler TypedContent
getPViewR viewMode = renderView defaultParam {pViewMode = viewMode}

postPViewR :: Maybe PlannerViewMode -> Handler TypedContent
postPViewR viewMode = do
  ((resp, _), _) <- runFormPost (paramForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderView param {pViewMode = viewMode <|> pViewMode param }

getPImageR :: Text -> Int64 -> Int64 -> Handler TypedContent
getPImageR sha i width = do
  scenarioM <- cacheScenarioOut sha
  traceShowM ("IMAGE", scenarioM)
  case scenarioM of
    Nothing -> do
      error "No matching scenario"
    Just (scenario, layoutSize) -> do
        diagE <- renderScenario scenario Nothing
        case diagE of
          Left e -> error e
          Right diags -> cache0 False (cacheHour 1) ("DIAG", width, i, sha) $
                                sendResponseDiag width (diags !! min (fromIntegral i) (layoutSize-1))
  
-- * Form

paramForm :: Maybe FormParam -> _
paramForm param = renderBootstrap3 BootstrapBasicForm form
  where form = FormParam
          <$> areq textareaField "Scenario" (pOrgfile <$> param)
          <*> pure (pDisplayMode =<< param)
          <*> pure (pViewMode =<< param)
          <*> aopt textField "report parameter" (pParameter <$> param)

-- * Rendering
-- ** General
renderView :: FormParam -> Handler TypedContent
renderView param0 = do
  modeS <- lookupPostParam "mode"
  let mode = modeS >>=readMay
      vmode = pViewMode param0
  scenarioE <- readScenario (unTextarea $ pOrgfile param0)
  (param, widget) <- case scenarioE of
      Left err -> setError (toHtml err) >> return (param0, "Invalid scenario")
      Right scenario ->  do
          param <- expandScenario (param0 {pDisplayMode = mode}) scenario
          w <- case fromMaybe PlannerSummaryView (pViewMode param) of
              PlannerSummaryView-> renderSummaryReport scenario
              PlannerGraphicCompactView-> renderGraphicCompactView scenario
              PlannerGraphicBigView-> renderGraphicBigView scenario
              PlannerShelvesReport-> renderShelvesReport scenario
              PlannerShelvesGroupReport-> renderShelvesGroupReport scenario
              PlannerAllReport -> renderConsoleReport reportAll scenario
              PlannerBestBoxesFor -> renderConsoleReport (bestBoxesFor (unpack $ fromMaybe "" (pParameter param))) scenario
              PlannerBestShelvesFor -> renderConsoleReport (bestShelvesFor (unpack $ fromMaybe "" (pParameter param))) scenario
              PlannerBestAvailableShelvesFor -> renderConsoleReport (bestAvailableShelvesFor (unpack $ fromMaybe "" (pParameter param))) scenario
              PlannerGenerateMoves -> renderConsoleReport generateMoves scenario
          return (param, w)
    
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  let navs = [PlannerSummaryView .. ]
      navClass nav = if vmode == Just nav then "active" else "" :: Html
      fay = $(fayFile "PlannerView") -- js to post form when tab change
      mainW = [whamlet|
<form #planner-view-form role=form method=post action="@{PlannerR (PViewR (pViewMode param))}" encType="#{encType}">
  ^{formW}
  <button type="submit" .btn .btn-default name="mode" value="NormalM">Submit
  <button type="submit" .btn .btn-default name="mode" value="CompactM">Compact
  <button type="submit" .btn .btn-danger name="mode" value="InitialM">Save
  <button type="submit" .btn .btn-primary name="mode" value="ExpandedM">Expand
  <ul.nav.nav-tabs>
    $forall nav <- navs
      <li class="#{navClass nav}">
        <a.view-mode href="#" data-url="@{PlannerR (PViewR (Just nav))}">#{drop 7 (tshow nav)}
<div#planner-view-view>
  ^{widget}
|]
    -- typed response needed for ajax
  selectRep $ do
    provideRep  $ do
      html <- defaultLayout (mainW >> fay)
      return (html :: Html)
    provideRep $ do -- Ajax. return table
      div <- widgetToPageContent widget
      html <- withUrlRenderer (pageBody div)
      returnJson (renderHtml html)

expandScenario :: FormParam -> Scenario -> Handler FormParam
expandScenario param scenario = do
    text <- case pDisplayMode param of
            Just CompactM -> return $ scenarioToTextWithHash scenario 
            Just InitialM -> let s' = Scenario (Just (warehouseScenarioKey scenario))
                                               []
                                               (sLayout scenario)
                             in return $ scenarioToTextWithHash s'
            Just ExpandedM -> liftIO $ scenarioToFullText scenario
            _ -> return $ unTextarea (pOrgfile param)
    return $ param {pOrgfile = Textarea text} 

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
  
  return [whamlet|
<table.table.table-striged.table-hover>
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
  let header = splitOn "," (pack header')
      rows = map (splitOn "," . pack) rows'
  return [whamlet|
<table.table.table-striged.table-hover>
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

-- renderConsoleReport :: Scenario )Scenario -> Handler Widget
renderConsoleReport report scenario = do
  (rows) <- renderReport scenario report
  return [whamlet|
$forall row <- rows
  <samp>
    <p> #{row}
                 |]
