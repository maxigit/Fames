{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
)

where

import Data.List((!!))
import Data.Dynamic
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
import qualified Data.Map as Map
import Handler.Util

-- * Type
data ScenarioDisplayMode = NormalM | CompactM | InitialM | ExpandedM deriving (Eq, Show, Read)
data FormParam = FormParam
  { pPlannerDir :: Maybe FilePath -- ^ directory containings planner files
  , pOrgfile :: Maybe Textarea -- ^ text to add to the content of pPlannerDir
  , pDisplayMode :: Maybe ScenarioDisplayMode -- ^ How to re-render the scenario in the textarea field
  , pViewMode :: Maybe PlannerViewMode -- ^ How to view the warehouse
  , pParameter :: Maybe Text
  } deriving (Show, Read)

defaultParam = FormParam Nothing Nothing Nothing Nothing Nothing

-- * Handler
getPViewR :: Maybe PlannerViewMode -> Handler TypedContent
getPViewR viewMode = do
  setInfo plannerDoc
  renderView defaultParam {pViewMode = viewMode}

postPViewR :: Maybe PlannerViewMode -> Handler TypedContent
postPViewR viewMode = do
  ((resp, _), _) <- runFormPost =<< paramForm Nothing
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

paramForm :: Maybe FormParam -> Handler _
paramForm param = do
  plannerDirOptions <- getPlannerDirOptions
  let form = FormParam
          <$> aopt (selectFieldList plannerDirOptions) "Planner Directory" (pPlannerDir <$> param)
          <*> aopt textareaField "Scenario" (pOrgfile <$> param)
          <*> pure (pDisplayMode =<< param)
          <*> pure (pViewMode =<< param)
          <*> aopt textField "report parameter" (pParameter <$> param)
  return $ renderBootstrap3 BootstrapBasicForm form

getPlannerDirOptions :: Handler [(Text, FilePath)]
getPlannerDirOptions = getSubdirOptions appPlannerDir


-- * Rendering
-- ** General
plannerDoc = [shamlet|
<h2>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-main"> Planner quick reference 
<div.pre.collapse id=info-section-main>
  #{plannerDoc'}
|]
plannerDoc' = [shamlet|
$newline text
Displays a Planner (a visual view of the warehouse with its content).
The planner is a set of files compatible with Emacs org-mode.
Files are read from the given subdirectory (if any) and/or read from the text area.
A planner file is made of different sections. Each section correspond to an org-mode heading (beginning with one more <code>*</code>).
If the first word of a section is one of the valid section name, the whole content of the heading will be treated processed according to the section type.
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-sections">Sections
<div.pre.collapse id=info-section-sections>
    <ul> 
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-shelves"> Shelves (Mandatory)
        <div.pre.collapse id=info-section-shelves>
            Describes set of shelves. Should be a csv with the following header :
            <pre>name,comment,length,width,height,type
            Please note that there is always an error shelf named <code>error</code>.
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-layout"> Layout (Mandatory)
        <div.pre.collapse id=info-section-layout>
          Describes how shelves should be displayed.
          Shelves are displayed as a matrix (row and column) of bays. A bay being a set of shelves stacked together.
          Each line of the layout section describe a row.
          Columns are separated by one or more space and each element of a bay by a pipe <code>|</code>

          Example: 
            <pre>
                A1|A2|A3 B1|B2
                C D E
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-stocktake">  Stocktake
        <div.pre.collapse id=info-section-stocktake>
          Describes a set of boxes with their location and eventually orientation. It is a csv with the following header
          <pre>Bay No,Style,QTY,Length,Width,Height,Orientations
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-boxes"> Boxes
        <div.pre.collapse id=info-section-boxes>
          A set of boxes without initial location. They will be put in the <code>pending</code> shelf.
          It is a csv with the following header :
          <pre>style,quantity,l,w,h
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-moves"> Moves
        <div.pre.collapse id=info-section-moves>
          Describes a set of moves boxes to shelves. The first column describe a set of boxes to moves to
          a set of shelves. If multiple shelves are given, the Planner will fill the shelf in the given order
          and use the optimal orientation. If all boxes can't fit the given shelves, the excendatary boxes will
          be moved to error.
          It is csv with the following header:
          <pre>stock_id,location
          Please not the stock_id and location are in fact boxes and shelves selecto
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-tags"> Tags
        <div.pre.collapse id=info-section-tags>
          Allows to set a tag to a selection of boxes. Tags can be used to display boxes differently (via a colour)
          or select them (when moving or even tagging boxes).

          Boxes tagged with a colour name will be displayed with this colours. If a box is tagged with different colours, the result will be
          the mix of the colours. As a tag can only be set once per box, to change the weight of a colour by tagging it more than once, an underscore (or more) can be added before a colour name.
          This way, tags will different (example #black and #_black), but will be seen twice as the black colour, but will be seen twice as the black colour.

          Example <code>black#_black#white</code> will display a box in dark gray (black is counted twice)
          A Tag can be removed by setting with <code>-</code>

      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-orientations"> Orientations
        <div.pre.collapse id=info-section-orientations>
          Specifies the boxes configuration within a shelves (if they are stacked up, on the side, how many etc).
          Boxes of a given style can be given different configuration for different shelves by specifing the shelf
          in the box selector. This is a csv with the following header:
          <code>stock_id,orientation

          Example:
          <pre>
            TSHIRT/#top,^
            TSHIRT,=

          All T-shirt on top shelves (with the tag <code>top</code>) are up, whereas T-shirt in other shelves
          are being laid on the side.



    <div>
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-selectors">
    Boxes and shelves selector
<div.pre.collapse id=info-section-selectors>
  Most operations, (moving and tagging) operates on boxes. To do so, a set of boxes is selected and then the operation is applied.
  The main way of selecting a box is by specifying a style, which will select all boxes of the given style. However, more complex selection
  can be selecting boxes by tags and locations (as well as loction tag). The format of a box selector is the following

  <pre>
    style pattern <b>#</b> tag <b>/</b>/ location pattern <b>#</b> location tag

  Example
  <pre>
     TShirt => all boxes with the style 'TShirt'
     #white  =>  all boxes with the 'white' tag
     TShirt#white => all boxes with the style 'TShirt' AND the white tag
     /A => All boxes in the A Shelves
     #white/A* => All boxes in a shelves with the name starting with 'A' shelves AND The white tag
     TShirt/#top => ALL Tshirt on the shelf with the 'top' tag.
  <ul>
    <li>
      <h4> Style pattern
    <li>
      <h4> tags
      Starts should start and be separated with <code>#</code>). Only boxes with ALL tags will be selected.
      Preceding a tag with <code>-</code> means boxes wich don't have this tag.

      Example
      <pre>
        TShirt#red#-XL => all red tshirt , except the XL Ones.
    <li>
      <h4> Location pattern
    <li>
      <h4> Location tag
    <li>
      <h4> number restriction
      The <code>^</code> symbol is used to select only a certain number of boxes per variation/content, per shelf, and in total
      <pre>
         content ^ shelf ^ total ^

      Example
      <pre>
         TShirt^^^1 => the first box containing a t-shirt.
         ^1 => The first box of each variations (colour)
         ^2^3 =>  The two first boxes of each variations, with a maximum of 3 per shelves.
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-virtual-tags">
     Virtual tags
<div.pre.collapse id=info-section-virtual-tags>
  Virtual tags are tags which are not set by the user, but automatically set depending the box content and/or the box itself. It allows
  to select style by variations (<code>'</code>content) and dimensions (in 1/10th of mm) (<code>'l</code>length, <code>'w</code>width,<code>'h</code>height ).

  Example
  <pre>
      TShirt#'3500 => all T-shirt with boxes with a length of 35cm.
      TShirt#'RED  => all red T-shirt (with a content of Red)
|]

renderView :: FormParam -> Handler TypedContent
renderView param0 = do
  modeS <- lookupPostParam "mode"
  let mode = modeS >>=readMay
      vmode = pViewMode param0
  plannerDirContent <-  forM (pPlannerDir param0) readScenarioFromDir 
  scenarioE <- readScenario (concat $ catMaybes [plannerDirContent, unTextarea <$> pOrgfile param0])
  (param, widget) <- case scenarioE of
      Left err -> setInfo plannerDoc >> setError (toHtml err) >> return (param0, "Invalid scenario")
      Right scenario ->  do
          -- param <- expandScenario (param0 {pDisplayMode = mode}) scenario
          let param = param0 {pDisplayMode = mode}
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
              PlannerScenarioHistory -> renderHistory
          return (param, w)
    
  (formW, encType) <- generateFormPost =<< paramForm (Just param)
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

-- | Show scenario in cache
renderHistory :: Handler Widget
renderHistory = do
  cvar <- getsYesod appCache
  cache <- readMVar cvar
  info <- mapM (\km ->traverse readMVar km) (Map.toList cache)
  let sorted = sortBy (comparing $ Down . fst) info
      scenarios = [ (t,s)
                  | (k, (d, t)) <- sorted
                  , let smm = Data.Dynamic.fromDynamic d :: Maybe (Maybe (Scenario, Int))
                  , Just (Just (s, _)) <- return $ traceShow (k, smm) smm
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

