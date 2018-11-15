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
import Control.Monad.State

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
  -- traceShowM ("IMAGE", scenarioM)
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
            <h5> Special shelves
            The two first declared shelves have a special meaning.
            The first one is the <code>error</code> shelf and the second one is the <code>pending</code> shelf.
            The error shelf is used when ever a move try to fit more boxses than possible in the given shelf.
            All the selected boxes which doesn't fit the given location are moved to the error shelf.
            The <code>pending</code> shelf is used when creating new boxes. Boxes created using the Boxes command are
            created within the <code>pending</code> shelf.
            It is best practice to name those shelves <code>error</code> and <code>pending</code> (or equivalent).

            <h5> Name expansion
            Groups of similar shelves can be created in one line using name expansion. Before creating a shelf,
            the name is expanded into one or many shelf names. One shelf will be created of each of the resulting name.
            Shelf name are split with the <code>|</code> separator, then when encountering <code>[..]</code>, name will be expanded by generating a name for each character within the bracket.
            Example
            <pre>
              A|B => A B
              A[01] => A0 A1
              A[ABx]X => AAX ABX AxX

            If diferent ranger are given, all combination will generated
            Example
            <pre>
              [ABC][123] => A1 A2 A3 B1 B2 B3 C1 C2 C3
            <h5> dimension formula 
            Shelf dimension depending on the dimension on another shelf can be expressed using shelf dimension.
            This can be useful when shelf are back to back or one shelf is not physically a shelf but the space
            leftover between a shelf and wall. Basic arithmetic can be performed and the the same dimension of another
            shelf can referenced using <code>{...}</code>. Wildcard (<code>%</code>,<code>_</code>,<code>+</code> and <code>-</code>) can be used to modify the name of the current shelf itsef.
            Example
            <pre>
              name,comment,length,width,height,type
              A1,,150,40,200,
              A2,,100,{_-},20, -- same width as A1 
              B1,,200-{A_},-- length 200 - length of A1
              B2,,200-{A_},-- length 200 - length of A2
            <h5>shelf types
            The shelf type determines the default boxes orientation and filling strategy. The current shelf style are
            <ul>
              <li> Shelf (normal first) : tilted row first
              <li> deadzone : allow up column first
              <li> other, column first
            <h5> Tag
            On the contrary of boxes, shelves can only have one tag (or none).
            The tag can be used to select shelf when doing box moves, but is also used to
            group shelves when displaying the summary. Shelves with a tag starting with a <code>_</code>
            are considered as virtual shelves and are not taken in to account when calculated used spaces
            and floor space. Also, shelves with the <code>sep</code> are seen as separator : shelves
            present for layout purpose only and are excluded from the summary as well as being displayed
            differently.
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
          Please not, that boxes created this way will be tagged with the <code>new</code> tag.
          It is the user responsibilities to remove this tag when needed.
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-moves"> Moves
        <div.pre.collapse id=info-section-moves>
          Describes a set of moves boxes to shelves. The first column describe a set of boxes to moves to
          a set of shelves. If multiple shelves are given, the Planner will fill the shelf in the given order
          and use the optimal orientation. If all boxes can't fit the given shelves, the excedendatary boxes will
          be moved to the error shelf.
          It is csv with the following header:
          <pre>stock_id,location
          Please not the stock_id and location are in fact boxes and shelves selectors

          <h5> Filling order, Exit on top
          When moves boxes to a new set of shelves, shelves are filled by alphabetical order. For example
          the command
          <pre>
          :Moves:
          stock_id,location
          ,A|B|C
          Will move all boxes to the  shelves A, B and C starting by filling A, the filling B and so on.
          Boxes are stacked in column form left to right.
          It is however sometimes desirable to carry on filling the same column on the next shelf
          rather than creating a new column on the current shelf. This can be achieved by specifying
          the "exit on top" option by starting the location with <code>^</code>
          <pre>
          :Moves:
          stock_id,location
          ,^A|B|C

          The code above, will fill the first colum into shelf A, then a column in B and then C.
          When the first column in C is full, it will start a 2nd column in A, then B  etc ...
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
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-movesandtags"> MAT (moves and tags)
        <div.pre.collapse id=info-section-movesandtags>
          Allows to move and tag at the same time a set of boxes. This can be faster and less verbose than creating a move and
          a tag section. Tags needs to start with a <code>#</code> and location CAN start with <code>/</code>
          Example
          <pre>
            :MAT:
            stock_id,location#tag
            #new,A#-new
          Moves all new boxes (with the new tag) to A and unset the new tag.
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-transformtags"> Transform (transform tags)
        <div.pre.collapse id=info-section-transformtags>
          Allow to use POSIX regular expression to subsitute existing tags into new ones.
          Each tag of the selecting boxes are matched against the pattern. A set of new tags is generated
          by substituing the pattern with the substitution string  which is then splitted using <code>#</code>.
          Other tags can be removed by generating a <i>negative</i> tag (using <code>-</code>).
          The original tag is not deleted but can be done using <code>-\0</code>.
          It is a csv with the following header
          <pre>
             stock_id,pat(tern),sub(stitue)

          Examples
          <pre>
             A,black,blue --> add the blue tag to each box of type A
             ,black,blue#-black --> replace black by blue
             ,black,blue#-\0 --> replace black by blue. (remove black)
             ,^[[:upper]],-\0 --> remove all tags starting with an uppercase
          Group (using `(..)`) can be use to extract substring
          <pre>
             ,(..)-(..),\2:\1 --> add BB:AA from the tag AA-BB
          Properties and virtual tags are expanded in the regexp itself.
          Example
          <pre>
            ,location=$shelfname,unmoved -- detect boxes which haven't changed
      <li>
        <h4>
          <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-orientations"> Orientations
        <div.pre.collapse id=info-section-orientations>
          Specifies the boxes configuration within a shelves (if they are stacked up, on the side, how many etc).
          Boxes of a given style can be given different configuration for different shelves by specifing the shelf
          in the box selector. This is a csv with the following header:
          <code>stock_id,orientation

          Orientation must have the following format <code>min-depth : max-depth  orientations</code>
          Example:
          <pre>
            TSHIRT/#top,^
            TSHIRT,=

          All T-shirt on top shelves (with the tag <code>top</code>) are up, whereas T-shirt in other shelves
          are being laid on the side.
          <h5> Orientations
          <pre>
            * all 
            % default orientations
            ^ up
            = tilted forward
            > tilted right
            | tilted forward & right
            ' rotated up
            @ rotated side
          <h5> Depth specification
          By default, boxes are stacked using only one level of depth. This way, no boxes hide behind others and so all boxes are visible.
          To enable the use of multiple depth and allow boxes to hide each other, a minimum and max depth can set (before)
          <pre>
            ,1:4 -- allow up to 4 depth level
            ,1: -- use a mininum of 2
            ,4 -- similar to 1:4
            ,4^ -- up to 4 levels, stacking boxes up
    <div>
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-selectors">
    Boxes and shelves selector
<div.pre.collapse id=info-section-selectors>
  Most operations, (moving and tagging) operates on boxes. To do so, a set of boxes is selected and then the operation is applied.
  The main way of selecting a box is by specifying a style, which will select all boxes of the given style. However, more complex selection
  can be selecting boxes by tags and locations (as well as location tag). The format of a box selector is the following

  <pre>
    style pattern <b>#</b> tag(s) pattern <b>/</b>/ location pattern <b>#</b> location tag

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
      <h4> Glob pattern
      When selecting boxes or shelves, wildcard can be used to match the name and the tags of the box or the shelf using
      <a href="https://en.wikipedia.org/wiki/Glob_(programming)">UNIX glob pattern</a>.
      The <code>|</code> operator can also be used to give a list of alternatives.
      Example, given the location
      <ul>
        <li> A01
        <li> A02
        <li> A11
        <li> A12
        <li> B1
        <li> B2
        <li> B3
      <pre>
        A* => <b>A0</b>1 <b>A0</b>2 A03 A11 A12 A3
        ?? => B1 B2 B3 -- two characters only
        B[12] => B1 B2 -- 3 is not in the range 1-2
        A?1|B2 => A01, A11 , B2

      Please note that the syntax used to create group of shelves is only a subset of the glob pattern syntax.
      Even though pattern globing can only create disjunction (OR), conjunction (AND) can be achieve using tags.
      Example
      <pre>
        %RED|%XL => select things which are red OR extra large..
        %RED,red => tag all RED thing
        %XL,xl => tag all XL things
        #red#xl => select thing which are red AND extra large.
        
    <li>
      <h4> style pattern
      A glob pattern on the name of the box.
    <li>
      <h4> tags
      Starts should start and be separated with <code>#</code>). Only boxes with ALL tags will be selected.
      Preceding a tag with <code>-</code> means boxes wich don't have this tag.

      Example
      <pre>
        TShirt#red#-XL => all red tshirt , except the XL Ones.
    <li>
      <h4> Location pattern
      A glob pattern on the name of the shelf.
    <li>
      <h4> Location tag
      A glob pattern on the tag to select a shelf.
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
    <li>
      <h4> Priority and selection order
      Boxes are selected in semi-arbitrary order which can be modified setting up priority.
      The order in which box are selected affect the way boxes are actually stacked on shelves
      but also which boxese are selected when using number restriction (see above).
      By default boxes are selected in order by
      <ul>
        <li> global priority
        <li> style name  (ascending)
        <li> style priority (priority within style)
        <li> content name
        <li> content priority (priority within content)
      By default, all priorities  are set 100. Priorities can be modified by assigning a special tag
      <pre>
         content @ style @ global
      
      For example, given fox boxes, A-Black, A-Red, B-Black, B-Red. Boxes will be stacked in the following order
      <ul>
        <li> B-Black
        <li> B-Red
        <li> A-Black
        <li> A-Red
      or 
      <ul>
        <li> A-Black
        <li> A-Red
        <li> B-Black
        <li> B-Red
      
      A and B having the same global priority, the system is free to start with A or B. However, content (Black and Red) are sorted alphabetically.
      To ensure that, A is processed before B. We need to assign it a priority < 100 to A (global priority) with
      <pre>
         A,@@1
      
      To get B-Red boxes before B-Black boxes we can assign it a priority (style priority)
      <pre>
       B-Red,@1

      Settings those two priorities will result in the following order :
      <ul>
        <li> A-Black # @100@1
        <li> A-Red   # @100@1
        <li> B-Red   # @1@100
        <li> B-Black # @100@100

      The content priority could be used for example, to select which one of the B-Black boxes to get first.
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-virtual-tags">
     Virtual tags
<div.pre.collapse id=info-section-virtual-tags>
  Virtual tags are tags which are not set by the user, but automatically set depending the box content and/or the box itself. It allows
  to select style by variations (<code>'</code>content) and dimensions (in 1/10th of mm) (<code>'l</code>length, <code>'w</code>width,<code>'h</code>height ).

  Example
  <pre>
      TShirt#'l3500 => all T-shirt with boxes with a length of 35cm.
      TShirt#'RED  => all red T-shirt (with a content of Red)
  Virtual tags can also be used to change the dimension of a box. Setting for example the tag <code>'h3400</code> will change the box size to 34 cm.
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-property">
     Properties
<div.pre.collapse id=info-section-box-attributes>
  Tag with the shape <code>propery=value</code> are properties. Their value can be expanded to create tag. 
  Example
  <pre>
    A,prop=first --> A boxes have the tag 'prop=first'
    B,prop=second
    ,XXX[prop] -- create XXXfirst tag for A and XXXsecond for B
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-box-attributes">
     Box attributes
<div.pre.collapse id=info-section-box-attributes>
  Certain attributes like the current location or orientation of a box can be used to set a new tag
  with the corresponding value. The following attributes are available.
  <ul>
    <li> $shelfname # current shelf
    <li> $shelftag # tag of the current shelf
    <li> $orientation # current orientation

  Example
  <pre>
    /pending,loc=$shelfname  => All boxes in the pending location will be tagged with "loc=pending".
<h3>
  <span.data-toggler.collapsed data-toggle=collapse data-target="#info-section-mop-export">
     Mop export
<div.pre.collapse id=info-section-mop-export>
  The location of all the boxes can be exported to MOP (via the generateMOPLocation).
  By default, locations are given for each style (regardless of the content) in the form of the pattern matching all used shelves.
  The location of a particular variation (style + content) can exported separately using the tag <code>mop-exception</code>.
  Boxes can also be excluded using the tag <code>mop-exclude</code>. This is particularey usefull to exclude shelves which shoudn't be taken into account.
  Example, to exclude all shelves starting with <code>_</code> (shelves filtered from the summary report)
  <pre>
    :TAGS:
    stock_id,tag
    /#_*,mop-exclude
    :END:
  Arbitrary comments can also be added to a box using the tag <code>mop-comment=<i>your comment</i></code>.
|]

renderView :: FormParam -> Handler TypedContent
renderView param0 = do
  modeS <- lookupPostParam "mode"
  let mode = modeS >>=readMay
      vmode = pViewMode param0
  scenariosEM <-  forM (pPlannerDir param0) readScenariosFromDir 
  scenarioEM <- forM (pOrgfile param0) (readScenario . unTextarea)
  Right extra <- readScenario "* Best Available shelves for"
  (param, widget) <- case liftA2 (,) (sequence scenariosEM) (sequence scenarioEM) of
      Left err -> setInfo plannerDoc >> setError (toHtml err) >> return (param0, "Invalid scenario")
      Right (scenariosM, scenarioM) ->  do
          -- param <- expandScenario (param0 {pDisplayMode = mode}) scenario
          -- we intersperce before the catMaybe so that there a savingPoint after  the last file even
          -- if there is no extra scenario
          let scenario = mconcat . catMaybes $ (intersperse (Just savePointScenario) $ (sequence scenariosM <> [scenarioM]))
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
              PlannerBestAvailableShelvesFor -> let
                -- needed to use a different key to cache the warehouse
                -- as this report modify the warehouse. We don't want it to modify the cached one
                in renderConsoleReport (bestAvailableShelvesFor (unpack $ fromMaybe "" (pParameter param))) (scenario `mappend` extra)

              PlannerGenerateMoves -> renderConsoleReport (generateMoves boxStyle) scenario
              PlannerGenerateMovesWithTags -> renderConsoleReport (generateMoves boxStyleWithTags) scenario
              PlannerGenerateMOPLocations -> renderConsoleReport (generateMOPLocations) scenario
              PlannerScenarioHistory -> renderHistory
              PlannerBoxGroupReport -> renderBoxGroupReport (pParameter param) scenario
              -- PlannerBoxGroupReport -> renderBoxGroupReport
          return (param, w)
    
  (formW, encType) <- generateFormPost =<< paramForm (Just param)
  let navs = [PlannerSummaryView .. ]
      navClass nav = if vmode == Just nav then "active" else "" :: Html
      fay = $(fayFile "PlannerView") -- js to post form when tab change
      mainW = [whamlet|
<form #planner-view-form role=form method=post action="@{PlannerR (PViewR (pViewMode param))}" encType="#{encType}">
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


renderBoxGroupReport :: Maybe Text ->  Scenario -> Handler Widget
renderBoxGroupReport selectorM = renderConsoleReport report where
  report = do
    boxIds <- case selectorM of
      Nothing -> toList <$> gets boxes
      Just pat -> findBoxByStyleAndShelfNames (unpack pat)
    boxes <- mapM findBox boxIds
    groupBoxesReport boxes
  

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

