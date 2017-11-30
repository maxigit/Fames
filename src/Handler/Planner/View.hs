module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
)

where

import Import
import Planner.Internal
import Planner.Types
import Handler.Planner.Exec
import WarehousePlanner.Base
import Diagrams.Prelude hiding(iso)
import Diagrams.Backend.Cairo
import qualified Yesod.Media.Simple as M
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Util.Cache
import Data.List((!!))

-- * Type
data ScenarioDisplayMode = NormalM | CompactM | InitialM | ExpandedM deriving (Eq, Show, Read)
data FormParam = FormParam
  { pOrgfile :: Textarea
  , pDisplayMode :: Maybe ScenarioDisplayMode
  } deriving (Show, Read)

defaultParam = FormParam (Textarea "") Nothing

-- * Handler
getPViewR :: Handler Html
getPViewR = renderView defaultParam

postPViewR :: Handler Html
postPViewR = do
  ((resp, _), _) <- runFormPost (paramForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderView param

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
          <$> areq textareaField "org" (pOrgfile <$> param)
          <*> pure (pDisplayMode =<< param)

-- * Rendering

renderView :: FormParam -> Handler Html
renderView param0 = do
  modeS <- lookupPostParam "mode"
  let mode = modeS >>=readMay
  scenarioE <- readScenario (unTextarea $ pOrgfile param0)
  traceShowM ("VIEW", scenarioE)
  (imgRouteM, param, layoutSize) <- case scenarioE of
      Left err -> setError (toHtml err) >> return (Nothing, Nothing,0)
      Right scenario ->  do
          (sha, lSize) <- cacheScenarioIn scenario
          param <- expandScenario (param0 {pDisplayMode = mode}) scenario

          return $ (Just (\i width -> PlannerR (PImageR (sha) i width))
                   , Just param
                   , lSize
                   )

  (formW, encType) <- generateFormPost $ paramForm param

  let is = [0..fromIntegral (layoutSize-1)]
  defaultLayout $ [whamlet|
<form #planner-view role=form method=post action="@{PlannerR PViewR}" encType="#{encType}">
  ^{formW}
  <button type="submit" .btn .btn-default name="mode" value="NormalM">Submit
  <button type="submit" .btn .btn-default name="mode" value="CompactM">Compact
  <button type="submit" .btn .btn-danger name="mode" value="InitialM">Save
  <button type="submit" .btn .btn-primary name="mode" value="ExpandedM">Expand
$maybe imgRoute <- imgRouteM
  <div.well>
    <table>
    $forall i <- is
      <tr><td><a href="@{imgRoute i 8000}" ><img src=@{imgRoute i 350} style="width:800;">
|]

  
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
