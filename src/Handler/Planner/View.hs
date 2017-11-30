module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
)

where

import Import
import Planner.Internal
import Handler.Planner.Exec
import WarehousePlanner.Base
import Diagrams.Prelude hiding(iso)
import Diagrams.Backend.Cairo
import qualified Yesod.Media.Simple as M
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- * Type
data FormParam = FormParam
  { pOrgfile :: Textarea
  } deriving (Show, Read)

defaultParam = FormParam (Textarea "")

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

getPImageR :: Text -> Int64 -> Handler TypedContent
getPImageR sha width = do
  scenarioM <- cacheScenarioOut sha
  traceShowM ("IMAGE", scenarioM)
  case scenarioM of
    Nothing -> do
      error "No matching scenario"
    Just scenario -> do
        diagE <- renderScenario scenario Nothing
        case diagE of
          Left e -> error e
          Right diag -> sendResponseDiag width diag
  
-- * Form

paramForm :: Maybe FormParam -> _
paramForm param = renderBootstrap3 BootstrapBasicForm form
  where form = FormParam
          <$> areq textareaField "org" (pOrgfile <$> param)

-- * Rendering

renderView :: FormParam -> Handler Html
renderView param = do
  scenarioE <- readScenario (unTextarea $ pOrgfile param)
  traceShowM ("VIEW", scenarioE)
  (formW, encType) <- generateFormPost $ paramForm (Just param)
  imgRouteM <- case scenarioE of
      Left err -> setError (toHtml err) >> return Nothing
      Right scenario ->  do
          sha <- cacheScenarioIn scenario

          return $ Just (\width -> PlannerR (PImageR (sha) width))

  defaultLayout $ [whamlet|
<form #planner-view role=form method=post action="@{PlannerR PViewR}" encType="#{encType}">
  ^{formW}
  <button type="submit" .btn .btn-default>Submit
$maybe imgRoute <- imgRouteM
  <div.well>
    #{tshow (fmap scenarioToTextWithHash scenarioE)}
  <div.well>
    <a href="@{imgRoute 2000}" ><img src=@{imgRoute 800} style="width:800;">
|]

  

sendResponseDiag :: Int64 -> _ -> Handler TypedContent
sendResponseDiag width diag =  do
  let size = dims2D w w 
      w = fromIntegral width
  M.renderContent (M.SizedDiagram size diag)
