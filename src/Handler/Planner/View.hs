module Handler.Planner.View
( getPViewR
, postPViewR
, getPImageR
)

where

import Import
import Planner.Internal
import WarehousePlanner.Base
import Diagrams.Prelude hiding(iso)
import Diagrams.Backend.Cairo
import qualified Yesod.Media.Simple as M
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import WarehousePlanner.Display
import Unsafe.Coerce (unsafeCoerce)
-- * Type
data FormParam = FormParam
  { pOrgfile :: Textarea
  } deriving (Show, Read)

defaultParam = FormParam (Textarea "")

-- data WarehouseCache = forall s . WarehouseCache (Warehouse s, ShelfGroup s) deriving Typeable
data WarehouseCache = WarehouseCache (Warehouse (), ShelfGroup ()) deriving Typeable
data DiagCache = DiagCache (Diagram Cairo) deriving Typeable
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
  -- diag <- cacheWarehouse $ {-error "not there !" -- -}  warehouseFromOrg "" -- hack use the last cached warehouse
  WarehouseCache (w0, groups0) <- cacheWarehouse $ error "not there !" 

  diag <- execWH (unsafeCoerce w0) (renderGroup (unsafeCoerce groups0))
  sendResponseDiag width diag
  
-- * Form

paramForm :: Maybe FormParam -> _
paramForm param = renderBootstrap3 BootstrapBasicForm form
  where form = FormParam
          <$> areq textareaField "org" (pOrgfile <$> param)

-- * Rendering

renderView :: FormParam -> Handler Html
renderView param = do
  warehouse <- cacheWarehouse $ warehouseFromOrg "" -- hack use the last cached warehouse
  (formW, encType) <- generateFormPost $ paramForm (Just param)

  let imgRoute width = PlannerR (PImageR "pipo" width)

  defaultLayout $ [whamlet|
<form #planner-view role=form method=post action="@{PlannerR PViewR}" encType="#{encType}">
  ^{formW}
<div.well>
 <a href="@{imgRoute 2000}" ><img src=@{imgRoute 800} style="width:800;">
|]

  

sendResponseDiag :: Int64 -> _ -> Handler TypedContent
sendResponseDiag width diag =  do
  let size = dims2D w w 
      w = fromIntegral width
  M.renderContent (M.SizedDiagram size diag)

cacheWarehouse warehouse = do
  cache0 False cacheForEver "titi" $ do
    (groups, w) <- runWH warehouse emptyWarehouse
    return $ WarehouseCache (unsafeCoerce w, unsafeCoerce groups)
  -- let d = d0

