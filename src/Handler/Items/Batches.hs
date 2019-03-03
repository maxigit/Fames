module Handler.Items.Batches
( getItemBatchesR
, postItemBatchesR
) where

import Import
import Items.Types
import Handler.Items.Reports.Common
import Handler.Items.Common
import Handler.Util
import Handler.Table
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import Lens.Micro.Extras
import Data.Text(toTitle)


-- * Form
-- * Handler
getItemBatchesR :: Handler Html
getItemBatchesR = do
  return ""


postItemBatchesR :: Handler Html
postItemBatchesR = do
  return ""


-- * Rendering
batchTables :: [Entity Batch] -> Widget
batchTables batches = displayTable columns colDisplays (zip (map batchToRow batches) (List.cycle [])) where
  columns = [ xxx BatchId, xxx BatchName, xxx BatchDescription, xxx BatchCreatedAt]
  colDisplays (name, _) = (toHtml name, [])
  batchToRow batch (name, getter) = Just ( toHtml . tshow $ getter batch, [name] )


xxx  :: PersistField typ => EntityField Batch typ -> (Text, Entity Batch  -> PersistValue )
xxx field = (fieldName, getter) where
  fieldDef = persistFieldDef field
  fieldName = toTitle . unDBName $ fieldDB fieldDef
  getter = toPersistValue . view (fieldLens field)
  

-- * DB
loadBatches :: SqlHandler [Entity Batch]
loadBatches = do
  batches <- selectList [] [Desc BatchCreatedAt, Asc BatchName]
  return batches

