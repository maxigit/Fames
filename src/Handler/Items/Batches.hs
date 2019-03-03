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


-- * Form
-- * Handler
getItemBatchesR :: Handler Html
getItemBatchesR = do
  batcheEs <- runDB $ loadBatches
  let table = batchTables batcheEs
  defaultLayout table


postItemBatchesR :: Handler Html
postItemBatchesR = do
  return ""


-- * Rendering
batchTables :: [Entity Batch] -> Widget
batchTables batches = [whamlet|
  <table.table.table-hover>
    ^{rowsAndHeader}
  |] where
  rowsAndHeader = displayTableRowsAndHeader columns colDisplays (zip (map batchToRow batches) (List.cycle [])) where
  columns = [ entityFieldToColumn BatchId, entityFieldToColumn BatchName, entityFieldToColumn BatchDescription, entityFieldToColumn BatchCreatedAt]
  colDisplays (name, _) = (toHtml name, [])
  batchToRow batch (name, getter) = Just ( toHtml . tshow $ getter batch, [name] )

-- * DB
loadBatches :: SqlHandler [Entity Batch]
loadBatches = do
  batches <- selectList [] [Desc BatchCreatedAt, Asc BatchName]
  return batches

