module Handler.Items.Batches
( getItemBatchesR
, postItemBatchesR
, getItemBatchR
, getItemNewBatchR
, postItemNewBatchR
, getItemEditBatchR
, postItemEditBatchR
) where

import Import
import Items.Types
import Handler.Items.Reports.Common
import Handler.Items.Common
import Handler.Util
import Handler.Table
import Yesod.Form.Bootstrap3 (renderBootstrap3, BootstrapFormLayout(..))
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import Data.Text(toTitle)
import Database.Persist.Sql(unSqlBackendKey)

-- * Form
-- * Handler
getItemBatchesR :: Handler Html
getItemBatchesR = do
  batcheEs <- runDB $ loadBatches
  let lastBatches = infoPanel "Last Batches" (batchTables batcheEs)
  defaultLayout lastBatches


postItemBatchesR :: Handler Html
postItemBatchesR = do
  return ""


-- ** Single Batch
-- display a batch
getItemBatchR :: Int64 -> Handler Html
getItemBatchR key = do
  let batchKey = BatchKey (fromIntegral key)
  batch <- runDB $ getJust batchKey
  renderBatch (Entity batchKey batch)
  
-- display page to create a new batch
getItemNewBatchR :: Handler Html
getItemNewBatchR = do
  -- ret
  today <- todayH
  ((_, form), encType) <- runFormPost $ batchForm today Nothing
  defaultLayout $ primaryPanel "New Batch" [whamlet|
    <form method=post action="@{ItemsR ItemNewBatchR}" enctype="#{encType}">
      ^{form}
      <button.btn.btn-danger type="submit" name="action" value="Submit"> Create
      <button.btn.btn-default type="submit" name="action" value="Cancel"> Cancel
      |]
                                                     
batchForm today batchM = renderBootstrap3 BootstrapBasicForm form where
  form = Batch <$> areq textField "Name" (batchName <$> batchM)
               <*> aopt textField "Description" (batchDescription <$> batchM)
               <*> areq dayField "Date" (batchDate <$> batchM <|> Just today)

postItemNewBatchR :: Handler Html
postItemNewBatchR = do
  today <- todayH
  (batch, _) <- unsafeRunFormPost (batchForm today Nothing)
  key <- runDB $  insert batch
  setSuccess "Batch has been successfuly saved!"
  redirect (ItemsR . ItemBatchR $ unSqlBackendKey (unBatchKey key))

getItemEditBatchR :: Int64 -> Handler Html
getItemEditBatchR key = return "Todo"
 
postItemEditBatchR :: Int64 -> Handler Html
postItemEditBatchR key = return "Todo"

-- * Rendering
renderBatch :: Entity Batch -> Handler Html
renderBatch (Entity _ Batch{..}) = defaultLayout $ infoPanel ("Batch: " <> batchName) [whamlet|
<table.table>
  <tr>
    <th>Name
    <td>#{batchName}
  <tr>
    <th>Description
    <td>#{fromMaybe "" batchDescription}
  <tr>
    <th>Created At
    <td>#{tshow batchDate}
|]
  
batchTables :: [Entity Batch] -> Widget
batchTables batches = [whamlet|
  <table.table.table-hover>
    ^{rowsAndHeader}
  |] where
  rowsAndHeader = displayTableRowsAndHeader columns colDisplays (map ((,[]) . entityColumnToColDisplay)  batches) where
  columns = [ entityFieldToColumn BatchId, entityFieldToColumn BatchName, entityFieldToColumn BatchDescription, entityFieldToColumn BatchDate]
  colDisplays (name, _) = (toHtml name, [])

-- * DB
loadBatches :: SqlHandler [Entity Batch]
loadBatches = do
  batches <- selectList [] [Desc BatchDate, Asc BatchName]
  return batches
  
