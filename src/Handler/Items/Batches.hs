module Handler.Items.Batches
( getItemBatchesR
, postItemBatchesR
, getItemBatchR
, getItemNewBatchR
, postItemNewBatchR
, getItemEditBatchR
, postItemEditBatchR
, postItemBatchUploadMatchesR
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
import Handler.Items.Batches.Matches
import Handler.CsvUtils(renderParsingResult)

-- * Form
batchForm today batchM = renderBootstrap3 BootstrapBasicForm form where
  form = Batch <$> areq textField "Name" (batchName <$> batchM)
               <*> aopt textField "Alias" (batchAlias <$> batchM)
               <*> aopt textField "Supplier" (batchSupplier <$> batchM)
               <*> aopt textField "Material" (batchMaterial <$> batchM)
               <*> aopt textField "Season" (batchSeason <$> batchM)
               <*> aopt textField "Description" (batchDescription <$> batchM)
               <*> areq dayField "Date" (batchDate <$> batchM <|> Just today)

-- * Handler
getItemBatchesR :: Handler Html
getItemBatchesR = do
  renderUrl <- getUrlRenderParams
  batcheEs <- runDB $ loadBatches
  let lastBatches = infoPanel "Last Batches" (batchTables renderUrl batcheEs)
  defaultLayout $ lastBatches >> infoPanel "Actions" [whamlet|
   <form method=get action="@{ItemsR ItemNewBatchR}">
     <button.btn.btn-default type=submit> Create New Batch
     |]


postItemBatchesR :: Handler Html
postItemBatchesR = do
  return ""


-- ** Single Batch
-- display a batch
getItemBatchR :: Int64 -> Handler Html
getItemBatchR key = do
  let batchKey = BatchKey (fromIntegral key)
  batch <- runDB $ getJust batchKey
  (form, encType) <- generateFormPost (uploadFileFormInline (pure ()))
  defaultLayout $ do
    renderBatch (Entity batchKey batch)
    [whamlet|
   <div.well>
      <form.form-inline role=form method=POST action="@{ItemsR (ItemBatchUploadMatchesR key)}" enctype=#{encType}>
        ^{form}
        <button.btn.btn-primary type=submit name="action" value="validate" > Upload matches
            |]

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

postItemBatchUploadMatchesR :: Int64 -> Handler Html
postItemBatchUploadMatchesR key = do
  ((fileInfo,encoding, ()), (view, encType)) <- unsafeRunFormPost (uploadFileFormInline (pure ()))
  (bytes, hash ) <- readUploadUTF8 fileInfo encoding

  parsingResult <- runDB $ parseMatchRows bytes
  let w = renderParsingResult (\msg w' -> do msg >> w') (\r -> setSuccess "Spreadsheet parsed successfully") parsingResult
  defaultLayout w


  
  -- return "Todo"

-- * Rendering
renderBatch :: Entity Batch -> Widget
renderBatch (Entity _ Batch{..}) = infoPanel ("Batch: " <> batchName) [whamlet|
<table.table>
  <tr>
    <th>Name
    <td>#{batchName}
  <tr>
    <th>Alias
    <td>#{fromMaybe "" batchAlias}
  <tr>
    <th>Supplier
    <td>#{fromMaybe "" batchSupplier}
  <tr>
    <th>Material
    <td>#{fromMaybe "" batchMaterial}
  <tr>
    <th>Season
    <td>#{fromMaybe "" batchSeason}
  <tr>
    <th>Description
    <td>#{fromMaybe "" batchDescription}
  <tr>
    <th>Date
    <td>#{tshow batchDate}
|]
  
batchTables :: _renderUrl -> [Entity Batch] -> Widget
batchTables renderUrl batches = [whamlet|
  <table.table.table-hover>
    ^{rowsAndHeader}
  |] where
  rowsAndHeader = displayTableRowsAndHeader columns colDisplays (map ((,[]) . entityColumnToColDisplay)  batches) where
  columns = [ entityKeyToColumn renderUrl (ItemsR . ItemBatchR) BatchId
            , entityFieldToColumn BatchName
            , entityFieldToColumn BatchAlias
            , entityFieldToColumn BatchSupplier
            , entityFieldToColumn BatchMaterial
            , entityFieldToColumn BatchSeason
            , entityFieldToColumn BatchDescription
            , entityFieldToColumn BatchDate]
  colDisplays (name, _) = (toHtml name, [])

  
-- * DB
loadBatches :: SqlHandler [Entity Batch]
loadBatches = do
  batches <- selectList [] [Desc BatchDate, Asc BatchName]
  return batches
