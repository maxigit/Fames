module Handler.Items.Batches
( getItemBatchesR
, postItemBatchesR
, getItemBatchR
, getItemNewBatchR
, postItemNewBatchR
, getItemEditBatchR
, postItemEditBatchR
, postItemBatchUploadMatchesR
, postItemBatchSaveMatchesR
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
import Handler.CsvUtils(renderParsingResult, Renderable(..))

-- * Form
data Mode = Validate | Success deriving (Show, Read, Eq)
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

-- saveMatchesForm text = renderBootstrap3 BootstrapBasicForm form where
--   form = (,) <$> undefined <*> undefined
--   -- form = unTextarea <$> areq textareaField "dimensions" (fmap Textarea text)

-- saveMatchesForm :: Maybe (Encoding, (DocumentHash, Text)) -> _ -> _ (Form (Encoding, (DocumentHash, Text)), _ ) 
saveMatchesForm encoding''hash'pathM = renderBootstrap3 BootstrapBasicForm  form where
  form = (,) <$> areq hiddenField "encoding" (fst <$> encoding''hash'pathM )
             <*>  ((,) <$> areq hiddenField "key" (fst . snd <$> encoding''hash'pathM)
                       <*>  areq hiddenField "path" (snd . snd <$> encoding''hash'pathM)
                  )

postItemBatchUploadMatchesR :: Int64 -> Handler Html
postItemBatchUploadMatchesR key = do
  ((fileInfo,encoding, ()), (view, encType)) <- unsafeRunFormPost (uploadFileFormInline (pure ()))
  Just (bytes, hash, path ) <- readUploadOrCacheUTF8 encoding (Just fileInfo) Nothing Nothing

  parsingResult <- parseMatchRows bytes
  let onSuccess rows = do
        -- check if the spreadhsheet has already been uploaded
        (documentKey'msgM) <- runDB $ loadAndCheckDocumentKey hash
        forM documentKey'msgM $ \(_, msg) -> do
           setWarning msg
        setSuccess "Spreadsheet parsed successfully"
        (uploadFileFormW, encType) <- generateFormPost $ saveMatchesForm (Just (encoding, (hash, path)))
        return [whamlet|
              ^{render (map unvalidateRow rows)} 
              <div.well>
                 <form.form-inline role=form method=POST action="@{ItemsR ItemBatchSaveMatchesR}" enctype=#{encType}>
                   ^{uploadFileFormW}
                   <button.btn.btn-default type=sumbit name=action value=Cancel>Cancel
                   <button.btn.btn-danger type=sumbit name=action value=save>Save
                      |]
  w <- renderParsingResult (\msg w' -> return( msg >> w')) onSuccess parsingResult
  defaultLayout w

postItemBatchSaveMatchesR :: Handler Html
postItemBatchSaveMatchesR = do
  actionM <- lookupPostParam "action"
  case actionM of
    Just "save" -> do
      ((encoding, (hash, path) ), _) <- unsafeRunFormPost (saveMatchesForm Nothing)
      Just (bytes, hash, _) <- readUploadOrCacheUTF8 encoding Nothing (Just hash) (Just path)
      let onSuccess valids = runDB $ do
            let finals = map finalizeRow valids
            today <- todayH
            (documentKey'msgM) <- loadAndCheckDocumentKey hash
            docKey <- case documentKey'msgM of
              Nothing -> createDocumentKey (DocumentType "BatchMatch") hash path ""
              Just (dock, _) -> return  (entityKey dock)
            let matches = map (finalRowToMatch today Nothing (docKey)) finals
            mapM_ insert_ matches
            setSuccess "Batch matches uploaded successfully"
      parsingResult <- parseMatchRows bytes
      renderParsingResult (\msg _ -> msg) onSuccess parsingResult
    _ ->  return ()
  getItemBatchesR
     
      



      
  


  
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
