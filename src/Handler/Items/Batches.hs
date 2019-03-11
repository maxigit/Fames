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
, getItemBatchMatchTableR
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
import Database.Persist.Sql -- (unSqlBackendKey)

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

-- | Form used to display a table of how how batches or items matches
data BatchRole = AsColumn | AsRow deriving (Show, Read, Eq)
data MatchTableParam  = MatchTableParam
  { mtBatchRole :: [(Key Batch, BatchRole) ] -- wether a batch is displayed as a column or row or not a all
  , mtSkuFilter :: Maybe FilterExpression -- 
  } deriving (Show, Read, Eq)
matchTableForm = renderBootstrap3 BootstrapInlineForm form where
  form = MatchTableParam <$> pure [] -- filled manually by parsing params
                         <*> aopt filterEField "Sku" Nothing

-- * Handler
-- ** All batches
getItemBatchesR :: Handler Html
getItemBatchesR = do
  renderUrl <- getUrlRenderParams
  batcheEs <- runDB $ loadBatches
  ((_, form), encType) <- runFormGet matchTableForm
  let allBatches = batchTables renderUrl roleRadioColumns batcheEs
      mainPanel = infoPanel "All Batches" [whamlet|
        <form role=form method=get action="@{ItemsR ItemBatchMatchTableR}" encType=#{encType}>
          ^{allBatches}
          <div.form-inline>
            ^{form}
            <button.btn.btn-default type="submit"> Display Match Table
                 |]
  defaultLayout $ mainPanel >> infoPanel "Actions" [whamlet|
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

-- ** Upload
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
    
-- ** Match Table
getItemBatchMatchTableR :: Handler Html
getItemBatchMatchTableR = do
  param <- getMatchTableParam 
  table <- loadMatchTable param
  defaultLayout $ primaryPanel "Match Table" [whamlet|
^{table}
                        |]

getMatchTableParam :: Handler MatchTableParam
getMatchTableParam = do
  (param, _) <- unsafeRunFormGet matchTableForm
  key'values <- reqGetParams <$> getRequest
  let radios = mapMaybe (\(k, v) -> (,) <$> (stripPrefix radioNamePrefix k >>= readMay )
                                        <*> readMay v
                        ) key'values
      toBatchKey :: Int64 -> Key Batch
      toBatchKey = fromBackendKey . SqlBackendKey
  traceShowM (key'values, radios)
  return $ param { mtBatchRole = map (first toBatchKey) radios}
  
loadMatchTable :: MatchTableParam -> Handler Widget -- [Entity BatchMatch]
loadMatchTable param | Just skuFilter <- mtSkuFilter param  = do
  let columns = map fst $ filter ((== AsColumn) . snd) (mtBatchRole param)
  skuToStyleVar <- skuToStyleVarH
  tableW <- runDB $ do
      sku'batchIds <- loadSkuBatches skuFilter
      let rows = List.nub $ sort $ map snd sku'batchIds
      -- load batch the correct way
      normal <- selectList (BatchMatchSource <-?. rows <> BatchMatchTarget <-?. columns) []
      wrongWay <- selectList (BatchMatchSource <-?. columns <>  BatchMatchTarget <-?. rows) []
      let matches = ForBuildTable $ map entityVal normal ++ map (reverseBatchMatch . entityVal) wrongWay

      rowBatches <- selectList [BatchId <-. rows] []
      columnBatches <- selectList [BatchId <-. columns] []
      let style''var'batchs = map (skuToStyle''var'Batch skuToStyleVar rowBatches) sku'batchIds

      let (cols, colDisplay, tableRows) = buildTableForSku (concatMap colour'QualityToHtml) style''var'batchs columnBatches  matches
      
      return $ displayTable cols colDisplay tableRows
  return tableW
   

loadMatchTable param = do
  let (rows, columns) = bimap (List.nub . sort . map fst)
                              (List.nub . sort . map fst) $ partition ((== AsRow) . snd) (mtBatchRole param)
  tableW <- runDB $ do
      -- load batch the correct way
      normal <- selectList (BatchMatchSource <-?. rows <> BatchMatchTarget <-?. columns) []
      wrongWay <- selectList (BatchMatchSource <-?. columns <>  BatchMatchTarget <-?. rows) []
      let matches = ForBuildTable $ map entityVal normal ++ map (reverseBatchMatch . entityVal) wrongWay

      rowBatches <- selectList [BatchId <-. rows] []
      columnBatches <- selectList [BatchId <-. columns] []

      let (cols, colDisplay, tableRows) = buildTable (concatMap colour'QualityToHtml) rowBatches columnBatches  matches
      
      return $ displayTable cols colDisplay tableRows


  return tableW
    

-- * Rendering
renderBatch :: Entity Batch -> Widget
renderBatch batchEntity@(Entity _ Batch{..}) = infoPanel ("Batch: " <> batchName) [whamlet|
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

radioNamePrefix = "batch-role-" :: Text
radioName (Entity batchId _) = radioNamePrefix <> tshow (unSqlBackendKey $ unBatchKey batchId)
roleRadioColumns :: [(Text, Entity Batch -> Either Html PersistValue)]
roleRadioColumns =  map (uncurry go) [ ("Don't use" :: Text, "" :: Text)
                              , ("As AsRow", tshow AsRow)
                              , ("As AsColumn", tshow AsColumn)
                              ] where
  go title value = ( title
                   , \batchEntity -> Left [shamlet|
                         <input type=radio name="#{radioName batchEntity}" value="#{value}">
                         |]
                   )
  
batchTables :: _renderUrl -> [(Text, Entity Batch -> Either Html PersistValue)] -> [Entity Batch] -> Widget
batchTables renderUrl extraColumns batches = [whamlet|
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
            , entityFieldToColumn BatchDate
            ] <> extraColumns
  colDisplays (name, _) = (toHtml name, [])

  
-- * DB
loadBatches :: SqlHandler [Entity Batch]
loadBatches = do
  batches <- selectList [] [Desc BatchDate, Asc BatchName]
  return batches
