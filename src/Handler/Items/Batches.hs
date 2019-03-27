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
, getItemBatchExpandMatchesR
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
  , mtBatchCategory :: Text -- which category to use as batch
  , mtAggregationMode :: MatchAggregationMode
  , mtRowAggregationMode :: Maybe BatchMergeMode -- merging means hide the batch column
  , mtDisplayMode :: QualityDisplayMode
  } deriving (Show, Read, Eq)
matchTableForm categories = renderBootstrap3 BootstrapInlineForm form where
  form = MatchTableParam <$> pure [] -- filled manually by parsing params
                         <*> aopt filterEField "Sku" Nothing
                         <*> areq (selectFieldList $ zip categories categories ) "Batch category" Nothing
                         <*> areq (selectField optionsEnum) "Aggregation Mode" Nothing
                         <*> aopt (selectField optionsEnum) "Aggregation Batch" Nothing
                         <*> areq (selectField optionsEnum) "Display Mode" Nothing

batchCategoryIndexForm categories batchName = renderBootstrap3 BootstrapInlineForm form where
  form = (,) <$> areq (selectFieldList $ zip categories categories ) ("Batch category" { fsName = Just "category"}) Nothing
             <*> areq hiddenField ("" { fsName = Just "category-filter"}) (Just batchName)
-- * Handler
-- ** All batches
getItemBatchesR :: Handler Html
getItemBatchesR = do
  renderUrl <- getUrlRenderParams
  batcheEs <- runDB $ loadBatches
  categories <- batchCategoriesH
  ((_, form), encType) <- runFormGet $ matchTableForm categories

  extra <- uploadBatchExtra
  (uForm, uEncType) <- generateFormPost (uploadFileFormInline extra)
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
   <form.form-inline role=form method=POST action="@{ItemsR (ItemBatchUploadMatchesR)}" enctype=#{uEncType}>
        ^{uForm}
        <button.btn.btn-primary type=submit name="action" value="validate" > Upload matches
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
  categories <- batchCategoriesH
  ((_, form), encType) <- runFormGet $ batchCategoryIndexForm categories (batchName batch)
  defaultLayout $ do
    renderBatch (Entity batchKey batch)
    [whamlet|
   <form.form-inline role=form method=GET action="@{ItemsR (ItemsIndexR Nothing)}" enctype=#{encType}>
        ^{form}
        <button.btn.btn-primary type=submit >View Items
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

-- saveMatchesForm :: Maybe (Encoding, ((DocumentHash, Text), (_, _, _))) -> _ -> _ (Form (Encoding, (DocumentHash, Text)), _ ) 
saveMatchesForm encoding'hash'''pathM''day'''operator = renderBootstrap3 BootstrapBasicForm  form where
  form = (,) <$> areq hiddenField "encoding" (fst <$> encoding'hash'''pathM''day'''operator )
             <*> ((,) <$>  ((,) <$> areq hiddenField "" (fst . fst . snd <$> encoding'hash'''pathM''day'''operator)
                                <*>  areq hiddenField "" (snd . fst . snd <$> encoding'hash'''pathM''day'''operator)
                            )
                      <*> ((,,) <$> areq hiddenField ""  ( view1 . snd . snd <$> encoding'hash'''pathM''day'''operator)
                                <*> areq hiddenField ""  ( view2 . snd . snd <$> encoding'hash'''pathM''day'''operator)
                                <*> areq hiddenField ""  ( view3 . snd . snd <$> encoding'hash'''pathM''day'''operator)
                          )
                 )
  view1 (a, _, _ ) = a
  view2 (_, b, _ ) = b
  view3 (_, _, c ) = c

-- ** Upload
uploadBatchExtra =  do
  today <- todayH
  categories <- batchCategoriesH
  let operatorOptions = optionsPersistKey [OperatorActive ==. True] [Asc OperatorNickname] operatorNickname
  return ( (,,) <$>  areq dayField "Date" (Just today)
           <*> areq (selectField operatorOptions) "Operator" Nothing
           <*> areq (selectFieldList $ zip categories categories) "Operator" Nothing
         )
postItemBatchUploadMatchesR :: Handler Html
postItemBatchUploadMatchesR = do
  extra <- uploadBatchExtra
  ((fileInfo,encoding, (day, operator, batchCategory)), (view, encType)) <- unsafeRunFormPost (uploadFileFormInline extra)
  Just (bytes, hash, path ) <- readUploadOrCacheUTF8 encoding (Just fileInfo) Nothing Nothing

  parsingResult <- parseMatchRows batchCategory bytes
  let onSuccess rows = do
        -- check if the spreadhsheet has already been uploaded
        (documentKey'msgM) <- runDB $ loadAndCheckDocumentKey hash
        forM documentKey'msgM $ \(_, msg) -> do
           setWarning msg
        setSuccess "Spreadsheet parsed successfully"
        (uploadFileFormW, encType) <- generateFormPost $ saveMatchesForm (Just (encoding, ((hash, path), (day, operator, batchCategory))))
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
      ((encoding, ((hash, path), (day, operator, batchCategory)) ), _) <- unsafeRunFormPost (saveMatchesForm Nothing)
      Just (bytes, hash, _) <- readUploadOrCacheUTF8 encoding Nothing (Just hash) (Just path)
      let onSuccess valids = runDB $ do
            let finals = map finalizeRow valids
            (documentKey'msgM) <- loadAndCheckDocumentKey hash
            docKey <- case documentKey'msgM of
              Nothing -> createDocumentKey (DocumentType "BatchMatch") hash path ""
              Just (dock, _) -> return  (entityKey dock)
            let matches = map (finalRowToMatch day (Just operator) (docKey)) finals
            mapM_ insert_ matches
            setSuccess "Batch matches uploaded successfully"
      parsingResult <- parseMatchRows batchCategory bytes
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
  categories <- batchCategoriesH
  (param, _) <- unsafeRunFormGet $ matchTableForm categories
  key'values <- reqGetParams <$> getRequest
  let radios = mapMaybe (\(k, v) -> (,) <$> (stripPrefix radioNamePrefix k >>= readMay )
                                        <*> readMay v
                        ) key'values
      toBatchKey :: Int64 -> Key Batch
      toBatchKey = fromBackendKey . SqlBackendKey
  traceShowM (key'values, radios)
  return $ param { mtBatchRole = map (first toBatchKey) radios}
  
loadMatchTable :: MatchTableParam -> Handler Widget -- [Entity BatchMatch]
loadMatchTable MatchTableParam{..} | Just skuFilter <- mtSkuFilter = do
  -- TODO factorize  
  let columns = map fst $ filter ((== AsColumn) . snd) mtBatchRole
  skuToStyleVar <- skuToStyleVarH
  tableW <- runDB $ do
      sku'batchIds <- loadSkuBatches mtBatchCategory skuFilter
      let rows = List.nub $ sort $ map snd sku'batchIds
      -- load batch the correct way
      normal <- selectList (BatchMatchSource <-?. rows <> BatchMatchTarget <-?. columns) []
      wrongWay <- selectList (BatchMatchSource <-?. columns <>  BatchMatchTarget <-?. rows) []
      let matches = ForBuildTable $ map entityVal normal ++ map (reverseBatchMatch . entityVal) wrongWay

      rowBatches <- selectList [BatchId <-. rows] []
      columnBatches <- selectList [BatchId <-. columns] []
      let style''var'batchs = map (skuToStyle''var'Batch skuToStyleVar rowBatches) sku'batchIds

      return $ case mtRowAggregationMode of
               Nothing -> displayTable `uncurry3` buildTableForSku (colour'QualitysToHtml' mtAggregationMode mtDisplayMode) style''var'batchs columnBatches  matches
               Just mergeMode -> displayTable `uncurry3` buildTableForSkuMerged mergeMode (colour'QualitysToHtml' mtAggregationMode mtDisplayMode) style''var'batchs columnBatches  matches
  return tableW
   

loadMatchTable MatchTableParam{..} = do
  let (rows, columns) = bimap (List.nub . sort . map fst)
                              (List.nub . sort . map fst) $ partition ((== AsRow) . snd) mtBatchRole
  tableW <- runDB $ do
      -- load batch the correct way
      normal <- selectList (BatchMatchSource <-?. rows <> BatchMatchTarget <-?. columns) []
      wrongWay <- selectList (BatchMatchSource <-?. columns <>  BatchMatchTarget <-?. rows) []
      let matches = ForBuildTable $ map entityVal normal ++ map (reverseBatchMatch . entityVal) wrongWay

      rowBatches <- selectList [BatchId <-. rows] []
      columnBatches <- selectList [BatchId <-. columns] []

      let (cols, colDisplay, tableRows) = buildTable (colour'QualitysToHtml' mtAggregationMode mtDisplayMode) rowBatches columnBatches  matches
      
      return $ displayTable cols colDisplay tableRows

  return tableW
      
getItemBatchExpandMatchesR :: Handler Html 
getItemBatchExpandMatchesR = do
  matches <- runDB $ selectList [] []
  let expanded = expandMatches $ map entityVal matches
      guessed = filter (isNothing . batchMatchOperator) expanded
      widget = [whamlet|
<table.table.table-hover.table-striped>
    <tr>
      <td>Source
      <td>Source Colour
      <td>Target
      <td>Target Colour
      <td>Quality
      <td>Comment
  $forall BatchMatch{..} <- guessed
    <tr>
      <td>#{unSqlBackendKey $ unBatchKey batchMatchSource}
      <td>#{batchMatchSourceColour}
      <td>#{unSqlBackendKey $ unBatchKey batchMatchTarget}
      <td>#{batchMatchTargetColour}
      $case batchMatchOperator
        $of Nothing 
          <td.text-danger>#{tshow batchMatchQuality}
        $of (Just _)
          <td>#{tshow batchMatchQuality}
      <td>#{tshowM batchMatchComment}
                   |]
  defaultLayout widget
  

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

colour'QualitysToHtml' :: MatchAggregationMode -> QualityDisplayMode -> [(Text, MatchQuality)] -> Html
colour'QualitysToHtml' aggregationMode displayMode c'qs0 = let
  (filterQ ,qualityToHtml') = case displayMode of
                         FullQuality -> (id, toHtml . tshow)
                         LimitQuality -> (filter ((/= Bad) . snd), qualityToShortHtml)
  c'qs = filterQ $ aggregateQuality aggregationMode c'qs0
  in colour'QualitysToHtml qualityToHtml' c'qs


  
    
  
-- * DB
loadBatches :: SqlHandler [Entity Batch]
loadBatches = do
  batches <- selectList [] [Desc BatchDate, Asc BatchName]
  return batches
