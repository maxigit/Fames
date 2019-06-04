module Handler.GL.TaxReport
( getGLTaxReportsR
, postGLNewTaxReportR 
, getGLTaxReportR
, postGLTaxReportR
, getGLTaxReportDetailsR
, postGLTaxReportCollectDetailsR
) where
import Import hiding(RuleInput)
import GL.TaxReport.Types
import GL.TaxReport.Settings
import GL.TaxReport
import Handler.GL.TaxReport.Types
import GL.Utils
import Data.Time (addDays)
import Database.Persist.MySQL(unSqlBackendKey)
import Database.Persist.Sql (rawSql, RawSql, fromSqlKey, toSqlKey, Single(..))
import Data.Aeson(encode)
import qualified FA as FA
import Metamorphosis
import Lens.Micro
import Formatting as F
import Data.Tagged
import qualified Data.Map as Map
import Data.List(nub)

-- * Handler
-- | Display the list of the available tax reports
getGLTaxReportsR :: Handler Html
getGLTaxReportsR = do
  settings <- appTaxReportSettings <$> getsYesod appSettings
  name'tables <- forM (mapToList settings) renderReportList
  let panel name tableW = infoPanel name (tableW >> newW)
        where newW = [whamlet|
       <div.well>
         <form method=POST action=@{GLR $ GLNewTaxReportR name}>
           <button.btn.btn-danger>New
                   |]
  defaultLayout [whamlet|
   <h1.jumbotron> Tax report
   $forall (name, tableW) <- name'tables
     ^{panel name tableW }
     |]

-- | Create a report for the given type
postGLNewTaxReportR :: Text -> Handler Html
postGLNewTaxReportR name = do
  settings <- unsafeGetReportSettings name
  runDB $ do
    lasts <- selectList [TaxReportType ==. name] [Desc TaxReportStart]
    let taxReportStart = case lasts of
          (Entity _ report: _) -> 1 `addDays` (Import.taxReportEnd report)
          [] -> startDate settings
        taxReportType = name
        taxReportReference = pack $ formatTime defaultTimeLocale (unpack $ referenceFormat settings) taxReportStart
        taxReportEnd = (-1) `addDays` calculateDate (nextPeriod settings) taxReportStart
        taxReportStatus = Pending
        taxReportSubmittedAt = Nothing

    key <- insert TaxReport{..}
    lift $ do
      setSuccess "Report created sucessfully"
      let newRoute =GLR $ GLTaxReportR (fromSqlKey key) Nothing 
      pushLinks ("View tax report " <> taxReportReference) newRoute []
      getGLTaxReportR (fromSqlKey key) Nothing >>= sendResponseStatus created201

getGLTaxReportR :: Int64 -> Maybe TaxReportViewMode -> Handler Html
getGLTaxReportR key mode = do
  report <- runDB $ loadReport key
  reportSettings  <- getReportSettings (taxReportType $ entityVal report)
  let reportRules = maybe defaultRule rules reportSettings
  (before, inRange)  <- runDB $ countPendingTransTaxDetails (entityVal report)
  view <- renderReportView reportRules report (fromMaybe TaxReportPendingView mode)
  when (before > 0) $ do
    setWarning "Some Transactions part of the  have been entered or modified before current report start date"
  defaultLayout $ renderReportHeader report (before, inRange) >> view

postGLTaxReportR :: Int64 -> Maybe TaxReportViewMode -> Handler Html
postGLTaxReportR key modem = return "todo"


postGLTaxReportCollectDetailsR :: Int64 -> Handler Html
postGLTaxReportCollectDetailsR key = do
  let rId = TaxReportKey (fromIntegral key)
  report <- runDB $ do
    report <- getJust rId
    reportSettings  <- lift $ getReportSettings (taxReportType report)
    let reportRules = maybe defaultRule rules reportSettings
    details <- lift $ loadPendingTaxDetails reportRules (Entity rId report)
    insertMany_ $ mapMaybe tdDetailToSave details
    return report

  setSuccess "Transaction tax details collected successfully"
  let newRoute = GLR . GLTaxReportR key
  pushLinks ("View tax report " <> taxReportReference report) (newRoute Nothing) []
  getGLTaxReportR key (Just TaxReportCollectedView) >>= sendResponseStatus created201
     

-- | Load 
loadBucketSummary :: Key TaxReport -> SqlHandler (Map (Bucket, Entity FA.TaxType) TaxSummary)
loadBucketSummary key = do
  taxTypes <- selectList [] []
  let
    rateMap :: Map FA.TaxTypeId (Entity FA.TaxType)
    rateMap = mapFromList $ map (fanl entityKey) taxTypes
  let sql = "SELECT bucket, fa_tax_type, SUM(net_amount), SUM(tax_amount)  "
            <> " FROM fames_tax_report_detail "
            <> " WHERE tax_report_id = ? "
            <> " GROUP BY bucket, fa_tax_type"
  raws <- rawSql sql (keyToValues key)
  return $ mapFromList  [ ((bucket, taxType), tx )
                           | (Single bucket, Single taxId, Single net, Single tax) <- raws
                           , let tx = TaxSummary net tax
                           , let Just taxType = lookup taxId rateMap
                           ]
  

-- * Render

renderReportList :: (Text, TaxReportSettings) -> Handler (Text, Widget)
renderReportList (name, settings) = runDB $ do
  reports <- selectList [TaxReportType==. name] [Desc TaxReportEnd]
  let widget = [whamlet|
  <table *{datatable}>
    <thead>
      <tr>
        <th> Id
        <th> Reference
        <th> Start
        <th> End
        <th> Status
        <th> Submitted
    $forall (Entity reportKey TaxReport{..}) <- reports
      <tr>
        <td> <a href="@{GLR $ GLTaxReportR (fromSqlKey reportKey) Nothing}">  
            #{tshow $ fromSqlKey reportKey}
        <td> #{taxReportReference}
        <td> #{tshow taxReportStart}
        <td> #{tshow taxReportEnd}
        <td> #{tshow taxReportStatus}
        <td> #{tshowM taxReportSubmittedAt}
  |]
  return (name, widget)

renderReportHeader :: Entity TaxReport -> (Int, Int)-> Widget
renderReportHeader (Entity rId TaxReport{..}) (before, inRange) = do
  let panelClass = case (taxReportStatus, taxReportSubmittedAt) of
                     (Pending, Just _ ) -> "panel-danger" -- shoudn't happeellon
                     (Pending, Nothing) -> "panel-warning" -- in progress
                     (Process, Just _) -> "panel-success" -- done
                     (Process, Nothing) -> "panel-success" -- shoudn't happen
      hasBefore = before > 0
      panel1 = panel panelClass taxReportReference  [whamlet|
   <table.table.table-border.table-striped>
     <tr>
       <th> Type
       <td> #{taxReportType}
     <tr>
       <th> Start
       <td> #{tshow taxReportStart}
     <tr>
       <th> End
       <td> #{tshow taxReportEnd}
     <tr>
       <th> Status
       <td> #{tshow taxReportStatus}
     <tr>
       <th> Submitted
       <td> #{tshow taxReportSubmittedAt}
     <tr :hasBefore:.text-danger.bg-danger>
       <th> Transaction out of report range  
       <td> #{tshow before}
     <tr> 
       <th> Transaction within report range  
       <td> #{tshow inRange}
          |]
  panel1

collectButtonForm :: Key TaxReport -> Widget
collectButtonForm key = [whamlet|
  <form method=POST action="@{GLR $ GLTaxReportCollectDetailsR $ fromSqlKey key}">
    <button.btn.btn-danger type="submit"> Collect
                            |]
renderReportView :: Rule -> Entity TaxReport -> TaxReportViewMode ->  Handler Widget
renderReportView rule report mode = do
  settings <- unsafeGetReportSettings (taxReportType $ entityVal report)
  bucket'rates <- getBucketRateFromConfig report
  view <- case mode of
            TaxReportPendingView -> do
              details <- loadPendingTaxDetails rule report
              return $ renderTaxDetailTable (taxReportStart $ entityVal report) details
                     >> collectButtonForm (entityKey report)
            TaxReportCollectedView -> do
              details <- loadCollectedTaxDetails report
              return $ renderTaxDetailTable (taxReportStart $ entityVal report) details
            TaxReportBucketView -> do
              buckets <- runDB $ loadBucketSummary (entityKey report)
              return $ renderBucketTable bucket'rates buckets
            TaxReportBoxesView -> do
              buckets <- runDB $ loadBucketSummary (entityKey report)
              let box'amounts = computeBoxes bucket'rates buckets (boxes settings)
              return $ renderBoxTable box'amounts
            TaxReportConfigChecker -> do
              buckets <- getBucketRateFromConfig report
              return $ renderBoxConfigCheckerTable buckets (boxes settings)
  let navs = [TaxReportPendingView .. ]
      myshow t0 = let t =  drop  12 $ splitSnake $ tshow t0
                  in take (length t - 4) t :: Text
      navClass nav = if mode == nav then "active" else "" :: Text
      mainW = [whamlet|
        <ul.nav.nav-tabs>
          $forall nav <- navs 
            <li class="#{navClass nav}">
              <a href="@{GLR $ GLTaxReportR (fromSqlKey $ entityKey report) (Just nav)}">#{myshow nav}
                    |]
  return $ mainW >> view
-- | Render a table with the transaction, with or without bin
  
_to_remove_renderTransDifferedTable reportId = do
  [whamlet|
  <table#trans-table *{datatable} data-ajax="@{GLR $ GLTaxReportDetailsR (fromSqlKey reportId)}">
    <thead>
      <tr>
        <th>Date
        <th data-class-name="text-right"> TransNo
        <th> TransType
        <th> Memo
        <th data-class-name="text-right">netAmount
        <th data-class-name="text-right">taxAmount
        <th data-class-name="text-right">rate
        <th>Bucket
        |]
  toWidget [julius|
    $(document).read(function () {
      $('#trans-table').columnDefs
    
    })
                  |]

renderTaxDetailTable startDate taxDetails = 
  [whamlet|
   <table *{datatable}>
    <thead>
      <tr>
        <th>Date
        <th> TransNo
        <th> TransType
        <th> Memo
        <th> netAmount
        <th>taxAmount
        <th>rate
        <th>Bucket
    <tbody>
      $forall detail <- taxDetails 
        $with old <- tdTranDate detail < startDate
          <tr :old:.bg-danger>
            <td :old:.text-danger>#{tshow $ tdTranDate detail }
            <td.text-right>#{tshow $ tdTransNo detail }
            <td.text-center>#{transactionIconSpan $ tdTransType detail }
            <td>#{fromMaybe "" $ tdMemo detail }
            <td.text-right>#{formatDouble' $ tdNetAmount detail }
            <td.text-right>#{formatDouble' $ tdTaxAmount detail }
            <td.text-right>#{formatDouble' $ tdRate detail }%
            <td>#{tdBucket detail}
          |]

-- | Do pivot table  Bucket/Rate
renderBucketTable :: Set (Bucket, Entity FA.TaxType) -- ^ Combination bucket/rate used by config
                  -> Map (Bucket, Entity FA.TaxType) TaxSummary -> Widget
renderBucketTable bucket'rates bucketMap = let
  bucketMap' = unionWith (<>) bucketMap $ mapFromList $ (map (, TaxSummary 0 0 ) (toList bucket'rates))
  buckets :: [(Bucket, [TaxSummary])]
  buckets = Map.toList $ groupAsMap (fst . fst) (return . snd)$ Map.toList bucketMap'
  taxTypes :: [(Entity FA.TaxType, [TaxSummary])]
  taxTypes = Map.toList $ groupAsMap (snd . fst) (return . snd) $  Map.toList bucketMap'
  confBuckets = setFromList $ map fst (toList bucket'rates) :: Set Bucket
  in [whamlet|
   <table *{datatable -. "table-striped"}>
     <thead>
       <tr>
         <th>Bucket
         $forall (Entity (FA.TaxTypeKey taxId) FA.TaxType{..}, _) <- taxTypes
           <th :taxTypeInactive:.text-danger :taxTypeInactive:.bg-danger data-toggle='tooltip' :taxTypeInactive:title="TaxType is Inactive">
              <div>##{tshow taxId} #{taxTypeName}
              <div>#{formatDouble' taxTypeRate}%
         <th>Total
     <tbody>
       $forall (bucket, txs) <- buckets
         $with bucketIn <-  notElem bucket confBuckets
          <tr :bucketIn:.bg-danger :bucketIn:.text-danger data-toggle="tooltip" :bucketIn:title="Bucket #{bucket} not in current report configuration">
              <td>#{bucket}
            $forall (taxType@(Entity _ FA.TaxType{..}), _) <- taxTypes
              $case lookup (bucket, taxType) bucketMap
                $of Just tx
                  $with invalidBucket <- notElem (bucket, taxType) bucket'rates
                    <td :invalidBucket:.bg-danger :taxTypeInactive:.bg-danger data-toggle='tooltip' :invalidBucket:title="Bucket/Tax combination not part of the report configuration">
                        ^{renderTaxSummary (Just taxTypeRate) tx}
                $of _
                  <td>
           $# Right margin
           <td>
               ^{renderTaxSummary Nothing $ mconcat txs}
       $# Bottom margin
       <tfoot>
         <tr>
           <th> Total
           $forall (Entity _ FA.TaxType{..}, txs ) <- taxTypes
             <td>
                ^{renderTaxSummary (Just $ taxTypeRate) $ mconcat txs}
           $with allTxs <- toList bucketMap
             <td>
               ^{renderTaxSummary Nothing $ mconcat allTxs}
          |]

renderTaxSummary rateM TaxSummary{..} = let
  -- check that the rate if given matches the transaction
  klass = case rateM of
    Nothing -> "" :: Text
    Just rate -> case netAmount * rate - taxAmount of
                    diff | diff < 1e-4 -> "" -- correct
                    diff | diff <= 1e-2 -> "bg-warning text-warning"
                    _ -> "bg-danger text-danger"
                   
  in [whamlet|
  <span class="#{klass}">
    <div.tax-amount.text-right>#{formatDouble' taxAmount}
    <div.net-amount.text-right>#{formatDouble' netAmount}
    <div.gross-amount.text-right>
      <span.guessed-value>#{formatDouble' $ taxAmount + netAmount}
|]

-- | Displays a table with the box values
renderBoxTable :: [(TaxBox, Double)] -> Widget
renderBoxTable box'amounts =
  let klass isNegative amount = case isNegative of
             Just True -> "positive-bad" :: Text
             Just False -> "negative-bad"
             Nothing -> ""
        <> " " <> case () of
                  _ | amount >  1e-2 -> "positive"
                  _ | abs amount <  1e-2 -> ""
                  _ -> "negative"

  in [whamlet|
  <table *{datatableNoPage}>
    <thead>
      <tr>
        <th>Box
        <th>Description
        <th>Amount
    $forall (TaxBox{..}, amount) <- box'amounts
      <tr class="#{klass tbShouldBeNegative amount}">
        <td>#{tbName}
        <td>#{fromMaybe "" tbDescription}
        <td>#{formatDouble' amount}
          |]

-- | Display a bucket table showing each box
-- which bucket as an impact on. To do
-- we calculate the value of each box
-- by setting a bucket to 1 and look at the value of the box.
renderBoxConfigCheckerTable :: Set (Bucket, Entity FA.TaxType) -> [TaxBox] ->  Widget
renderBoxConfigCheckerTable bucket'rates boxes =  let
  buckets :: [Bucket] 
  buckets = nub . sort . map fst $ toList bucket'rates
  rates :: [Entity FA.TaxType]
  rates = nub . sort .map snd $ toList bucket'rates
  mkTxs = [ TaxSummary 0 1 -- tax amount
          , TaxSummary 1 0  --net amount
          ]
  bucketMap0 :: Map Bucket TaxSummary
  bucketMap0 = mapFromList $ map  (, TaxSummary 0 0 ) buckets
  mkMap bucket tx = insertMap bucket tx bucketMap0 
  box'ids = zip boxes [1 ::Int ..]
  renderBoxFor mkTx bucket rate = let
    boxDetails =[ (tshow boxId, tbName, boxValue, up)
                | (TaxBox{..}, boxId) <- box'ids
                , let boxValue = computeBoxAmount  tbRule (mkMap bucket mkTx)
                , let up = if boxValue > delta
                           then "up"
                           else if boxValue < (negate delta)
                           then "down"
                           else "no"
                ]
    klasses = [ "box-" <> boxId  <> "-" <> up  | (boxId, boxName, value, up) <- boxDetails ]
    boxUps = length $ filter (== "up") [up | (_,_,_,up) <- boxDetails]
    boxDowns = length $ filter (== "down") [up | (_,_,_,up) <- boxDetails]
    iconFor "up" = "glyphicon-arrow-up boxup" :: Text
    iconFor "down" = "glyphicon-arrow-down boxdown"
    iconFor _ = "glyphicon-remove-sign nobox"
    taxFor0Rate = FA.taxTypeRate (entityVal rate) == 0 && mkTx == TaxSummary 0 1 
    delta = 1e-2
    partialFor v = abs v < 1 - delta && v > delta
    in if  (bucket, rate) `member` bucket'rates
       then
        [shamlet|
       <td.box :taxFor0Rate:.tax-for-0rate class=#{intercalate " " klasses} data-boxup=#{tshow boxUps} data-boxdown=#{tshow boxDowns}>
         $forall  (boxId, boxName, boxValue, up) <- boxDetails
           $with partial <- partialFor boxValue
            <span.glyphicon.box-selected :partial:.partial class="#{iconFor up} box-#{boxId} #{up}" data-toggle="tooltip" title="#{boxName} #{formatDouble' boxValue}" onClick="toggleBox(#{boxId});")>
               |]
       else
        [shamlet|
                    <td.text-center> -
                    |]
  colWidth = tshow (100 / (fromIntegral $ 1 + length rates)) <> "%"
  bucketTable = [whamlet|
   <table.table.table-border.table-hover>
     <thead>
       <tr>
         <th>Bucket
         $forall (Entity (FA.TaxTypeKey taxId) rate) <- rates
           <th style="width:#{colWidth}">
            <div>##{tshow taxId} #{FA.taxTypeName rate}
            <div>#{formatDouble' $ FA.taxTypeRate rate}%
         $forall _ <- drop 1 mkTxs
           <th>
     <tbody>
       $forall bucket <- buckets
         $forall (first, mkTx) <- zip (True : repeat False) mkTxs
          <tr>
           $if first
            <td rowspan="#{tshow $ length mkTxs}">#{bucket}
           $forall rate <- rates
                #{renderBoxFor mkTx bucket rate}
          |]
  control = [whamlet|
   <table.table.table-hover.table-striped.table-border>
     <thead>
       <tr>
        <th>Boxes
          <input.master-box type=checkbox onChange="toggleAll()" checked>
     <tbody>
       $forall (TaxBox{..}, boxId) <- box'ids
         <tr>
          <td>
            <label>#{tbName}
            <input.box.box-#{boxId} type=checkbox onchange="changeBox('#{tshow boxId}')" checked>
  |]
  controlJS = [julius|
    function changeBox(boxName) {
       var state = this.event.target.checked;
       if (state) {
        $('td.box-'+boxName+'-up').attr('data-boxup', increment);
        $('td.box-'+boxName+'-down').attr('data-boxdown', increment);
        $('span.box-'+boxName).addClass('box-selected');

        $('input.master-box:not(:checked)').prop('checked', true);
       } else {
        $('td.box-'+boxName+'-up').attr('data-boxup', decrement);
        $('td.box-'+boxName+'-down').attr('data-boxdown', decrement);
        $('span.box-'+boxName).removeClass('box-selected');
        //if all unselct
        if($('input.box:checked').length == 0)
          $('input.master-box:checked').prop('checked', false);
       }

    }

    function increment(el, value) {
       return parseInt(value)+1;
    }
    function decrement(el, value) {
       return Math.max(0, parseInt(value)-1);
    }

    function toggleBox(idx) {
      $('input[type=checkbox].box-'+idx).click();
    
    }
   function toggleAll() {
     var state = this.event.target.checked;
     if (state)
        $('input.box[type=checkbox]:not(:checked)').click();
     else
       $('input.box[type=checkbox]:checked').click();
     
     
   
   }
   $(document).ready(function () { 
   // reset all checkbox 
      $('input[type=checkbox]').prop('checked', true);
      // $('td.box').attr('data-boxup', "0");
      // $('td.box').attr('data-boxdown', "0");
      })
              |]
  bucketCSS = [cassius|
td.box
  # mute span
  color: gray
  span.glyphicon
    cursor: pointer
td.box[data-boxup="1"][data-boxdown="0"]
  background: lightgreen
  & span.glyphicon.box-selected
    color: green
  & span.glyphicon.box-selected.partial
    background: green
    color: white
  color: gray
  
td.box[data-boxup="0"][data-boxdown="1"]
  background: lightblue
  & span.glyphicon.box-selected
    color: blue
  & span.glyphicon.box-selected.partial
    background: blue
    color: white
  color: gray
td.box[data-boxup="0"][data-boxdown="0"]
  background: pink
  & span.glyphicon.nobox
    color: red
  color: gray

  &.tax-for-0rate
    background: whitesmoke
    & span.glyphicon.nobox
      color: gray

td.box
  background: peachpuff
  & span.glyphicon.box-selected.nobox
    color: gray
  & span.glyphicon.box-selected
    color: orange
    &.partial
      background: orange
      color: white
  color: gray
                          |]

    
  in [whamlet|
      <div.row>
        <div.col-md-2 id=control-table >
          ^{control}
        <div.col-md-10 id=bucket-table >
          ^{bucketTable}
          |] <> toWidget controlJS <> toWidget bucketCSS
     


-- * DB

loadReport :: Int64 -> SqlHandler (Entity TaxReport)
loadReport key = do
  let rId = TaxReportKey (fromIntegral key)
  report <- getJust rId

  return $ Entity rId report

-- loadReportBoxes :: Key TaxReportKey -> SqlHandler [Entity TaxReportBoxes]
-- loadReportBoxes :: Key TaxReportKey -> SqlHandler [Entity TaxReportBoxes]

-- | Count how many tax transaction are within the tax report date range
-- and haven't been accounted for.
-- collectPendingTransTaxDetails ::  TaxReport -> SqlHandler [TaxReportDetail]
countPendingTransTaxDetails :: TaxReport -> SqlHandler (Int, Int)
countPendingTransTaxDetails TaxReport{..} =  do
  [Single inRange] <- runTaggedSql (buildPendingTransTaxDetailsQuery selectCount
                                                              taxReportType
                                                              (Just taxReportStart)
                                                              taxReportEnd
                            ) 
  [Single before] <- runTaggedSql (buildPendingTransTaxDetailsQuery selectCount
                                                             taxReportType
                                                             Nothing
                                                             ((-1) `addDays` taxReportStart)
                           )

  return (before, inRange)                                                                                     

buildPendingTransTaxDetailsQuery :: Tagged e Text -> Text -> Maybe Day -> Day -> (Tagged e Text, [PersistValue])
buildPendingTransTaxDetailsQuery (Tagged selectQuery) reportType startDate endDate = 

  let sql0 =  selectQuery <> " FROM 0_trans_tax_details fad"
          <>  " LEFT JOIN fames_tax_report_detail rd ON (tax_trans_detail_id = fad.id) "
          <>  " LEFT JOIN fames_tax_report r ON(r.tax_report_id =  rd.tax_report_id AND type = ?) "
          <> " WHERE (rd.tax_report_id is NULL OR ("
          <> "           abs (fad.net_amount * ex_rate - rd.net_amount) > 1e-4 "
          <> "                OR    abs (fad.amount * ex_rate - rd.tax_amount) > 1e-4 "
          <> "               )"
          <> "       ) AND fad.tran_date <= ?"
      p0 = [ toPersistValue $ reportType
           , toPersistValue $ endDate
           ]
  in first Tagged $ case startDate of
                Nothing -> (sql0, p0)
                Just start -> (sql0 <> " AND fad.tran_date >= ? ", p0 <> [toPersistValue start])

buildCollectedTaxDetailsQuery :: Tagged e Text -> Key TaxReport -> (Tagged e Text, [PersistValue])
buildCollectedTaxDetailsQuery (Tagged selectQuery) reportKey = 
  let sql0 =  selectQuery <> " FROM 0_trans_tax_details fad"
          <>  " JOIN fames_tax_report_detail rd ON (tax_trans_detail_id = fad.id) "
          <> " WHERE tax_report_id = ? "
          <> "      AND (("
          <> "           abs (fad.net_amount * ex_rate - rd.net_amount) <= 1e-4 "
          <> "                AND    abs (fad.amount * ex_rate - rd.tax_amount) <= 1e-4 "
          <> "               )"
          <> "       ) "
      p0 = keyToValues reportKey
           
  in (Tagged sql0, p0)

selectTt'MRd :: Tagged (Entity FA.TransTaxDetail, Maybe (Entity TaxReportDetail)) Text
selectTt'MRd = Tagged "SELECT fad.*, rd.* /* ?? */ /* ?? */ " -- hack to use 

selectTt'Rd :: Tagged (Entity FA.TransTaxDetail, Entity TaxReportDetail) Text
selectTt'Rd = Tagged "SELECT fad.*, rd.* /* ?? */ /* ?? */ " -- hack to use 

selectCount :: Tagged (Single Int) Text
selectCount = Tagged "SELECT count(*) "

runTaggedSql :: RawSql r => (Tagged r Text, [PersistValue]) -> SqlHandler [r]
runTaggedSql (Tagged sql, params) = rawSql sql params

loadPendingTaxDetails :: Rule -> Entity TaxReport -> Handler [TaxDetail]
loadPendingTaxDetails taxRule (Entity reportKey TaxReport{..}) =
  loadTaxDetail (uncurry $ taxDetailFromDetailM reportKey (applyTaxRule taxRule . faTransToRuleInput))
                (buildPendingTransTaxDetailsQuery selectTt'MRd
                                                  taxReportType
                                                  Nothing
                                                  taxReportEnd
                )

loadCollectedTaxDetails (Entity reportKey TaxReport{..})  =
  loadTaxDetail (uncurry taxDetailFromDetail)
                (buildCollectedTaxDetailsQuery selectTt'Rd
                                                    reportKey 
                )

-- | Load transactions Report details For datatable
getGLTaxReportDetailsR :: Int64 -> Handler Value
getGLTaxReportDetailsR key = do
  (rows, reportType) <- runDB $  do
    (Entity _ TaxReport{..}) <- loadReport key

    let (Tagged sql, params) = buildPendingTransTaxDetailsQuery selectQ taxReportType Nothing taxReportEnd
        selectQ = "SELECT fad.* /* ?? */ " -- hack to use 
        -- we need to order them to get a consistent paging
        order = " ORDER BY fad.tran_date, fad.id"
        paging = " LIMIT 50000"
    r <- rawSql (sql <> order <> paging) params
    return (r, taxReportType)
  TaxReportSettings{..} <- unsafeGetReportSettings reportType
  let _types = rows :: [Entity FA.TransTaxDetail]
      _report0 = TaxReportKey 0
      showType = showShortTransType :: FATransType -> Text
      toj (Entity _ trans@FA.TransTaxDetail{..}) = [ toJSON transTaxDetailTranDate
                                             , toJSON transTaxDetailTransNo
                                             , toJSON ((showType . toEnum ) <$> transTaxDetailTransType)
                                             , toJSON transTaxDetailMemo
                                             , toJSON transTaxDetailNetAmount
                                             , toJSON transTaxDetailAmount
                                             , toJSON transTaxDetailRate
                                             , toJSON (applyTaxRule rules $ faTransToRuleInput trans)
                                             ]

  returnJson $ object [ "data" .= (map toj rows) ]
  
loadTaxDetail :: RawSql e 
              => (e -> Either Text TaxDetail)
              -> (Tagged e Text, [PersistValue])
              -> Handler [TaxDetail]
loadTaxDetail mkDetail query  = do
  rows <- runDB $ runTaggedSql $ query
  case mapM mkDetail rows of
    Left err -> do
       setError "Inconsistent DB contact your administrator"
       error (unpack err)
    Right details -> return details


-- * Util
-- | Return the setting corresponding to a report name.
-- Normally, all the function calling should have been given
-- a valid report name. Unless the user is typing randorm url.
-- in which case it is legitimate  to raise an error ;-)
unsafeGetReportSettings :: Text -> Handler TaxReportSettings
unsafeGetReportSettings name = do
  r <- getReportSettings name
  case r of
    Nothing -> error $ unpack $ "Can't tax settings for " <> name
    Just settings -> return settings

getReportSettings :: Text -> Handler (Maybe TaxReportSettings)
getReportSettings name = do
  settings <- appTaxReportSettings <$> getsYesod appSettings
  return $ lookup name settings
  

formatDouble' = F.sformat commasFixed

-- | Get the list of all  Bucket/rate configuration from the config
-- (and the rate in the database)
getBucketRateFromConfig :: Entity TaxReport -> Handler (Set (Bucket, Entity FA.TaxType ))
getBucketRateFromConfig report = do
  settings <- unsafeGetReportSettings (taxReportType $ entityVal report)

  taxEntities <- runDB $ selectList [FA.TaxTypeInactive ==. False] []
  return $ computeBucketRates (rules settings) $ mapFromList [ (key, e)
                                                             | (Entity key e) <- taxEntities
                                                             ]
  


-- * Rule
faTransToRuleInput :: FA.TransTaxDetail -> RuleInput
faTransToRuleInput FA.TransTaxDetail{..} = let
  riTransType = maybe (error "DB Problem") toEnum transTaxDetailTransType
  riEntity = Nothing
  riTaxType = fromIntegral $ transTaxDetailTaxTypeId
  riTaxRate = transTaxDetailRate -- 1% = 1
  in RuleInput{..}


computeBoxes :: Set (Bucket, Entity FA.TaxType) -> Map (Bucket, Entity FA.TaxType) TaxSummary -> [TaxBox] -> [(TaxBox, Double )]
computeBoxes bucket'rates bucketMap1 boxes = let
  -- initialize all bucket with 0
  bucketMap0 = mapFromList $ map (, mempty) (toList bucket'rates)
  bucketMap = unionWith (<>) bucketMap1 bucketMap0
  buckets = groupAsMap (fst . fst) snd $ mapToList bucketMap
  in [ (box, computeBoxAmount (tbRule box) buckets) | box <- boxes ]


