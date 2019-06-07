module Handler.GL.TaxReport
( getGLTaxReportsR
, postGLNewTaxReportR 
, getGLTaxReportR
, postGLTaxReportR
, getGLTaxReportDetailsR
, postGLTaxReportCollectDetailsR
, postGLTaxReportRejectDetailsR
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
  (key, TaxReport{..}) <- runDB $ do
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
    return (key, TaxReport{..})
  
  setSuccess "Report created sucessfully"
  let newRoute =GLR $ GLTaxReportR (fromSqlKey key) Nothing 
  pushLinks ("View tax report " <> taxReportReference) newRoute []
  -- collect details
  postGLTaxReportCollectDetailsR (fromSqlKey key) >>= sendResponseStatus created201
      -- getGLTaxReportR (fromSqlKey key) Nothing >>= sendResponseStatus created201

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
     
postGLTaxReportRejectDetailsR :: Int64 -> Handler Html
postGLTaxReportRejectDetailsR key = do
  let rId = TaxReportKey (fromIntegral key)
  runDB $ deleteWhere [TaxReportDetailReport ==. rId]
         >> deleteWhere [TaxReportBoxReport ==. rId]
  setSuccess "Transaction tax details successfully rejected"
  getGLTaxReportR key (Just TaxReportCollectedView) >>= sendResponseStatus created201


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

rejectButtonForm :: Key TaxReport -> Widget
rejectButtonForm key = [whamlet|
  <form method=POST action="@{GLR $ GLTaxReportRejectDetailsR $ fromSqlKey key}">
    <button.btn.btn-danger type="submit"> Reject
                            |]

renderReportView :: Rule -> Entity TaxReport -> TaxReportViewMode ->  Handler Widget
renderReportView rule report mode = do
  settings <- unsafeGetReportSettings (taxReportType $ entityVal report)
  bucket'rates <- getBucketRateFromConfig report
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  personName <- entityNameH False

  taxMap <- runDB $ loadTaxTypeMap
  let boxValues :: [(TaxBox, TaxDetail -> Maybe Double)]
      boxValues = [ (box, f)
                  | box <- (boxes settings)
                  , let f detail = either (const Nothing) Just $ computeBox bucket'rates 
                                              (reportDetailToBucketMap taxMap $ tdReportDetail detail) 
                                              box
                  ]

  let urlFn = urlForFA faURL
  view <- case mode of
            TaxReportPendingView -> do
              details <- loadPendingTaxDetails rule report
              return $ renderTaxDetailTable urlFn personName boxValues (taxReportStart $ entityVal report) details
                     >> collectButtonForm (entityKey report)
            TaxReportCollectedView -> do
              details <- loadCollectedTaxDetails report
              return $ renderTaxDetailTable urlFn personName boxValues (taxReportStart $ entityVal report) details
                     >> rejectButtonForm (entityKey report)
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

renderTaxDetailTable :: (FATransType -> Int -> Text) -- ^ Url
                     -> (FATransType -> Maybe Int64 -> Text) -- ^ Name
                     -> [(TaxBox, TaxDetail -> Maybe Double) ]
                     -> Day -> [TaxDetail] -> Widget
renderTaxDetailTable urlFn personName boxes startDate taxDetails =  let
  hasBoxes = not $ null boxes 
  in [whamlet|
   <table *{datatable}>
    <thead>
      <tr>
        <th>Date
        <th> TransNo
        <th> TransType
        <th> Memo
        <th> Person
        <th> netAmount
        <th>taxAmount
        <th>rate
        <th>Bucket
        $if hasBoxes
          <th>Boxes
        $forall (TaxBox{..}, _) <- boxes
          <th.none>#{tbName} 
    <tbody>
      $forall detail <- taxDetails 
        $with old <- tdTranDate detail < startDate
          <tr :old:.bg-danger>
            <td :old:.text-danger>#{tshow $ tdTranDate detail }
            <td.text-right>#{transNoWithLink urlFn "" (tdTransType detail) (tdTransNo detail) }
            <td.text-center>#{transactionIconSpan $ tdTransType detail }
            <td>#{maybe "" decodeHtmlEntities $ tdMemo detail }
            <td>#{personName (tdTransType detail) (tdEntity detail)}
            <td.text-right>#{formatDoubleWithSign $ tdNetAmount detail }
            <td.text-right>#{formatDoubleWithSign $ tdTaxAmount detail }
            <td.text-right>#{formatDouble' $ (*) (tdRate detail) 100 }%
            <td>#{tdBucket detail}
            $if hasBoxes
              <td>
                $forall  (box, valueF) <- boxes
                  $maybe value <- valueF detail
                      $if value > 0.01
                       <span.badge.up data-toggle="tooltip" title="#{formatDouble' value}">
                         #{tbName box}
                      $else
                        $if value < -0.01
                          <span.badge.down data-toggle="tooltip" title="#{formatDouble' value}">
                            #{tbName box}
            $forall (box, boxValue) <- boxes
              #{renderBoxForDetail box (boxValue detail)}
          |] <> toWidget bucketCSS

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

-- | Compute the contribution of a detail for a box
-- and display it if needed. Also add a hidden field
-- allowing to search transaction by box
renderBoxForDetail box valuem =  case valuem of
  Just value | abs value > 1e-2 -> [shamlet|
      <td>
        #{formatDouble' value}
        <span.hidden>box:#{tbName box}
                        |]
  _ -> [shamlet|<td>|]


-- | Displays a table with the box values
renderBoxTable :: [(TaxBox, Double)] -> Widget
renderBoxTable box'amounts =
  let klass shouldBe amount = case shouldBe of
             Just LT -> "positive-bad" :: Text
             Just EQ -> "negative-bad positive-bad"
             Just GT -> "negative-bad"
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
      <tr class="#{klass tbShouldBe amount}">
        <td>#{tbName}
        <td>#{fromMaybe "" tbDescription}
        <td.text-right>#{formatDouble' amount}
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
                , let boxValue = either (error . unpack) id $ computeBoxAmount  tbRule (mkMap bucket mkTx)
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
  in [whamlet|
      <div.row>
        <div.col-md-2 id=control-table >
          ^{control}
        <div.col-md-10 id=bucket-table >
          ^{bucketTable}
          |] <> toWidget controlJS <> toWidget bucketCSS
     


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

span.badge.up
  color: white
  background: #{greenBadgeBg}
span.badge.down
  color: white
  background: #{blueBadgeBg}
                          |]

    
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
          <>  " LEFT JOIN 0_debtor_trans AS dt ON (fad.trans_no = dt.trans_no AND fad.trans_type = dt.type)"
          <>  " LEFT JOIN 0_supp_trans AS st ON (fad.trans_no = st.trans_no AND fad.trans_type = st.type)"
          <> " WHERE (rd.tax_report_id is NULL OR ("
          <> "           abs (fad.net_amount * ex_rate * " <> transSignSQL <> " - rd.net_amount) > 1e-4 "
          <> "                OR    abs (fad.amount * ex_rate * " <> transSignSQL <> "- rd.tax_amount) > 1e-4 "
          <> "               )"
          <> "       ) AND fad.tran_date <= ?"
          <> "       AND fad.trans_type != " <> tshow (fromEnum ST_CUSTDELIVERY) <> " "
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
          <>  " LEFT JOIN 0_debtor_trans AS dt ON (fad.trans_no = dt.trans_no AND fad.trans_type = dt.type)"
          <>  " LEFT JOIN 0_supp_trans AS st ON (fad.trans_no = st.trans_no AND fad.trans_type = st.type)"
          <> " WHERE tax_report_id = ? "
          <> "      AND (("
          <> "           abs (fad.net_amount * ex_rate * " <> transSignSQL <> "- rd.net_amount) <= 1e-4 "
          <> "                AND    abs (fad.amount * ex_rate * " <> transSignSQL <> "- rd.tax_amount) <= 1e-4 "
          <> "               )"
          <> "       ) "
          <> "       AND fad.trans_type != " <> tshow (fromEnum ST_CUSTDELIVERY) <> " "
      p0 = keyToValues reportKey
           
  in (Tagged sql0, p0)


-- | FA store all transactions detail with a positive amount
-- To match the report detail, transaction needs to be reversed (or not)
-- so that the amount is positive for output (tax to pay)
transSignSQL :: Text
transSignSQL = "IF(fad.trans_type IN ( " <> inputTrans <> "), -1, 1)" where
  inputTrans = inTypes [ ST_CUSTCREDIT, ST_CUSTPAYMENT -- refund
                       , ST_SUPPINVOICE -- expenditure ST_BANKPAYMENT already -ve
                       , ST_JOURNAL -- expenditures too
                       ]

selectTt'MRd :: Tagged (Entity FA.TransTaxDetail, Maybe (Entity TaxReportDetail), Single (Maybe Int64)) Text
selectTt'MRd = let
  faSelect = "`fad`.`id`, `fad`.`trans_type`, `fad`.`trans_no`, `fad`.`tran_date`, `fad`.`tax_type_id`, `fad`.`rate`, `fad`.`ex_rate`, `fad`.`included_in_price`, "
    <> transSignSQL <> " * `fad`.`net_amount`, "
    <> transSignSQL <> " * `fad`.`amount`, `fad`.`memo` "
  in Tagged $ "SELECT " <> faSelect <>  ", rd.* /* ?? */ /* ?? */, COALESCE(debtor_no, supplier_id) " -- hack to use 

selectTt'Rd :: Tagged (Entity FA.TransTaxDetail, Entity TaxReportDetail, Single (Maybe Int64)) Text
selectTt'Rd = retag  selectTt'MRd

selectCount :: Tagged (Single Int) Text
selectCount = Tagged "SELECT count(*) "

runTaggedSql :: RawSql r => (Tagged r Text, [PersistValue]) -> SqlHandler [r]
runTaggedSql (Tagged sql, params) = rawSql sql params

loadPendingTaxDetails :: Rule -> Entity TaxReport -> Handler [TaxDetail]
loadPendingTaxDetails taxRule (Entity reportKey TaxReport{..}) =
  loadTaxDetail (uncurry3Single $ taxDetailFromDetailM reportKey (\t p -> applyTaxRule taxRule $ faTransToRuleInput t p))
                (buildPendingTransTaxDetailsQuery selectTt'MRd
                                                  taxReportType
                                                  Nothing
                                                  taxReportEnd
                )

loadCollectedTaxDetails (Entity reportKey TaxReport{..})  =
  loadTaxDetail (uncurry3Single taxDetailFromDetail)
                (buildCollectedTaxDetailsQuery selectTt'Rd
                                                    reportKey 
                )

uncurry3Single f (trans , detail,  Single personm) = f trans detail personm
-- | Load 
loadTaxTypeMap = do
  taxTypes <- selectList [] []
  return $  mapFromList $ map (fanl entityKey) taxTypes

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
  

-- | Load transactions Report details For datatable
getGLTaxReportDetailsR :: Int64 -> Handler Value
getGLTaxReportDetailsR key = do
  error "Not implemented"
  -- 
  -- (rows, reportType) <- runDB $  do
  --   (Entity _ TaxReport{..}) <- loadReport key

  --   let (Tagged sql, params) = buildPendingTransTaxDetailsQuery selectQ taxReportType Nothing taxReportEnd
  --       selectQ = "SELECT fad.* /* ?? */ " -- hack to use 
  --       -- we need to order them to get a consistent paging
  --       order = " ORDER BY fad.tran_date, fad.id"
  --       paging = " LIMIT 50000"
  --   r <- rawSql (sql <> order <> paging) params
  --   return (r, taxReportType)
  -- TaxReportSettings{..} <- unsafeGetReportSettings reportType
  -- let _types = rows :: [Entity FA.TransTaxDetail]
  --     _report0 = TaxReportKey 0
  --     showType = showShortTransType :: FATransType -> Text
  --     toj (Entity _ trans@FA.TransTaxDetail{..}) = [ toJSON transTaxDetailTranDate
  --                                            , toJSON transTaxDetailTransNo
  --                                            , toJSON ((showType . toEnum ) <$> transTaxDetailTransType)
  --                                            , toJSON transTaxDetailMemo
  --                                            , toJSON transTaxDetailNetAmount
  --                                            , toJSON transTaxDetailAmount
  --                                            , toJSON transTaxDetailRate
  --                                            , toJSON (applyTaxRule rules $ faTransToRuleInput trans Nothing)
  --                                            ]

  -- returnJson $ object [ "data" .= (map toj rows) ]
  
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

formatDoubleWithSign amount = [shamlet|<span :negative:.text-danger>#{formatDouble' amount}|]
  where negative = amount < -1e-2


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
faTransToRuleInput :: FA.TransTaxDetail -> Maybe Int64 -> RuleInput
faTransToRuleInput FA.TransTaxDetail{..} riEntity = let
  riTransType = maybe (error "DB Problem") toEnum transTaxDetailTransType
  riTaxType = fromIntegral $ transTaxDetailTaxTypeId
  riTaxRate = transTaxDetailRate -- 1% = 1
  riAmount = transTaxDetailAmount
  in RuleInput{..}


computeBoxes :: Set (Bucket, Entity FA.TaxType) -> Map (Bucket, Entity FA.TaxType) TaxSummary -> [TaxBox] -> [(TaxBox, Double )]
computeBoxes bucket'rates bucketMap boxes = let
  values = map (computeBox bucket'rates bucketMap) boxes
  in case lefts values of
    [] -> zip boxes $ rights values
    errors -> error $ unpack $ intercalate "\n" errors

computeBox :: Set (Bucket, Entity FA.TaxType) -> Map (Bucket, Entity FA.TaxType) TaxSummary -> TaxBox -> Either Text Double 
computeBox bucket'rates bucketMap1 box = let
  -- initialize all bucket with 0
  bucketMap0 = mapFromList $ map (, mempty) (toList bucket'rates)
  bucketMap = unionWith (<>) bucketMap1 bucketMap0
  buckets = groupAsMap (fst . fst) snd $ mapToList bucketMap
  in computeBoxAmount (tbRule box) buckets

reportDetailToBucketMap :: Map FA.TaxTypeId (Entity FA.TaxType)
                        -> TaxReportDetail
                        -> Map (Bucket, Entity FA.TaxType) TaxSummary
reportDetailToBucketMap taxTypeMap TaxReportDetail{..} = let
  taxSummary = TaxSummary taxReportDetailNetAmount taxReportDetailTaxAmount
  Just taxType = lookup (FA.TaxTypeKey taxReportDetailFaTaxType) taxTypeMap
  key = (taxReportDetailBucket, taxType)
  in singletonMap key taxSummary

