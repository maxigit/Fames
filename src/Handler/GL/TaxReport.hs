module Handler.GL.TaxReport
( getGLTaxReportsR
, postGLNewTaxReportR 
, getGLTaxReportR
, postGLTaxReportR
, getGLTaxReportDetailsR
, postGLTaxReportCollectDetailsR
, postGLTaxReportRejectDetailsR
, postGLTaxReportReopenR
, postGLTaxReportPreSubmitR
, postGLTaxReportSubmitR
, getGLTaxReportOAuthR
, getGLTaxReportStatusesR
, getGLTaxReportValidateFraudPreventionHeadersR
) where
import Import hiding(RuleInput)
import GL.TaxReport.Types
import GL.TaxReport.Settings
import GL.TaxReport
import Handler.GL.TaxReport.Types
import Handler.GL.TaxReport.Processor
import Handler.GL.TaxReport.HMRC
import Handler.GL.TaxReport.Common
import GL.Utils
import Data.Time (addDays)
import Database.Persist.Sql (fromSqlKey)
import qualified FA as FA
import qualified Data.Map as Map
import Data.List(nub)
import Util.Decimal

-- * Handler 

-- | Display the list of the available tax reports
{-# NOINLINE getGLTaxReportsR #-}
getGLTaxReportsR :: Handler Html
getGLTaxReportsR = do
  settings <- appTaxReportSettings <$> getsYesod appSettings
  name'tables <- forM (mapToList settings) renderReportList
  let panel_ name tableW = infoPanel' (Just $ "tax-report-panel-" <> name) name (tableW >> newW)
        where newW = [whamlet|
       <div.well>
         <form method=POST action=@{GLR $ GLNewTaxReportR name}>
           <button.btn.btn-danger>New
                   |]
  defaultLayout [whamlet|
   <h1.jumbotron> Tax report
   $forall (name, tableW) <- name'tables
     ^{panel_ name tableW }
     |]

-- | Create a report for the given type
{-# NOINLINE postGLNewTaxReportR #-}
postGLNewTaxReportR :: Text -> Handler Html
  
postGLNewTaxReportR name = do
  (settings, __processor) <- unsafeGetReportSettings name
  (key, TaxReport{..}) <- runDB $ do
    lasts <- selectList [TaxReportType ==. name] [Desc TaxReportStart]
    let taxReportStart = case lasts of
          (Entity _ report_: _) -> 1 `addDays` (Import.taxReportEnd report_)
          [] -> startDate settings
        taxReportType = name
        taxReportReference = pack $ formatTime defaultTimeLocale (unpack $ referenceFormat settings) taxReportStart
        taxReportEnd = (-1) `addDays` calculateDate (nextPeriod settings) taxReportStart
        taxReportStatus = Pending
        taxReportSubmittedAt = Nothing
        taxReportExternalReference = Nothing
        taxReportExternalData = Nothing

        report = TaxReport{..}


    key <- insert report
    return (key, report)
  
  setSuccess "Report created sucessfully"
  -- collect details
  postGLTaxReportCollectDetailsR (fromSqlKey key) >>= sendResponseStatus created201
      -- getGLTaxReportR (fromSqlKey key) Nothing >>= sendResponseStatus created201

{-# NOINLINE getGLTaxReportR #-}
getGLTaxReportR :: Int64 -> Maybe TaxReportViewMode -> Handler Html
getGLTaxReportR key mode = do
  report <- runDB $ loadReport key
  reportSettings'processor  <- getReportSettings (taxReportType $ entityVal report)
  let
    reportRules = maybe defaultRule (rules. fst) reportSettings'processor
    newRoute =GLR $ GLTaxReportR key Nothing 
  (before, withinPeriod)  <- runDB $ countPendingTransTaxDetails (entityVal report)
  view <- renderReportView reportRules report (defaultViewMode before withinPeriod mode)
  when (before > 0) $ do
    setWarning "Some Transactions part of the  have been entered or modified before current report start date"

  pushLinks ("View tax report " <> taxReportReference (entityVal report)) newRoute []
  defaultLayout $ renderReportHeader report (before, withinPeriod) >> view

-- | Computes which view to use if not are provided.
-- Depends on the state of the report.
defaultViewMode :: Int -> Int -> Maybe TaxReportViewMode -> TaxReportViewMode
defaultViewMode _ _ (Just mode) = mode
defaultViewMode before withinPeriod Nothing = case (before, withinPeriod) of
  (0, 0) -> TaxReportBoxesView
  _ -> TaxReportPendingView

{-# NOINLINE postGLTaxReportR #-}
postGLTaxReportR :: Int64 -> Maybe TaxReportViewMode -> Handler Html
postGLTaxReportR __key __modem = return "todo"


{-# NOINLINE postGLTaxReportCollectDetailsR #-}
postGLTaxReportCollectDetailsR :: Int64 -> Handler Html
postGLTaxReportCollectDetailsR key = do
  let rId = TaxReportKey (fromIntegral key)
  runDB $ do
    report <- getJust rId
    reportSettings'processor  <- lift $ getReportSettings (taxReportType report)
    let reportRules = maybe defaultRule (rules . fst) reportSettings'processor 
    details <- lift $ loadPendingTaxDetails reportRules (Entity rId report)
    -- delete existing one which need to be replaced
    mapM_ delete (mapMaybe tdExistingKey details )
    insertMany_  $ map tdDetailToSave $ filter (not . tdIsNull) details

  setSuccess "Transaction tax details collected successfully"
  getGLTaxReportR key (Just TaxReportCollectedView) >>= sendResponseStatus created201
     
{-# NOINLINE postGLTaxReportRejectDetailsR #-}
postGLTaxReportRejectDetailsR :: Int64 -> Handler Html
postGLTaxReportRejectDetailsR key = do
  let rId = TaxReportKey (fromIntegral key)
  runDB $ deleteWhere [TaxReportDetailReport ==. rId]
         >> deleteWhere [TaxReportBoxReport ==. rId]
  setSuccess "Transaction tax details successfully rejected"
  getGLTaxReportR key (Just TaxReportCollectedView) >>= sendResponseStatus created201


{-# NOINLINE postGLTaxReportReopenR #-}
postGLTaxReportReopenR :: Int64 -> Handler Html
postGLTaxReportReopenR key = do
  runDB $ do
    report <- loadReport key
    openReport report
  getGLTaxReportR key (Just TaxReportCollectedView) >>= sendResponseStatus created201
  
-- | Check that the report is ready to be submitted 
-- and ask the user confirmation
{-# NOINLINE postGLTaxReportPreSubmitR #-}
postGLTaxReportPreSubmitR :: Int64 -> Handler Html
postGLTaxReportPreSubmitR key = do
  report <- runDB $ loadReport key
  -- traceShowM ("REPORT SUBMIt", report)
  Just (__reportSettings, processor)  <- getReportSettings (taxReportType $ entityVal report)
  -- let reportRules = maybe defaultRule rules reportSettings
  (__before, withinPeriod)  <- runDB $ countPendingTransTaxDetails (entityVal report)
  
  _ <- forM (taxReportSubmittedAt (entityVal report)) (alreadySubmitted key)
  externalStatus <- checkExternalStatus processor report
  _ <- case externalStatus of
    Submitted submitted -> alreadySubmitted key submitted
    Ready -> return ""
    _ -> error $ "external VAT status inconsistent: " <> show externalStatus
    
  presubmitW <- preSubmitCheck processor report
  when (withinPeriod > 0) $ do
    let widget = dangerPanel "Tax report incomplete" [whamlet|
                        <p>Some transactions within the tax period haven't been collected.
                        <p>Please collect them before submitting the report  "
                        ^{continueButtonForm (GLTaxReportR key (Just TaxReportPendingView)) }
                        |]
    defaultLayout widget >>= sendResponseStatus preconditionFailed412

  when (taxReportStatus (entityVal report) == Pending ) $ closeReport processor report
  box'amounts <- runDB $ loadTaxBoxes processor AllBuckets (entityKey report)
  setInfo "The current tax report is ready to be submitted. Are you sure you want to proceed ?"
  defaultLayout [whamlet|
"    ^{renderBoxTable box'amounts}
     $maybe pre <- presubmitW
      ^{submitButtonForm pre (entityKey report)}
     ^{cancelSubmitButtonForm (entityKey report)}
                        |] 


alreadySubmitted :: Int64 -> UTCTime -> Handler Html
alreadySubmitted key submitted = do
    let widget = warningPanel "Tax Return already submitted"
                   [whamlet|<p>Tax return has already been submitted on the #{tshow submitted}  
                           <p> Modification would have to be sent in the next return.
                           ^{continueButtonForm (GLTaxReportR key (Just TaxReportBoxesView) )}
              |]
    (defaultLayout widget) >>= sendResponseStatus preconditionFailed412
  

-- | Submit the report
{-# NOINLINE postGLTaxReportSubmitR #-}
postGLTaxReportSubmitR :: Int64 -> Handler TypedContent
postGLTaxReportSubmitR key = do
  report <- runDB $ loadReport key
  Just (__reportSettings, processor)  <- getReportSettings (taxReportType $ entityVal report)
  result <- submitReturn processor report
  case result of
    Left err ->  do
      setError (toHtml err)
      toTypedContent <$> postGLTaxReportPreSubmitR key
    Right (TaxReport{..}, contentm ) -> do
        runDB $ update (entityKey report) [ TaxReportSubmittedAt =. taxReportSubmittedAt
                                          , TaxReportExternalReference =. taxReportExternalReference 
                                          , TaxReportExternalData =. taxReportExternalData
                                          ]
        setSuccess "Report has been succesfully submitted"
        case contentm of
          Nothing -> toTypedContent <$> getGLTaxReportsR  >>= sendResponseStatus created201
          Just content -> return content

{-# NOINLINE getGLTaxReportStatusesR #-}
getGLTaxReportStatusesR :: Text -> Handler Html
getGLTaxReportStatusesR name = do
  (__settings, processor) <- unsafeGetReportSettings name
  w <- displayExternalStatuses processor name
  defaultLayout $ infoPanel name w

-- ** HMRC OAuth2 
-- | Process The authorization code from HMRC
{-# NOINLINE getGLTaxReportOAuthR #-}
getGLTaxReportOAuthR :: Handler TypedContent
getGLTaxReportOAuthR = do
  codem <- lookupGetParam "code"
  statem <- lookupGetParam "state"
  case statem >>= readMay of
    Nothing -> respondAuthError 
    Just (reportType, routem) -> do
      (settings, _) <- unsafeGetReportSettings reportType
      case codem of
        Just code | HMRCProcessor params <- processor settings -> do
          setHMRCAuthorizationCode reportType params $ AuthorizationCode code
          -- redirect to presubmit page
          case routem of
            Nothing -> toTypedContent <$> getGLTaxReportsR
            Just (GLR (GLTaxReportSubmitR reportId)) ->  postGLTaxReportSubmitR reportId
            Just (GLR (GLTaxReportPreSubmitR reportId)) ->  toTypedContent <$> postGLTaxReportPreSubmitR reportId
            Just (route) -> toTypedContent <$> (redirect (route :: Route App) :: Handler Html) --  we use redirect
            -- to be sure we the result go through normal authorization
        _ -> do -- 
          respondAuthError 
      
respondAuthError  = do
  error_code <- lookupGetParam "error_code"
  error_param <- lookupGetParam "error"
  error_description<- lookupGetParam "error_description" 
  let
    table = [whamlet|
    <table.table.table-striped.table-hover>
      <tr>
        <th>Error
        <td>#{tshowM error_param}
      <tr>
        <th>Error Code
        <td>#{tshowM error_code}
      <tr>
        <th>Description
        <td>#{tshowM error_description}
                |]
    w = dangerPanel "OAuth2 error" [whamlet|
              <p>Can't authorize the application to use HMRC credentital
              ^{table}
                                             |]
  defaultLayout w >>= sendResponseStatus badRequest400



getGLTaxReportValidateFraudPreventionHeadersR :: Text -> Handler Html
getGLTaxReportValidateFraudPreventionHeadersR report = do
  (settings, _) <- unsafeGetReportSettings report
  case processor settings of
   HMRCProcessor params -> validateHMRCFraudPreventionHeaders report params
   _ -> return "Nothing to validate"
  
  

-- * Render 

renderReportList :: (Text, TaxReportSettings) -> Handler (Text, Widget)
renderReportList (name, settings) = runDB $ do
  reports <- selectList [TaxReportType==. name] [Desc TaxReportEnd]
  today <- lift todayH
  let
    classFor state = case state of
      Early -> "text-muted bg-muted" :: Text
      Ready -> "text-warning bg-warning"
      Closed -> "text-warning bg-warning"
      Late _ -> "text-danger bg-danger"
      Submitted _ -> "text-success bg-success"
      _ -> ""
    widget = [whamlet|
  <table *{datatable}>
    <thead>
      <tr>
        <th> Id
        <th> Reference
        <th> Start
        <th> End
        <th> Status
        <th> Submitted
        $#<th> External Reference
    $forall (Entity reportKey report) <- reports
      <tr>
        <td> <a href="@{GLR $ GLTaxReportR (fromSqlKey reportKey) Nothing}">  
            #{tshow $ fromSqlKey reportKey}
        <td> #{taxReportReference report}
        <td.start> #{tshow $ taxReportStart report}
        <td.end> #{tshow $ taxReportEnd report}
        <td class="#{classFor $ taxReportDateStatus settings today report}">
                    #{tshow $ taxReportStatus report}
        <td.submitted> #{tshowM $ taxReportSubmittedAt report}
        $#<td.submitted> #{fromMaybe "" $ taxReportExternalReference report}
  |]
  return (name, widget)

renderReportHeader :: Entity TaxReport -> (Int, Int)-> Widget
renderReportHeader (Entity _ TaxReport{..}) (before, withinPeriod) = do
  let panelClass = case (taxReportStatus, taxReportSubmittedAt, before, withinPeriod) of
                     (Pending, Just _, _, _) -> "panel-danger" -- shoudn't happeellon
                     (Pending, Nothing , 0, 0) -> "panel-info" -- shoudn't happeellon
                     (Pending, Nothing, _, _) -> "panel-warning" -- in progress
                     (Process, Just _, _, _) -> "panel-success" -- done
                     (Process, Nothing, _, _) -> "panel-success" -- shoudn't happen
      hasBefore = before > 0
      panel1 = panel panelClass Nothing taxReportReference  [whamlet|
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
       <th> Transaction out of report period  
       <td> #{tshow before}
     <tr> 
       <th> Transaction within report period  
       <td> #{tshow withinPeriod}
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

reopenButtonForm :: Key TaxReport -> Widget
reopenButtonForm key = [whamlet|
  <form method=POST action="@{GLR $ GLTaxReportReopenR $ fromSqlKey key}">
    <button.btn.btn-danger type="submit"> Reopen
                            |]
preSubmitButtonForm :: Key TaxReport -> Widget
preSubmitButtonForm key = [whamlet|
  <form method=POST action="@{GLR $ GLTaxReportPreSubmitR $ fromSqlKey key}">
    <button.btn.btn-danger type="submit"> Submit
                               |]
submitButtonForm :: Widget -> Key TaxReport -> Widget
submitButtonForm pre key = [whamlet|
  <form method=POST action="@{GLR $ GLTaxReportSubmitR $ fromSqlKey key}">
    ^{pre}
    <button.btn.btn-danger type="submit"> Submit
                               |]

cancelSubmitButtonForm :: Key TaxReport -> Widget
cancelSubmitButtonForm key = [whamlet|
  <form method=GET action="@{GLR $ GLTaxReportR (fromSqlKey key) Nothing}">
    <button.btn.btn-warning type="submit"> Cancel
                               |]

continueButtonForm route = [whamlet|
  <form method=GET action="@{GLR $ route}">
    <button.btn.btn-primary type="submit"> Continue
|]

renderReportView :: Rule -> Entity TaxReport -> TaxReportViewMode ->  Handler Widget
renderReportView rule report mode = do
  (settings, processor) <- unsafeGetReportSettings (taxReportType $ entityVal report)
  bucket'rates <- getBucketRateFromConfig report
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  personName <- entityNameH False
  taxMap <- runDB $ loadTaxTypeMap
  today <- todayH
  (boxes, bucketSummary) <- runDB $ do
    bucketSummary <- loadBucketSummary AllBuckets (entityKey report)
    let buckets = setFromList $ map fst (keys bucket'rates) <> map fst (keys bucketSummary) :: Set Bucket
    boxes <- getBoxes processor buckets 
    return (boxes, bucketSummary)
  let boxValues :: [(TaxBox, TaxDetail -> Maybe Decimal)]
      boxValues = [ (box, f)
                  | box <- boxes
                  , let f detail = either (const Nothing) Just $ computeBox bucket'rates0
                                              (reportDetailToBucketMap taxMap $ tdReportDetail detail) 
                                              box
                  ]
      taxName taxId = FA.taxTypeName . entityVal <$> lookup taxId taxMap
      bucket'rates0 = setFromList . keys $ Map.filter (== ConfigBucket) bucket'rates :: Set (Bucket, Entity FA.TaxType)

  let urlFn = urlForFA faURL
      status = (taxReportStatus (entityVal report), taxReportDateStatus settings today (entityVal report))
  view <- case mode of
            TaxReportPendingView -> do
              details <- loadPendingTaxDetails rule report
              return $ renderTaxDetailTable urlFn personName taxName boxValues (taxReportStart $ entityVal report) details
                     >> case status of
                         (_, Submitted _) -> return ()
                         (Process, _ ) -> reopenButtonForm (entityKey report)
                         _ -> collectButtonForm (entityKey report)
            TaxReportCollectedView -> do
              details <- loadCollectedTaxDetails report
              return $ renderTaxDetailTable urlFn personName taxName boxValues (taxReportStart $ entityVal report) details
                     >> case status of
                         (_, Submitted _) -> return ()
                         (Process, _ ) -> reopenButtonForm (entityKey report)
                         _ -> rejectButtonForm (entityKey report)
            TaxReportBucketView -> do
              return $ renderBucketTable bucket'rates0 bucketSummary
            TaxReportBoxesView -> do
              let box'amounts = computeBoxes bucket'rates0 bucketSummary boxes
              return $ renderBoxTable box'amounts
                     >> case status of
                          (_, Submitted _) -> return ()
                          _ -> preSubmitButtonForm (entityKey report)
            TaxReportConfigChecker -> do
              return $ renderBoxConfigCheckerTable bucket'rates boxes
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

renderTaxDetailTable :: (FATransType -> Int -> Text) --  ^ Url
                     -> (FATransType -> Maybe Int64 -> Text) --  ^ Name
                     -> (FA.TaxTypeId -> Maybe Text) --  ^ Tax type name
                     -> [(TaxBox, TaxDetail -> Maybe Decimal) ]
                     -> Day -> [TaxDetail] -> Widget
renderTaxDetailTable urlFn personName taxName boxes startDate taxDetails =  let
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
        <th>Tax Type
        <th>Bucket
        $if hasBoxes
          <th>Boxes
        $forall (box, _) <- boxes
          <th.none>#{tbName box} 
        <th.none>Status
    <tbody>
      $forall detail <- taxDetails 
        $with (old, pending) <- (tdTranDate detail < startDate, tdIsPending detail && isJust (tdExistingKey detail))
          <tr :old:.bg-danger :pending:.bg-warning>
            <td :old:.text-danger>#{tshow $ tdTranDate detail }
            <td.text-right>#{transNoWithLink urlFn "" (tdTransType detail) (tdTransNo detail) }
            <td.text-center>#{transactionIconSpan $ tdTransType detail }
            <td>#{maybe "" decodeHtmlEntities $ tdMemo detail }
            <td>#{personName (tdTransType detail) (tdEntity detail)}
            <td.text-right>#{formatDoubleWithSign $ tdNetAmount detail }
            <td.text-right>#{formatDoubleWithSign $ tdTaxAmount detail }
            <td.text-right>#{formatDouble' $ (*) (tdRate detail) 100 }%
            <td>
              $case taxName (FA.TaxTypeKey $ tdTaxTypeId detail)
                $of Nothing
                  ##{tshow $  tdTaxTypeId detail}
                $of Just name
                  #{name}
            <td>#{tdBucket detail}
            $if hasBoxes
              <td>
                $forall  (box, valueF) <- boxes
                  $maybe value <- valueF detail
                  $# value is decimal therefore we shouldn't have any rounding problem
                      $if value > 0
                       <span.badge.up data-toggle="tooltip" title="#{formatDecimal value}">
                         #{tbName box}
                      $else
                        $if value < -0
                          <span.badge.down data-toggle="tooltip" title="#{formatDecimal value}">
                            #{tbName box}
            $forall (box, boxValue) <- boxes
              #{renderBoxForDetail box (boxValue detail)}
            <td>
              <span>
                $if old
                  old
                $else
                  current
              <span>
                $if pending
                  pending
                $else
                  normal
          |] <> toWidget commonCSS

-- | Do pivot table  Bucket/Rate
renderBucketTable :: Set (Bucket, Entity FA.TaxType) --  ^ Combination bucket/rate used by config
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
         $forall (Entity (FA.TaxTypeKey taxId) tax@FA.TaxType{taxTypeInactive}, _) <- taxTypes
           <th :taxTypeInactive:.text-danger :taxTypeInactive:.bg-danger data-toggle='tooltip' :taxTypeInactive:title="TaxType is Inactive">
              <div>##{tshow taxId} #{FA.taxTypeName tax}
              <div>#{formatDouble' (FA.taxTypeRate tax)}%
         <th>Total
     <tbody>
       $forall (bucket, txs) <- buckets
         $with bucketIn <-  notElem bucket confBuckets
          <tr :bucketIn:.bg-danger :bucketIn:.text-danger data-toggle="tooltip" :bucketIn:title="Bucket #{bucket} not in current report configuration">
              <td>#{bucket}
            $forall (taxType@(Entity _ FA.TaxType{taxTypeInactive,taxTypeRate}), _) <- taxTypes
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
           $forall (Entity _ FA.TaxType{taxTypeRate}, txs ) <- taxTypes
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
renderBoxForDetail :: TaxBox -> Maybe Decimal -> Html
renderBoxForDetail box valuem =  case valuem of
  Just value | abs value > 0-> [shamlet|
      <td>
        #{tshow value}
        <span.hidden>box:#{tbName box}
                        |]
  _ -> [shamlet|<td>|]


-- | Displays a table with the box values
renderBoxTable :: [(TaxBox, Decimal)] -> Widget
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
    $forall (TaxBox{tbName,tbShouldBe,tbDescription}, amount) <- box'amounts
      <tr class="#{klass tbShouldBe amount}">
        <td>#{tbName}
        <td>#{fromMaybe "" tbDescription}
        <td.text-right>#{formatDecimal amount}
          |]

-- | Display a bucket table showing each box
-- which bucket as an impact on. To do
-- we calculate the value of each box
-- by setting a bucket to 1 and look at the value of the box.
renderBoxConfigCheckerTable :: Map (Bucket, Entity FA.TaxType) BucketType -> [TaxBox] ->  Widget
renderBoxConfigCheckerTable bucket'rates boxes =  let
  buckets :: [Bucket] 
  buckets = nub . sort . map fst $ keys bucket'rates
  rates :: [Entity FA.TaxType]
  rates = nub . sort .map snd $ keys bucket'rates
  mkTxs = [ TaxSummary 0 1 -- tax amount
          , TaxSummary 1 0  --net amount
          ]
  bucketMap0 :: Map Bucket TaxSummary
  bucketMap0 = mapFromList $ map  (, TaxSummary 0 0 ) buckets
  mkMap bucket tx = insertMap bucket tx bucketMap0 
  box'ids = zip boxes [1 ::Int ..]
  renderBoxFor mkTx bucket rate = let
    boxDetails =[ (tshow boxId, tbName, boxValue, up)
                | (b@TaxBox{..}, boxId) <- box'ids
                , let boxValue = either (error . unpack) id $ computeBoxAmount  tbRule rounding (mkMap bucket mkTx)
                      rounding = tbRound0 b
                , let up = if decimalMantissa boxValue >= 1
                           then "up"
                           else if decimalMantissa boxValue <= -1
                           then "down"
                           else "no"
                ]
    klasses = [ "box-" <> boxId  <> "-" <> up  | (boxId, __boxName, __value, up) <- boxDetails ]
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
            <span.glyphicon.box-selected :partial:.partial class="#{iconFor up} box-#{boxId} #{up}" data-toggle="tooltip" title="#{boxName} #{tshow boxValue}" onClick="toggleBox(#{boxId});")>
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
       $forall (TaxBox{tbName}, boxId) <- box'ids
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
  in
    [whamlet|
      <div.row>
        <div.col-md-2 id=control-table >
          ^{control}
        <div.col-md-10 id=bucket-table >
          ^{bucketTable}
          |] <> toWidget controlJS <> toWidget commonCSS
     


commonCSS = [cassius|
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

    

-- ** Saving 
-- | Mark the report as done and save its boxes
closeReport  :: TaxProcessor -> Entity TaxReport -> Handler ()
closeReport  processor report = do
  bucket'rates <- getBucketRateFromConfig report
  runDB $ do
    when (taxReportStatus (entityVal report) == Process) $ do
          error "Report already closed"
    box'amounts <- loadTaxBoxesFromBuckets processor AllBuckets (entityKey report) bucket'rates
    let   mkBox (TaxBox{..}, amount) = TaxReportBox{..} where
            taxReportBoxReport = entityKey report
            taxReportBoxName = tbName
            taxReportBoxValue = amount
    insertMany_ (map mkBox box'amounts)
    update (entityKey report) [TaxReportStatus =. Process]

openReport :: Entity TaxReport -> SqlHandler ()
openReport (Entity reportKey report) = do
  when (isJust $ taxReportSubmittedAt (report)) $ do
    error "Can't reopen a report which has already be submitted"
  deleteWhere [TaxReportBoxReport ==. reportKey]
  update reportKey [TaxReportStatus =. Pending]
  lift $ setSuccess "Report has been succesfully reopened."
  

-- ** Util 
-- | Return the setting corresponding to a report name.
-- Normally, all the function calling should have been given
-- a valid report name. Unless the user is typing randorm url.
-- in which case it is legitimate  to raise an error ;-)
unsafeGetReportSettings :: Text -> Handler (TaxReportSettings, TaxProcessor)
unsafeGetReportSettings name = do
  r <- getReportSettings name
  case r of
    Nothing -> error $ unpack $ "Can't tax settings for " <> name
    Just settings -> return settings

getReportSettings :: Text -> Handler (Maybe (TaxReportSettings, TaxProcessor))
getReportSettings name = do
  settings <- appTaxReportSettings <$> getsYesod appSettings
  return $ fmap (fanr mkTaxProcessor)  $ lookup name settings
  
-- | Get the list of all  Bucket/rate configuration from the config
-- (and the rate in the database)
getBucketRateFromConfig :: Entity TaxReport -> Handler (Map (Bucket, Entity FA.TaxType) BucketType )
getBucketRateFromConfig report = do
  (settings, _) <- unsafeGetReportSettings (taxReportType $ entityVal report)
  getBucketRateFromSettings (entityKey report) settings
