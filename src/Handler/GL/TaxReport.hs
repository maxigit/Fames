module Handler.GL.TaxReport
( getGLTaxReportsR
, postGLNewTaxReportR 
, getGLTaxReportR
, postGLTaxReportR
, getGLTaxReportDetailsR
) where
import Import
import GL.TaxReport.Settings
import GL.Utils
import Data.Time (addDays)
import Database.Persist.MySQL(unSqlBackendKey)
import Database.Persist.Sql (rawSql, toSqlKey, Single(..))
import Data.Aeson(encode)
import qualified FA as FA

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
  key <- runDB $ do
    lasts <- selectList [TaxReportType ==. name] [Desc TaxReportStart]
    let taxReportStart = case lasts of
          (Entity _ report: _) -> 1 `addDays` (Import.taxReportEnd report)
          [] -> startDate settings
        taxReportType = name
        taxReportReference = pack $ formatTime defaultTimeLocale (unpack $ referenceFormat settings) taxReportStart
        taxReportEnd = (-1) `addDays` calculateDate (nextPeriod settings) taxReportStart
        taxReportStatus = Pending
        taxReportSubmittedAt = Nothing

    insert TaxReport{..}
  redirect (GLR . GLTaxReportR  . unSqlBackendKey $ unTaxReportKey key)

getGLTaxReportR :: Int64 -> Handler Html
getGLTaxReportR key = do
  report <- runDB $ loadReport key
  (before, inRange)  <- runDB $ countPendingTransTaxDetails (entityVal report)
  when (before > 0) $ do
    setWarning "Some Transactions part of the  have been entered or modified before current report start date"
  defaultLayout $ renderReport report (before, inRange)

postGLTaxReportR :: Int64 -> Handler Html
postGLTaxReportR key = return "todo"

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
    $forall (Entity reportId TaxReport{..}) <- reports
      <tr>
        <td> <a href="@{GLR $ GLTaxReportR (unSqlBackendKey $ unTaxReportKey reportId)}">  
            #{tshow $ unSqlBackendKey $ unTaxReportKey reportId}
        <td> #{taxReportReference}
        <td> #{tshow taxReportStart}
        <td> #{tshow taxReportEnd}
        <td> #{tshow taxReportStatus}
        <td> #{tshowM taxReportSubmittedAt}
  |]
  return (name, widget)

renderReport :: Entity TaxReport -> (Int, Int)-> Widget
renderReport (Entity rId TaxReport{..}) (before, inRange) = do
  let panelClass = case (taxReportStatus, taxReportSubmittedAt) of
                     (Pending, Just _ ) -> "panel-danger" -- shoudn't happeellon
                     (Pending, Nothing) -> "panel-warning" -- in progress
                     (Process, Just _) -> "panel-success" -- done
                     (Process, Nothing) -> "panel-success" -- shoudn't happen
      hasBefore = before > 0
      key = unSqlBackendKey (unTaxReportKey rId)
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
  panel panelClass "Pending transactions" (renderTransTable key)

-- | Render a table with the transaction, with or without bin
  
renderTransTable key = do
  [whamlet|
  <table#trans-table *{datatable} data-server-side="true" data-ajax="@{GLR $ GLTaxReportDetailsR key}">
    <thead>
      <tr>
        <th>Date
        <th data-class-name="text-right"> TransNo
        <th> TransType
        <th> Memo
        <th data-class-name="text-right">netAmount
        <th data-class-name="text-right">taxAmount
        <th data-class-name="text-right">ratio
        <th>Bucket
        |]
  toWidget [julius|
    $(document).read(function () {
      $('#trans-table').columnDefs
    
    })
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
  let count = "SELECT COUNT(*) " 
  [Single inRange] <- uncurry rawSql (buildPendingTransTaxDetailsQuery count
                                                              taxReportType
                                                              (Just taxReportStart)
                                                              taxReportEnd
                            ) 
  [Single before] <- uncurry rawSql (buildPendingTransTaxDetailsQuery count
                                                             taxReportType
                                                             Nothing
                                                             ((-1) `addDays` taxReportStart)
                           )

  return (before, inRange)                                                                                     

buildPendingTransTaxDetailsQuery :: Text -> Text -> Maybe Day -> Day -> (Text, [PersistValue])
buildPendingTransTaxDetailsQuery selectQuery reportType startDate endDate = 

  let sql0 =  selectQuery <> " FROM 0_trans_tax_details fad"
          <>  " LEFT JOIN fames_tax_report_detail rd ON (tax_report_detail_id = fad.id) "
          <>  " LEFT JOIN fames_tax_report r ON(r.tax_report_id =  rd.tax_report_id AND type = ?) "
          <> " WHERE (rd.tax_report_id is NULL OR ("
          <> "           abs (fad.net_amount * ex_rate - rd.net_amount) > 1e-4 "
          <> "                OR    abs (fad.amount * ex_rate - rd.tax_amount) > 1e-4 "
          <> "               )"
          <> "       ) AND fad.tran_date <= ?"
      p0 = [ toPersistValue $ reportType
           , toPersistValue $ endDate
           ]
  in case startDate of
        Nothing -> (sql0, p0)
        Just start -> (sql0 <> " AND fad.tran_date >= ? ", p0 <> [toPersistValue start])

-- | Load transactions Report details For datatable
getGLTaxReportDetailsR :: Int64 -> Handler Value
getGLTaxReportDetailsR key = do
  rows <- runDB $  do
    (Entity _ TaxReport{..}) <- loadReport key
    let (sql, params) = buildPendingTransTaxDetailsQuery selectQ taxReportType (Just taxReportStart) taxReportEnd
        selectQ = "SELECT fad.* /* ?? */ " -- hack to use 
        -- we need to order them to get a consistent paging
        order = " ORDER BY fad.tran_date, fad.id"
        paging = " LIMIT 100"
    rawSql (sql <> order <> paging) params
  let _types = rows :: [Entity FA.TransTaxDetail]
      _report0 = TaxReportKey 0
      showType = showShortTransType :: FATransType -> Text
      toj (Entity _ FA.TransTaxDetail{..}) = [ toJSON transTaxDetailTranDate
                                             , toJSON transTaxDetailTransNo
                                             , toJSON ((showType . toEnum ) <$> transTaxDetailTransType)
                                             , toJSON transTaxDetailMemo
                                             , toJSON transTaxDetailNetAmount
                                             , toJSON transTaxDetailAmount
                                             , toJSON transTaxDetailRate
                                             , "<none>"
                                             ]

  returnJson $ object [ "data" .= (map toj rows) ]
  
-- * Util
-- | Return the setting corresponding to a report name.
-- Normally, all the function calling should have been given
-- a valid report name. Unless the user is typing randorm url.
-- in which case it is legitimate  to raise an error ;-)
unsafeGetReportSettings :: Text -> Handler TaxReportSettings
unsafeGetReportSettings name = do
  settings <- appTaxReportSettings <$> getsYesod appSettings
  case lookup name settings of
    Nothing -> error $ unpack $ "Can't tax settings for " <> name
    Just settings -> return settings
  
  
__transTaxDetailToReportDetail :: TaxReportId -> Entity FA.TransTaxDetail -> TaxReportDetail
__transTaxDetailToReportDetail reportId (Entity tid FA.TransTaxDetail{..}) = let
  taxReportDetailReport = reportId
  taxReportDetailTaxTransDetail = tid
  taxReportDetailNetAmount = transTaxDetailNetAmount * transTaxDetailExRate
  taxReportDetailTaxAmount =  transTaxDetailAmount * transTaxDetailExRate
  taxReportDetailRatio =  transTaxDetailRate
  taxReportDetailBucket = "<none>"
  taxReportDetailFaTransType = maybe (error "???") toEnum transTaxDetailTransType
  taxReportDetailFaTransNo = fromMaybe (error "???") transTaxDetailTransNo
  taxReportDetailFaTaxType = transTaxDetailTaxTypeId
  in TaxReportDetail{..}

