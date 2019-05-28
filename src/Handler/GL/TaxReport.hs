module Handler.GL.TaxReport
( getGLTaxReportsR
, postGLNewTaxReportR 
, getGLTaxReportR
, postGLTaxReportR
, getGLTaxReportDetailsR
) where
import Import
import GL.TaxReport.Settings
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
  redirect (GLR $ GLTaxReportR (fromSqlKey key) Nothing)

getGLTaxReportR :: Int64 -> Maybe TaxReportViewMode -> Handler Html
getGLTaxReportR key mode = do
  report <- runDB $ loadReport key
  (before, inRange)  <- runDB $ countPendingTransTaxDetails (entityVal report)
  view <- renderReportView report (fromMaybe TaxReportPendingView mode)
  when (before > 0) $ do
    setWarning "Some Transactions part of the  have been entered or modified before current report start date"
  defaultLayout $ renderReportHeader report (before, inRange) >> view

postGLTaxReportR :: Int64 -> Maybe TaxReportViewMode -> Handler Html
postGLTaxReportR key modem = return "todo"

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

renderReportView :: Entity TaxReport -> TaxReportViewMode ->  Handler Widget
renderReportView report mode = do
  view <- case mode of
            TaxReportPendingView -> do
              details <- loadPendingTaxDetails report
              return $ renderTaxDetailTable details
            TaxReportCollectedView -> do
              details <- loadCollectedTaxDetails report
              return $ renderTaxDetailTable details
            _ -> return $ renderTaxDetailTable []
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

renderTaxDetailTable taxDetails = 
  [whamlet|
   <table *{datatable}>
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
    <tbody>
      $forall detail <- taxDetails 
        <tr>
          <td>#{tshow $ tdTranDate detail }
          <td>#{tshow $ tdTransNo detail }
          <td>#{tshow $ tdTransType detail }
          <td>#{fromMaybe "" $ tdMemo detail }
          <td.text-right>#{formatDouble' $ tdNetAmount detail }
          <td.text-right>#{formatDouble' $ tdTaxAmount detail }
          <td.text-right>#{formatDouble' $ tdRate detail }%
          <td>#{tdBucket detail}
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
  in first Tagged $ case startDate of
                Nothing -> (sql0, p0)
                Just start -> (sql0 <> " AND fad.tran_date >= ? ", p0 <> [toPersistValue start])

buildCollectedTaxDetailsQuery :: Tagged e Text -> Key TaxReport -> (Tagged e Text, [PersistValue])
buildCollectedTaxDetailsQuery (Tagged selectQuery) reportKey = 
  let sql0 =  selectQuery <> " FROM 0_trans_tax_details fad"
          <>  " JOIN fames_tax_report_detail rd ON (tax_report_detail_id = fad.id) "
          <> " WHERE tax_report_detail_id = ? "
          <> "      AND (rd.tax_report_id is NULL OR ("
          <> "           abs (fad.net_amount * ex_rate - rd.net_amount) < 1e-4 "
          <> "                AND    abs (fad.amount * ex_rate - rd.tax_amount) < 1e-4 "
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

loadPendingTaxDetails :: Entity TaxReport -> Handler [TaxDetail]
loadPendingTaxDetails (Entity reportKey TaxReport{..}) =
  loadTaxDetail (uncurry $ taxDetailFromDetailM reportKey "<none>")
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
  rows <- runDB $  do
    (Entity _ TaxReport{..}) <- loadReport key
    let (Tagged sql, params) = buildPendingTransTaxDetailsQuery selectQ taxReportType Nothing taxReportEnd
        selectQ = "SELECT fad.* /* ?? */ " -- hack to use 
        -- we need to order them to get a consistent paging
        order = " ORDER BY fad.tran_date, fad.id"
        paging = " LIMIT 50000"
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
  settings <- appTaxReportSettings <$> getsYesod appSettings
  case lookup name settings of
    Nothing -> error $ unpack $ "Can't tax settings for " <> name
    Just settings -> return settings
  

formatDouble' = F.sformat commasFixed
