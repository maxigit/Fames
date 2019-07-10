module Handler.GL.TaxReport.Processor where
import Import
import GL.TaxReport.Types
import GL.TaxReport.Settings
import GL.TaxReport
import GL.Utils
import Handler.GL.TaxReport.Types
import Handler.GL.TaxReport.HMRC

-- * Main Dispatchers
checkExternalStatus :: Entity TaxReport -> TaxReportSettings -> Handler TaxReportStatus
checkExternalStatus report settings  = case processor settings of
  ManualProcessor _ -> return Ready
  HMRCProcessor params -> checkHMRCStatus report params

submitReturn :: Entity TaxReport -> TaxReportSettings -> Handler TaxReport
submitReturn report settings = case processor settings of
  ManualProcessor params  -> submitManual params (settings) report
  HMRCProcessor params  -> submitHMRC params report settings
   
-- | Allow a processor to alter boxes depending on buckets
-- This is used for ECSL where each bucket corresponding to a box
getBoxes :: TaxReportSettings -> [Bucket] -> SqlHandler [TaxBox]
getBoxes settings _ = return $ boxesRaw settings

-- * HMRC
-- ** Types
  
-- ** Main functions
-- | Connect to HMRC and check the status of the current tax return.
  -- Retrieve if needed the period reference needed to submit the return
checkHMRCStatus :: Entity TaxReport -> HMRCProcessorParameters -> Handler TaxReportStatus
checkHMRCStatus (Entity reportKey report@TaxReport{..}) settings = do
  -- get token for VAT obligation. hack to call getHMRC Token with a report id
  -- so that we are redirected to the correct page
  getHMRCToken taxReportType settings (Just reportKey)
  obligations <- retrieveVATObligations taxReportType (Just report)settings
  traceShowM ("OBLIGATIONS", obligations)
  case obligations of
    [obligation] -> case received obligation of
         Nothing -> return Ready
         Just submitted -> return $ Submitted (UTCTime submitted 0)
    -- [] -> return Submitted -- treats not present as already submitted, or not required
    _ ->  error "Problem retrieving the VAT obligations"

submitHMRC :: HMRCProcessorParameters -> Entity TaxReport -> TaxReportSettings  -> Handler TaxReport
submitHMRC params report _settings = do
  obligations <- retrieveVATObligations (taxReportType $ entityVal report) (Just $ entityVal report) params
  pKey <- case obligations of
    [obligation] -> case received obligation of
         Nothing -> return $ periodKey obligation
         Just submitted -> error "Report already submitted"
    _ -> error "No period key found. Can't submitt report "
  boxes <- runDB $ selectList [TaxReportBoxReport ==. entityKey report] []
  submitHMRCReturn (entityVal report) pKey (map entityVal boxes) params

-- * Manual
-- Set the submitted date 
submitManual ManualProcessorParameters{..} settings (Entity reportKey report) = do
  let
    -- applly submiission date calculator if available
    -- to deadline of end of period
    date0 = taxReportDeadline settings report
    submitted = maybe id calculateDate submissionDate $ date0
  return report { taxReportSubmittedAt = Just (UTCTime submitted 0)
                , taxReportExternalReference = Just ("Manual:" <> taxReportReference report)
                }
  
-- * ECSL
-- 
