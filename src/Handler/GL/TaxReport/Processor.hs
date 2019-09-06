{-# LANGUAGE DisambiguateRecordFields #-} 
{-# LANGUAGE MultiWayIf #-}
module Handler.GL.TaxReport.Processor
( mkTaxProcessor
, TaxProcessor()
, checkExternalStatus
, submitReturn
, displayExternalStatuses
, preSubmitCheck
, getBoxes
)
where
import Import
import GL.TaxReport.Types
import GL.TaxReport.Settings
import GL.TaxReport
import GL.Utils

import Handler.GL.TaxReport.HMRC
import Handler.GL.TaxReport.Common
import Handler.GL.TaxReport.Types
import Util.Decimal
import Data.Fixed


import Formatting
import Formatting.Time(year, month)
import qualified FA as FA
import Text.Shakespeare.Text (st)


-- * Type


-- * Processors
-- ** Manual
emptyProcessor :: TaxReportSettings ->  TaxProcessor
emptyProcessor settings = TaxProcessor {..} where
  checkExternalStatus _ = return Ready
  submitReturn (Entity _ report)= return $ Right (report, Nothing)
  displayExternalStatuses _ =  return "Processor doesn't provide statuses"
  getBoxes _ = return $ boxesRaw settings
  preSubmitCheck = \_ -> return (Just "")

mkTaxProcessor :: TaxReportSettings -> TaxProcessor
mkTaxProcessor settings = case processor settings of
  ManualProcessor params -> mkManualProcessor params settings
  HMRCProcessor params -> mkHMRCProcessor params settings
  ECSLProcessor params -> mkECSLProcessor params settings
  
mkManualProcessor :: ManualProcessorParameters -> TaxReportSettings -> TaxProcessor
mkManualProcessor params settings = (emptyProcessor settings)
  { submitReturn = \report -> (Right . (,Nothing)) <$> submitManual params (settings) report
  }

-- ** ECSL
mkECSLProcessor :: ECSLProcessorParameters -> TaxReportSettings -> TaxProcessor
mkECSLProcessor params settings = (emptyProcessor settings)
  { submitReturn = \report -> submitECSL params report settings
  }
-- ** HMRC
mkHMRCProcessor :: HMRCProcessorParameters -> TaxReportSettings -> TaxProcessor
mkHMRCProcessor params settings = processor where
  processor = (emptyProcessor settings)
                    { checkExternalStatus = \report -> checkHMRCStatus report params
                    , submitReturn = \report -> (,Nothing) <$$> submitHMRC params report settings
                    , getBoxes = \_ ->  return . either (error . unpack) id $ getHMRCBoxes settings
                    , displayExternalStatuses = \reportType -> displayHMRCStatuses reportType params
                    , preSubmitCheck = hmrcPreSubmitCheck getStatus
                    } 
  getStatus reportId = getHMRCCorrectionStatus loadReturn reportId
  loadReturn reportId whichBucket = do
    bucket'rates <- getBucketRateFromSettings reportId settings
    boxes <- runDB $ loadTaxBoxesFromBuckets processor  whichBucket reportId bucket'rates
    -- ^ recompute boxes for the corresponding bucket mode
    -- which are never saved in the database
    case mkVatReturn "<periodKey>" False (map (mkBox reportId) boxes) of
      Left err -> error (unpack err)
      Right ret -> return ret
  -- ^ loadTaxBoxes need the processor to use getBoxes
  mkBox reportId (TaxBox{..}, amount) = TaxReportBox{..} where
          taxReportBoxReport = reportId
          taxReportBoxName = tbName
          taxReportBoxValue = amount
  

-- * Main Dispatchers

   
-- | Allow a processor to alter boxes depending on buckets
-- This is used for ECSL where each bucket corresponding to a box
getBoxesOld :: TaxReportSettings -> Set Bucket -> SqlHandler [TaxBox]
getBoxesOld settings buckets = either (error . unpack) id <$> case processor settings of
  HMRCProcessor _ -> return $ getHMRCBoxes settings
  ECSLProcessor params -> Right <$> getECSLBoxes params settings buckets
  _ -> return . Right $ boxesRaw settings

-- * HMRC
-- ** Types
  
-- ** Main functions
-- | Connect to HMRC and check the status of the current tax return.
  -- Retrieve if needed the period reference needed to submit the return
checkHMRCStatus :: Entity TaxReport -> HMRCProcessorParameters -> Handler TaxReportStatus
checkHMRCStatus (Entity reportKey report@TaxReport{..}) settings = do
  -- get token for VAT obligation. hack to call getHMRC Token with a report id
  -- so that we are redirected to the correct page
  getHMRCToken taxReportType settings
  obligations <- retrieveVATObligations taxReportType (Just report)settings
  -- traceShowM ("OBLIGATIONS", obligations)
  case obligations of
    [obligation] -> case received obligation of
         Nothing -> return Ready
         Just submitted -> return $ Submitted (UTCTime submitted 0)
    -- [] -> return Submitted -- treats not present as already submitted, or not required
    _ ->  error "Problem retrieving the VAT obligations"

submitHMRC :: HMRCProcessorParameters -> Entity TaxReport -> TaxReportSettings  -> Handler (Either Text TaxReport)
submitHMRC params report _settings = do
  -- check user has read legal requirement
  ((result,view), encType) <- runFormPost $ hmrcPreSubmitForm
  case result of
    FormMissing -> error "missing"
    FormFailure msg ->  error $ "Form Failure:" ++ show msg
    FormSuccess False -> return $ Left "Please read and agree the legal declaration"
    FormSuccess confirmed -> do
        

      obligations <- retrieveVATObligations (taxReportType $ entityVal report) (Just $ entityVal report) params
      pKey <- case obligations of
        [obligation] -> case received obligation of
            Nothing -> return $ periodKey obligation
            Just submitted -> error "Report already submitted"
        _ -> error "No period key found. Can't submit report "
      boxes <- runDB $ selectList [TaxReportBoxReport ==. entityKey report] []
      Right <$> submitHMRCReturn (entityVal report) pKey (map entityVal boxes) params

getHMRCBoxes ::  TaxReportSettings -> Either Text [TaxBox]
getHMRCBoxes settings@TaxReportSettings{..} =  do
  let boxMap = mapFromList $ zip (map tbName boxesRaw) boxesRaw :: Map Text TaxBox
      findBox boxname = maybe (Left $ "Box " <> boxname <> " not present in HMRC report") Right
                      $ lookup boxname boxMap
  b1' <- findBox "B1"
  b2' <- findBox "B2"
  -- b3 calculated
  b4'<- findBox "B4"
  -- b5 calculated
  b6'<- findBox "B6" 
  b7'<- findBox "B7"
  b8'<- findBox "B8"
  b9'<- findBox "B9"

  let
    [b1,b2,b4] = [b {tbRound = Just $ Round 2} | b <- [b1', b2', b4']]
    b3 = TaxBox "B3" (Just "Total VAT due" )(Just GT) (TaxBoxSum $ map (TaxBoxRound 2 . tbRule) [b1, b2]) (Just $ Round 2)
    [b5'3, b5'4] = map (TaxBoxRound 2 . tbRule) [b3, b4]
    b5 = TaxBox "B5" (Just "Net to be payed to HMRC" ) Nothing (TaxBoxSub b5'3 b5'4 ) (Just $ Round 2)
    [b6,b7,b8,b9] = [b {tbRound = Just $ RoundDown 0} | b <- [b6',b7',b8',b9']]
  -- get all other boxes
  let newBoxes = [b1,b2,b3,b4,b5,b6,b7,b8,b9]
      newBoxesName = map tbName newBoxes
      others = filter ((`notElem` newBoxesName) . tbName) boxesRaw
  Right $ newBoxes <> others

displayHMRCStatuses :: Text -> HMRCProcessorParameters -> Handler Widget
displayHMRCStatuses reportType param = do
  obligations <- retrieveVATObligations reportType Nothing param
  return [whamlet|
<table *{datatable}>
 <thead>
   <th>Start
   <th>End
   <th>Due
   $# <th>Period Key
   <th>Received
 <tbody>
   $forall VATObligation{..} <- obligations
     <tr>
       <td> #{tshow start}
       <td> #{tshow end}
       <td> #{tshow due}
       $# <td> #{periodKey}
       <td> #{tshowM received}
|]
{-# NOINLINE hmrcLegalDeclaration #-}
hmrcLegalDeclaration :: Html
hmrcLegalDeclaration = [shamlet|
<p>When you submit this VAT information you are making a legal declaration that the information is true and complete. A false declaration can result in prosecution.
<p>Declaration text to be used if only Agents make the submission;
<p>I confirm that my client has received a copy of the information contained in this return and approved the information as being correct and complete to the best of their knowledge and belie
|]

hmrcMethodsForCorrectingErrors = [shamlet|
           <p> Please refers to HMRC <a href="#{hmrcBase}#methods-for-correcting-errors" target="_blank">notice</a> before submitting the return. 
        <p>There are 2 methods for correcting errors:
        <ul>
          <li>method 1 – for errors of a net value that do not exceed £10,000, or errors of a net value between £10,000 and £50,000 that do not exceed the limit described in <a href="#{hmrcBase}#Method-1" target="_blank">paragraph 4.3</a>
          <li>method 2 – for errors of a net value between £10,000 and £50,000 that exceed the limit in <a href="#{hmrcBase}#Method-1" target="_blank">paragraph 4.3</a>, or for net errors greater than £50,000.00, or if you so choose, for errors of any size
          |] where hmrcBase = "https://www.gov.uk/guidance/how-to-correct-vat-errors-and-make-adjustments-or-claims-vat-notice-70045" :: Text

-- | Display the legal declaration but within a form with a
-- mandatory check button
hmrcPreSubmitCheck :: (Key TaxReport -> Handler (CorrectionStatus, Fixed E2) ) -- ^ status loader
                   -> Entity TaxReport
                   -> Handler (Maybe Widget)
hmrcPreSubmitCheck getStatus report = do
  -- check that there is not too much correction in the past
  ((_,view), encType) <- runFormPost $ hmrcPreSubmitForm
  (correctionStatus, vat) <- getStatus (entityKey report)
  case correctionStatus of
    CorrectionManual -> do
      setError $ [shamlet|<h3>This return contains corrections of already submitted returns.
                          <p> The correction amount (#{showFixed False vat}) above the authorised threshold.
                          <p>Please contact your Accountant
                          |]
                 <> hmrcMethodsForCorrectingErrors
      return Nothing
    CorrectionDisplayNotice -> do -- display notice message
      return . Just $ dangerPanel "Legal Declaration" [whamlet|
         <h3> This return contains corrections of already submitted returns.
         <p> The correction amount (#{showFixed False vat}) seems to be within the authorized threshold.
         <p> Before submitting the return, please verity the above is correct.
         ^{hmrcMethodsForCorrectingErrors}
        ^{view}
        |]
    CorrectionOK -> return . Just $ infoPanel "Legal Declaration" view

getHMRCCorrectionStatus :: (Key TaxReport -> WhichBucket -> Handler VATReturn) -> Key TaxReport -> Handler (CorrectionStatus, Fixed E2)
getHMRCCorrectionStatus loadReturn report = do
  outReturn <- loadReturn report BucketsOut 
  let vat = abs (vr_netVatDue outReturn)
  status <- if | vat == 0 -> return $ CorrectionOK
               | vat > 50000 -> return CorrectionManual
               | vat >= 10000 -> do
                         allReturn <- loadReturn report AllBuckets
                         let total = abs (vr_totalValueSalesExVAT allReturn)
                         if vat >= (fromRational $ toRational total) * 0.01
                           -- ^ total is E0 fixed, so we have to convert to E2 before multiplying by 0.01
                           -- otherwise 0.0.1 get converted to 0
                           then return CorrectionManual
                           else return CorrectionDisplayNotice
               | otherwise -> return $ CorrectionDisplayNotice
  return (status, vat)
   
  
hmrcPreSubmitForm :: Html -> MForm Handler (FormResult Bool, Widget)
hmrcPreSubmitForm extra =  do
  (f, v) <- mreq checkBoxField "Confirm" Nothing
  let widget = [whamlet| 
          #{extra}
          #{hmrcLegalDeclaration}
          ^{renderField v}
          |]
  return (f, widget)
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
-- ** Type
data ECSL = ECSL
  { eCountry :: Text
  , eCustomerGST :: Text
  , eCustomerName :: Maybe Text
  , eAmount :: Int
  , eIndicator :: Int
  } deriving (Eq, Show)
-- ** Common
parseECSLBucket :: Bucket -> Either (Text, Maybe Text) (Text, Int)
parseECSLBucket bucket = case break (==';') bucket of 
  (_, "") -> Left (bucket, Nothing)
  (taxCode, drop 1 -> indicator' ) -> case readMay indicator' of
    Just indicator | indicator `elem` [0,2,3] -> Right (taxCode, indicator)
    _ -> Left (taxCode , Just indicator')

--  ** Compute boxes
getECSLBoxes  :: ECSLProcessorParameters -> TaxReportSettings -> Set Bucket -> SqlHandler [TaxBox]
getECSLBoxes ECSLProcessorParameters{..} TaxReportSettings{..} buckets = do
  -- for each bucket which match the box regexp
  -- traceShowM ("Bukets for boxes" , buckets )
  let bucketBoxes = do -- []
       bucket <- toList buckets
       let rule = TaxBoxNet bucket
           rounding = RoundDown 0
       -- traceShowM ("BUCKET", bucket, break (==';') bucket)
       case parseECSLBucket bucket of
          Left (_, Nothing) | Just bucket == outOfScope -> [TaxBox bucket (Just "Out of ECSL scope") Nothing rule (Just rounding)]
          Left (taxCode, indicatorm) -> [TaxBox bucket indicatorm (Just EQ) rule (Just rounding)]
          Right (taxCode, indicator) -> let
                                       description i = Just $ "Sales for "  <> taxCode <> " " <> toS i
                                       toS i = case (i :: Int) of
                                         0 -> "of Goods"
                                         2 -> "as intermediary"
                                         3 -> "of Services"
                                         _ -> "<ERROR: unknow indicator " <> tshow i <> ">"
                                   in [TaxBox bucket (description indicator) (Just GT) rule (Just rounding)]
  return $ boxesRaw <> bucketBoxes
    

-- ** Submit
submitECSL :: ECSLProcessorParameters -> Entity TaxReport -> TaxReportSettings
           -> Handler (Either Text ( TaxReport, Maybe TypedContent ))
submitECSL params@ECSLProcessorParameters{..} report TaxReportSettings{..} = do
  now <- liftIO $ getCurrentTime
  boxes <- runDB $ selectList [TaxReportBoxReport ==. entityKey report] []
  let ecls = rights $ map (boxToECSL . entityVal) boxes

  userm <- currentFAUser
  let    contactName = maybe "" FA.userRealName userm
  (content, attachmentName) <- generateECSLCSV contactName (entityVal report) params ecls
  return $ Right ( (entityVal report) { taxReportSubmittedAt = Just now
                , taxReportExternalReference = Just attachmentName
                }
         , Just content)

boxToECSL :: TaxReportBox -> Either (Text, Maybe Text) ECSL
boxToECSL (TaxReportBox{..}) = uncurry go <$> parseECSLBucket taxReportBoxName  where
  go taxCode eIndicator = ECSL{..} where
    (eCountry, eCustomerGST) = splitAt 2 taxCode
    eCustomerName = Nothing
    eAmount = case decimalConvert (normalizeDecimal taxReportBoxValue) of
      Just amount | 0 == decimalPlaces amount -> decimalMantissa amount
      _ -> error $ "ECSL Amount needs to be whole number" <>  " " <> show taxReportBoxValue


generateECSLCSV :: Text -> TaxReport -> ECSLProcessorParameters -> [ECSL] -> Handler (TypedContent, Text)
generateECSLCSV contactName TaxReport{..} ECSLProcessorParameters{..} ecsls = do
  let start =  taxReportStart
  let csv = [ "HMRC_VAT_ESL_BULK_SUBMISSION_FILE"
            , format ("\n"%stext%","%stext%","%year%","%month%",GBP,"%stext%",0")
                         vatNumber branch start start contactName
            ] ++ map go ecsls
      go e@ECSL{..} = format ("\n"%stext%","%stext%","%int%","%int) eCountry eCustomerGST eAmount eIndicator
      source = yieldMany csv
      attachmentName = format ("ecsl-"%year%"-"%month%".csv") start start
  setAttachment attachmentName
  content <- respondSource ("text/csv") (source .| mapC toFlushBuilder)
  return (content, toStrict attachmentName)

