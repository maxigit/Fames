{-# LANGUAGE DisambiguateRecordFields #-} 
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
import Util.Decimal


import Formatting
import Formatting.Time(year, month)
import qualified FA as FA
import Text.Shakespeare.Text (st)


-- * Type
data TaxProcessor = TaxProcessor
  { checkExternalStatus :: Entity TaxReport -> Handler TaxReportStatus
  , submitReturn :: Entity TaxReport -> Handler (TaxReport, Maybe TypedContent)
  , displayExternalStatuses :: Text -> Handler Widget
  , getBoxes :: Set Bucket -> SqlHandler [TaxBox]
  -- ^ Allow a processor to alter boxes depending on buckets
  -- This is used for ECSL where each bucket corresponding to a box
  , preSubmitCheck ::  Entity TaxReport -> Handler Widget
  -- ^ specific check and legal requirement
  }


-- * Processors
-- ** Manual
emptyProcessor :: TaxReportSettings ->  TaxProcessor
emptyProcessor settings = TaxProcessor {..} where
  checkExternalStatus _ = return Ready
  submitReturn (Entity _ report)= return  (report, Nothing)
  displayExternalStatuses _ =  return "Processor doesn't provide statuses"
  getBoxes _ = return $ boxesRaw settings
  preSubmitCheck = \_ -> return ""

mkTaxProcessor :: TaxReportSettings -> TaxProcessor
mkTaxProcessor settings = case processor settings of
  ManualProcessor params -> mkManualProcessor params settings
  HMRCProcessor params -> mkHMRCProcessor params settings
  ECSLProcessor params -> mkECSLProcessor params settings
  
mkManualProcessor :: ManualProcessorParameters -> TaxReportSettings -> TaxProcessor
mkManualProcessor params settings = (emptyProcessor settings)
  { submitReturn = \report -> (,Nothing) <$> submitManual params (settings) report
  }

-- ** ECSL
mkECSLProcessor :: ECSLProcessorParameters -> TaxReportSettings -> TaxProcessor
mkECSLProcessor params settings = (emptyProcessor settings)
  { submitReturn = \report -> submitECSL params report settings
  }
-- ** HMRC
mkHMRCProcessor :: HMRCProcessorParameters -> TaxReportSettings -> TaxProcessor
mkHMRCProcessor params settings = (emptyProcessor settings)
  { checkExternalStatus = \report -> checkHMRCStatus report params
  , submitReturn = \report -> (,Nothing) <$> submitHMRC params report settings
  , getBoxes = \_ ->  return . either (error . unpack) id $ getHMRCBoxes settings
  , displayExternalStatuses = \reportType -> displayHMRCStatuses reportType params
  , preSubmitCheck = hmrcPreSubmitCheck
  }

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

submitHMRC :: HMRCProcessorParameters -> Entity TaxReport -> TaxReportSettings  -> Handler TaxReport
submitHMRC params report _settings = do
  obligations <- retrieveVATObligations (taxReportType $ entityVal report) (Just $ entityVal report) params
  pKey <- case obligations of
    [obligation] -> case received obligation of
         Nothing -> return $ periodKey obligation
         Just submitted -> error "Report already submitted"
    _ -> error "No period key found. Can't submit report "
  boxes <- runDB $ selectList [TaxReportBoxReport ==. entityKey report] []
  submitHMRCReturn (entityVal report) pKey (map entityVal boxes) params

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
hmrcLegalDeclaration :: Text
hmrcLegalDeclaration = [st|
When you submit this VAT information you are making a legal declaration that the information is true and complete. A false declaration can result in prosecution.

Declaration text to be used if only Agents make the submission;

I confirm that my client has received a copy of the information contained in this return and approved the information as being correct and complete to the best of their knowledge and belie
|]

-- | Display the legal declaration but within a form with a
-- mandatory check button
hmrcPreSubmitCheck :: Entity TaxReport -> Handler Widget
hmrcPreSubmitCheck _ = do
  ((_,view), encType) <- runFormPost $ hmrcPreSubmitForm
  return view

hmrcPreSubmitForm :: Html -> MForm Handler (FormResult Bool, Widget)
hmrcPreSubmitForm _ =  do
  (f, v) <- mreq checkBoxField "Confirm" Nothing
  let widget = infoPanel "Legal Declaration" [whamlet| 
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
           -> Handler ( TaxReport, Maybe TypedContent )
submitECSL params@ECSLProcessorParameters{..} report TaxReportSettings{..} = do
  now <- liftIO $ getCurrentTime
  boxes <- runDB $ selectList [TaxReportBoxReport ==. entityKey report] []
  let ecls = rights $ map (boxToECSL . entityVal) boxes

  userm <- currentFAUser
  let    contactName = maybe "" FA.userRealName userm
  (content, attachmentName) <- generateECSLCSV contactName (entityVal report) params ecls
  return ( (entityVal report) { taxReportSubmittedAt = Just now
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

