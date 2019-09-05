{-# LANGUAGE StandaloneDeriving #-}
module Handler.GL.TaxReport.Types
( TaxDetail -- no constructor
-- , taxDetailFromDetail
-- , taxDetailFromBucket
-- , taxDetailFromDetailM
, taxDetailFromDetails
, isNew
, tdFADetail
, tdReportDetail
-- , tdDetailKey
, tdTransType
, tdTransNo
, tdTranDate
, tdTaxTypeId
, tdRate
, tdExRate
, tdIncludedInPrice
, tdNetAmount
, tdTaxAmount
, tdMemo
, tdBucket
, tdDetailToSave
, tdEntity
, tdExistingKey
, tdIsPending
, tdIsNull
)
where


import Import

import FA(TransTaxDetail(..))
import GL.TaxReport.Types
import GL.TaxReport


-- * Type
-- | A mix of FA trans tax detail and report
-- A smart constructor makes sure that the information common
-- to both report detail and trans matches
data TaxDetail = PrivateTaxDetail
 { _private_faDetail :: TransTaxDetail
 , _private_previousDetail :: [Entity TaxReportDetail] -- ^ Details from previous report
 , _private_currentDetail :: Maybe (Entity TaxReportDetail) -- ^ current report detail
 , _private_detailToSave :: TaxReportDetail
 , _private_entity :: Maybe Int64
 }
 deriving Show
deriving instance Show TransTaxDetail

-- * Constructor
makeAndValidateDetail :: Key TaxReport
                      -> (TransTaxDetail -> Maybe Int64 -> Bucket)
                      -> Entity TransTaxDetail
                      -> [Entity TaxReportDetail]
                      -> Maybe (Entity TaxReportDetail)
                      -> Maybe Int64
                      -> Either Text TaxDetail
makeAndValidateDetail report bucketFn (Entity transKey trans@TransTaxDetail{..}) previous currentm personm = do -- Either
  -- check that all details belong to the correct transaction
  let
    errorTail = " for trans_tax_detail #" <> tshow transKey
    details = currentm ?: previous
    validate :: (Eq a, Show a) => Text -> a -> (TaxReportDetail -> a) -> Either Text ()
    validate msg value getter = case filter (/= value) (map (getter . entityVal) details) of
                                    [] -> Right ()
                                    differents -> Left $  msg <> " differs : " <> tshow value <> " /= " <>  tshow differents <> errorTail
  _ <- validate "Trans No" (transTaxDetailTransNo) (Just . taxReportDetailFaTransNo)
  _ <- validate "Trans Type" (toEnum <$> transTaxDetailTransType) (Just . taxReportDetailFaTransType)
  _ <- validate "Trans id" transKey taxReportDetailTaxTransDetail
  -- normally, trans tax details are not modified but only voided ie set to 0.
  -- this can only happen once, therefore we should get a maximum of two previous details
  -- which in that case cancel each other and therefore have the same bucket.
  -- we can therefore consider that they also have the same bucket. But we can check
  let
    previousBuckets = setFromList $ map (taxReportDetailBucket . entityVal) previous :: Set Bucket
  when (length previousBuckets > 1) $ 
    Left $ "Bucket differ: " <> tshow previousBuckets <> errorTail
  -- to compute the expected report detail we ignore the current one
  taxReportDetailFaTransType <- maybe (Left "TransType should not be null") (Right . toEnum) transTaxDetailTransType
  taxReportDetailFaTransNo <- maybe (Left "TransType should not be null") Right transTaxDetailTransNo
  let
    oldsTx = mconcat $ map taxSummary previous
    toSave = taxSummary trans <> reverseTaxSummary oldsTx
    _private_faDetail = trans
    _private_previousDetail = previous
    _private_currentDetail = currentm
    _private_detailToSave = TaxReportDetail{..}
    _private_entity = personm
    taxReportDetailNetAmount = netAmount toSave
    taxReportDetailTaxAmount = taxAmount toSave
    taxReportDetailReport = report
    taxReportDetailTaxTransDetail = transKey
    taxReportDetailRate = transTaxDetailRate / 100
    taxReportDetailBucket = bucketFn trans personm
    taxReportDetailFaTaxType = transTaxDetailTaxTypeId

  Right PrivateTaxDetail{..}




taxDetailFromDetails :: (TransTaxDetail -> Maybe Int64 -> Bucket)
                    -> Key TaxReport
                    -> Entity TransTaxDetail
                    -> [Entity TaxReportDetail]
                    ->  Maybe Int64
                    ->  Either Text TaxDetail
taxDetailFromDetails bucketFn currentReport trans details personm = 
  case partition ((== currentReport) . taxReportDetailReport . entityVal) details of
    ([current], olds) -> makeAndValidateDetail currentReport bucketFn trans olds (Just current) personm
    ([], olds) -> makeAndValidateDetail currentReport bucketFn trans olds Nothing personm
    _ -> Left $ "Too many report details within the current report for transaction tax #" <> tshow (entityKey trans)

isNew :: TaxDetail -> Bool
isNew detail = isNothing (_private_currentDetail detail)

-- * Accessors
tdFADetail = _private_faDetail
tdReportDetail = _private_detailToSave

tdTranDate = transTaxDetailTranDate . tdFADetail
tdTaxTypeId = transTaxDetailTaxTypeId . tdFADetail
tdExRate = transTaxDetailExRate . tdFADetail
tdIncludedInPrice = transTaxDetailIncludedInPrice . tdFADetail
tdMemo = transTaxDetailMemo . tdFADetail

tdTransType = taxReportDetailFaTransType . tdReportDetail
tdTransNo = taxReportDetailFaTransNo . tdReportDetail
tdRate = taxReportDetailRate . tdReportDetail
tdNetAmount = taxReportDetailNetAmount . tdReportDetail
tdTaxAmount = taxReportDetailTaxAmount . tdReportDetail
tdBucket = taxReportDetailBucket . tdReportDetail

tdEntity = _private_entity
-- * Compute difference with what the report should be
tdDetailToSave = _private_detailToSave
tdExistingKey = entityKey <$$> _private_currentDetail

-- | Is pending if there is something to save
-- and it's different from what is alreday
tdIsPending d@PrivateTaxDetail{..} =  fmap entityVal _private_currentDetail /= Just _private_detailToSave
  -- && not (tdIsNull d ) 

tdIsNull detail  = amountNull taxAmount  && amountNull netAmount
  where amountNull f = (abs . f . taxSummary $ _private_detailToSave detail) < 1e-2
