module Handler.GL.TaxReport.Types
( TaxDetail -- no constructor
, taxDetailFromDetail
, taxDetailFromBucket
, taxDetailFromDetailM
, isNew
, tdFADetail
, tdReportDetail
, tdDetailKey
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
)
where


import Import
import qualified FA as FA
import FA(TransTaxDetail(..))
import GL.TaxReport.Types

-- | A mix of FA trans tax detail and report
-- A smart constructor makes sure that the information common
-- to both report detail and trans matches
data TaxDetail = PrivateTaxDetail
 { _private_faDetail :: TransTaxDetail
 , _private_detail :: TaxReportDetail
 , _private_detailKey :: Maybe (Key TaxReportDetail)
 , _privateEntity :: Maybe Int64 -- customer or supplier no
 } 


-- * Constructor

validateDetail :: Key TransTaxDetail ->  TaxDetail -> Either Text TaxDetail
validateDetail key d@(PrivateTaxDetail TransTaxDetail{..} TaxReportDetail{..} _ _) = do
     when (transTaxDetailTransType /= Just (fromEnum taxReportDetailFaTransType)) $ do
        Left $ "Type differs : " <> tshow transTaxDetailTransType <> " /= " <> tshow taxReportDetailFaTransType
     when (transTaxDetailTransNo /= Just taxReportDetailFaTransNo) $ do
        Left $ "No differs : " <> tshow transTaxDetailTransNo <> " /= " <> tshow taxReportDetailFaTransNo
     when (key /= taxReportDetailTaxTransDetail) $ do
        Left $ "trans tax detail id differs : " <> tshow key <> " /= " <> tshow taxReportDetailTaxTransDetail
     Right d
  

taxDetailFromDetail :: Entity TransTaxDetail -> Entity TaxReportDetail -> Maybe Int64 ->  Either Text TaxDetail  
taxDetailFromDetail trans detail personm = validateDetail (entityKey trans)
   $ PrivateTaxDetail (entityVal trans)
               (entityVal detail)
               (Just $ entityKey detail)
               personm

taxDetailFromBucket :: Key TaxReport -> Entity TransTaxDetail -> Maybe Int64 -> (TransTaxDetail -> Maybe Int64 -> Bucket) -> TaxDetail  
taxDetailFromBucket report (Entity transKey trans@TransTaxDetail{..}) personm bucketFn = let
  taxReportDetailReport = report
  taxReportDetailTaxTransDetail = transKey
  taxReportDetailNetAmount = transTaxDetailNetAmount * transTaxDetailExRate
  taxReportDetailTaxAmount = transTaxDetailAmount * transTaxDetailExRate
  taxReportDetailRate = transTaxDetailRate / 100
  taxReportDetailBucket = bucketFn trans personm
  taxReportDetailFaTransType = maybe (error "TransType should not be null") toEnum transTaxDetailTransType
  taxReportDetailFaTransNo = fromMaybe (error "TransType should not be null") transTaxDetailTransNo
  taxReportDetailFaTaxType = transTaxDetailTaxTypeId

  in PrivateTaxDetail trans TaxReportDetail{..} Nothing personm

isNew :: TaxDetail -> Bool
isNew detail = isJust (_private_detailKey detail)

taxDetailFromDetailM :: Key TaxReport
                     -> (TransTaxDetail -> Maybe Int64 -> Bucket)
                     -> Entity TransTaxDetail
                     -> Maybe (Entity TaxReportDetail)
                     -> Maybe Int64
                     -> Either Text TaxDetail
taxDetailFromDetailM report bucketFn trans Nothing personm = Right $ taxDetailFromBucket report trans personm bucketFn
taxDetailFromDetailM report bucket trans (Just detail) personm = taxDetailFromDetail trans detail personm

-- * Accessors
tdFADetail = _private_faDetail
tdReportDetail = _private_detail
tdDetailKey = _private_detail

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

tdEntity = _privateEntity
-- * Compute difference with what the report should be
tdDetailToSave d@PrivateTaxDetail{..}= case _private_detailKey of
  Nothing -> -- doesn't exist in the db
     Just _private_detail
  Just _ -> tdDetailDiff  d

tdDetailDiff (PrivateTaxDetail TransTaxDetail{..} TaxReportDetail{..} _ _) = let
  net = transTaxDetailNetAmount * transTaxDetailExRate - taxReportDetailNetAmount
  tax = transTaxDetailAmount * transTaxDetailExRate - taxReportDetailTaxAmount
  small x = abs x < 1e-4
  in if all small [tax, net]
     then Nothing
     else Just $ TaxReportDetail{taxReportDetailNetAmount = net, taxReportDetailTaxAmount=tax,..}
   

  
