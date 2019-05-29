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
 } 


-- * Constructor

validateDetail :: Key TransTaxDetail ->  TaxDetail -> Either Text TaxDetail
validateDetail key d@(PrivateTaxDetail TransTaxDetail{..} TaxReportDetail{..} _) = do
     when (transTaxDetailTransType /= Just (fromEnum taxReportDetailFaTransType)) $ do
        Left $ "Type differs : " <> tshow transTaxDetailTransType <> " /= " <> tshow taxReportDetailFaTransType
     when (transTaxDetailTransNo /= Just taxReportDetailFaTransNo) $ do
        Left $ "No differs : " <> tshow transTaxDetailTransNo <> " /= " <> tshow taxReportDetailFaTransNo
     when (key /= taxReportDetailTaxTransDetail) $ do
        Left $ "trans tax detail id differs : " <> tshow key <> " /= " <> tshow taxReportDetailTaxTransDetail
     Right d
  

taxDetailFromDetail :: Entity TransTaxDetail -> Entity TaxReportDetail -> Either Text TaxDetail  
taxDetailFromDetail trans detail = validateDetail (entityKey trans)
   $ PrivateTaxDetail (entityVal trans)
               (entityVal detail)
               (Just $ entityKey detail)

taxDetailFromBucket :: Key TaxReport -> Entity TransTaxDetail -> (TransTaxDetail -> Bucket) -> TaxDetail  
taxDetailFromBucket report (Entity transKey trans@TransTaxDetail{..}) bucketFn = let
  taxReportDetailReport = report
  taxReportDetailTaxTransDetail = transKey
  taxReportDetailNetAmount = transTaxDetailNetAmount * transTaxDetailExRate
  taxReportDetailTaxAmount = transTaxDetailAmount * transTaxDetailExRate
  taxReportDetailRate = transTaxDetailRate / 100
  taxReportDetailBucket = bucketFn trans 
  taxReportDetailFaTransType = maybe (error "TransType should not be null") toEnum transTaxDetailTransType
  taxReportDetailFaTransNo = fromMaybe (error "TransType should not be null") transTaxDetailTransNo
  taxReportDetailFaTaxType = transTaxDetailTaxTypeId

  in PrivateTaxDetail trans TaxReportDetail{..} Nothing

isNew :: TaxDetail -> Bool
isNew detail = isJust (_private_detailKey detail)

taxDetailFromDetailM :: Key TaxReport
                     -> (TransTaxDetail -> Bucket)
                     -> Entity TransTaxDetail
                     -> Maybe (Entity TaxReportDetail)
                     -> Either Text TaxDetail
taxDetailFromDetailM report bucketFn trans Nothing = Right $ taxDetailFromBucket report trans bucketFn
taxDetailFromDetailM report bucket trans (Just detail) = taxDetailFromDetail trans detail

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
