module Handler.GL.TaxReport.Common
where
import Import hiding(RuleInput)
import GL.TaxReport.Types
import GL.TaxReport.Settings
import GL.TaxReport
import Handler.GL.TaxReport.Types
import Handler.GL.TaxReport.Processor
import Handler.GL.TaxReport.HMRC
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
import Util.Decimal
import Handler.Items.Category.Cache (customerCategoryFinderCached)

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
  [Single withinPeriod] <- runTaggedSql (buildPendingTransTaxDetailsQuery selectCount
                                                              taxReportType
                                                              (Just taxReportStart)
                                                              taxReportEnd
                            ) 
  [Single before] <- runTaggedSql (buildPendingTransTaxDetailsQuery selectCount
                                                             taxReportType
                                                             Nothing
                                                             ((-1) `addDays` taxReportStart)
                           )

  return (before, withinPeriod)                                                                                     

buildPendingTransTaxDetailsQuery :: Tagged e Text -> Text -> Maybe Day -> Day -> (Tagged e Text, [PersistValue])
buildPendingTransTaxDetailsQuery (Tagged selectQuery) reportType startDate endDate = 

  let sql0 =  selectQuery <> " FROM 0_trans_tax_details fad"
          <>  " LEFT JOIN (SELECT d.* FROM fames_tax_report_detail d "
          <>  "            JOIN fames_tax_report USING(tax_report_id) WHERE type = ?) AS rd ON (rd.tax_trans_detail_id = fad.id) "
          <>  " LEFT JOIN 0_debtor_trans AS dt ON (fad.trans_no = dt.trans_no AND fad.trans_type = dt.type)"
          <>  " LEFT JOIN 0_supp_trans AS st ON (fad.trans_no = st.trans_no AND fad.trans_type = st.type)"
          -- already reported and aggregated
          <> " LEFT JOIN (  "
          <> "            SELECT tax_trans_detail_id, SUM(net_amount) net_amount, SUM(tax_amount) tax_amount  "
          <> "            FROM fames_tax_report_detail rd0  "
          <> "            JOIN fames_tax_report r USING (tax_report_id) "
          <> "            WHERE type = ?  "
          <> "            GROUP BY tax_trans_detail_id  "
          <> " ) rd0  ON (rd0.tax_trans_detail_id = fad.id) "
          <> " WHERE ((rd.tax_report_id is NULL AND (abs(fad.amount) > 1e-4 OR abs(fad.net_amount) > 1e-4 )) " -- not accounting or null
          <> "        OR ( " 
          <> "           abs (fad.net_amount * ex_rate * " <> transSignSQL <> " - rd0.net_amount) > 1e-4 "
          <> "                OR    abs (fad.amount * ex_rate * " <> transSignSQL <> "- rd0.tax_amount) > 1e-4 "
          <> "               )"
          <> "       ) AND fad.tran_date <= ?"
          <> "       AND fad.trans_type != " <> tshow (fromEnum ST_CUSTDELIVERY) <> " "
      orderBy = " ORDER BY fad.id "
      p0 = [ toPersistValue $ reportType
           , toPersistValue $ reportType
           , toPersistValue $ endDate
           ]
  in first Tagged $ case startDate of
                Nothing -> (sql0 <> orderBy, p0)
                Just start -> (sql0 <> " AND fad.tran_date >= ? " <> orderBy, p0 <> [toPersistValue start])

buildCollectedTaxDetailsQuery :: Tagged e Text -> Key TaxReport -> (Tagged e Text, [PersistValue])
buildCollectedTaxDetailsQuery (Tagged selectQuery) reportKey = 
  let sql0 =  selectQuery <> " FROM 0_trans_tax_details fad"
          <>  " JOIN fames_tax_report_detail rd ON (rd.tax_trans_detail_id = fad.id) "
          <>  " JOIN fames_tax_report r USING (tax_report_id) "
          <>  " LEFT JOIN 0_debtor_trans AS dt ON (fad.trans_no = dt.trans_no AND fad.trans_type = dt.type)"
          <>  " LEFT JOIN 0_supp_trans AS st ON (fad.trans_no = st.trans_no AND fad.trans_type = st.type)"
          -- we need all the report detail related to a transaction related to the current report
          -- the next join acts as a where clause
          <>  " JOIN (SELECT d.*, type FROM fames_tax_report_detail d JOIN fames_tax_report r0 USING(tax_report_id)) rd0 ON (rd0.tax_trans_detail_id = fad.id and rd0.tax_report_id = ? AND rd0.type = r.type) "
      orderBy = " ORDER BY fad.id "
      p0 = keyToValues reportKey
           
  in (Tagged $ sql0 <> orderBy, p0)


-- | FA store all transactions detail with a positive amount
-- To match the report detail, transaction needs to be reversed (or not)
-- so that the amount is positive for output (tax to pay)
transSignSQL :: Text
transSignSQL = "IF(fad.trans_type IN ( " <> inputTrans <> "), -1, 1)" where
  inputTrans = inTypes [ ST_CUSTCREDIT, ST_CUSTPAYMENT -- refund
                       , ST_SUPPINVOICE -- expenditure ST_BANKPAYMENT already -ve
                       , ST_JOURNAL -- expenditures too
                       ]

selectTt'MRd :: Tagged (Entity FA.TransTaxDetail, (Maybe (Entity TaxReportDetail), Single (Maybe Int64))) Text
selectTt'MRd = let
  faSelect = "`fad`.`id`, `fad`.`trans_type`, `fad`.`trans_no`, `fad`.`tran_date`, `fad`.`tax_type_id`, `fad`.`rate`, `fad`.`ex_rate`, `fad`.`included_in_price`, "
    <> transSignSQL <> " * `fad`.`net_amount`, "
    <> transSignSQL <> " * `fad`.`amount`, `fad`.`memo` "
  in Tagged $ "SELECT " <> faSelect <>  ", rd.* /* ?? */ /* ?? */, COALESCE(debtor_no, supplier_id) " -- hack to use 

selectTt'Rd :: Tagged (Entity FA.TransTaxDetail, (Entity TaxReportDetail, Single (Maybe Int64))) Text
selectTt'Rd = retag  selectTt'MRd

selectCount :: Tagged (Single Int) Text
selectCount = Tagged "SELECT count(*) "

runTaggedSql :: RawSql r => (Tagged r Text, [PersistValue]) -> SqlHandler [r]
runTaggedSql (Tagged sql, params) = rawSql sql params

loadPendingTaxDetails :: Rule -> Entity TaxReport -> Handler [TaxDetail]
loadPendingTaxDetails taxRule (Entity reportKey TaxReport{..}) = do
  custCategoryFinder <- customerCategoryFinderCached
  let mkDetail trans'detail''personms   =
        let
          ((trans,_) , _) = nuncons trans'detail''personms
          (_, detail'personms) = unzip $ toNullable trans'detail''personms
          (details, personms) = unzip detail'personms
          bucketFn t p =  applyTaxRule taxRule (faTransToRuleInput custCategoryFinder t p)
          personm = asum $ map unSingle personms
        in taxDetailFromDetails bucketFn reportKey trans (catMaybes details)  personm
  allDetails <- loadTaxDetail  mkDetail
                (entityKey . fst)
                (buildPendingTransTaxDetailsQuery selectTt'MRd
                                                  taxReportType
                                                  Nothing
                                                  taxReportEnd
                )
  return $ filter tdIsPending allDetails

loadCollectedTaxDetails (Entity reportKey TaxReport{..})  = do
  let mkDetail trans'detail''personms   =
        let
          ((trans,_) , _) = nuncons trans'detail''personms
          (_, detail'personms) = unzip $ toNullable trans'detail''personms
          (details, personms) = unzip detail'personms
          [mainDetail] = filter ((== reportKey) . taxReportDetailReport . entityVal ) details
          -- use bucket from current detail
          bucketFn _ _ =  taxReportDetailBucket (entityVal mainDetail)
          personm = asum $ map unSingle personms
        in taxDetailFromDetails bucketFn reportKey trans (details)  personm
  allDetails <- loadTaxDetail mkDetail
                (entityKey . fst)
                (buildCollectedTaxDetailsQuery selectTt'Rd
                                                    reportKey 
                )
  return allDetails

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
                           , let taxType = fromMaybe (mkTaxEntity taxId tax ) $ lookup taxId rateMap
                           ]
  

-- | Load transactions Report details For datatable
{-# NOINLINE getGLTaxReportDetailsR #-}
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
  
loadTaxDetail :: (RawSql e, Ord k, Eq k) 
              => (NonNull [e] -> Either Text TaxDetail)
              -> (e -> k)
              -> (Tagged e Text, [PersistValue])
              -> Handler [TaxDetail]
loadTaxDetail mkDetail key query  = do
  rows <- runDB $ runTaggedSql $ query
  -- group row by tax_trans_id
  let grouped = groupBy ((==) `on` key ) $ sortOn key rows
  case mapM (mkDetail) (mapMaybe fromNullable grouped) of
    Left err -> do
       setError "Inconsistent DB contact your administrator"
       error (unpack err)
    Right details -> return details

loadSavedBoxes :: Key TaxReport -> SqlHandler ([Entity TaxReportBox])
loadSavedBoxes reportKey = do
  selectList [TaxReportBoxReport ==. reportKey] [Asc TaxReportBoxId]

loadTaxBoxes :: TaxProcessor -> Key TaxReport -> SqlHandler [(TaxBox, Decimal)]
loadTaxBoxes processor reportKey = do
  savedBoxes <- loadSavedBoxes reportKey
  bucket'rateMap <- loadBucketSummary reportKey 
  let buckets = setFromList (map fst $ keys bucket'rateMap) :: Set Bucket
  boxes <- getBoxes processor buckets
  let
    -- boxes are the one we loaded + the one from settings
    taxBoxMap = (mapFromList $ map (fanl tbName) boxes) :: Map Text TaxBox
    taxBox0 name = TaxBox name Nothing Nothing (TaxBoxSum []) Nothing
    mkTaxBox'Amount (Entity _ TaxReportBox{..}) = (box, toDecimalWithRounding rounding taxReportBoxValue ) where
      box = fromMaybe (taxBox0 taxReportBoxName) $ lookup taxReportBoxName taxBoxMap 
      rounding = tbRound0 box
  return $ map mkTaxBox'Amount savedBoxes


-- ** Saving
-- | Mark the report as done and save its boxes
closeReport  processor report = do
  bucket'rates <- getBucketRateFromConfig report
  runDB $ do
      when (taxReportStatus (entityVal report) == Process) $ do
        error "Report already closed"
    -- load
      buckets <- loadBucketSummary $ entityKey report
      boxes <- getBoxes processor  (setFromList $ map fst $ keys buckets)
      -- mapM traceShowM (mapToList buckets)
      -- mapM traceShowM boxes
      let
        box'amounts = computeBoxes (setFromList $ keys bucket'rates) buckets boxes
        mkBox (TaxBox{..}, amount) = TaxReportBox{..} where
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
  
  

  

-- * Util
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
  

formatDouble' = F.sformat (commasFixedWith round 4)

formatDoubleWithSign amount = [shamlet|<span :negative:.text-danger>#{formatDouble' amount}|]
  where negative = amount < -1e-2

formatDecimal :: Decimal -> Text
formatDecimal x = F.sformat commasDecimal x

data BucketType = ConfigBucket | ExtraBucket deriving (Eq, Read, Show, Enum, Bounded)
-- | Get the list of all  Bucket/rate configuration from the config
-- (and the rate in the database)
getBucketRateFromConfig :: Entity TaxReport -> Handler (Map (Bucket, Entity FA.TaxType) BucketType )
getBucketRateFromConfig report = do
  (settings, _) <- unsafeGetReportSettings (taxReportType $ entityVal report)

  runDB $ do
    taxEntities <- selectList [FA.TaxTypeInactive ==. False] []
    let configBucket'rates = computeBucketRates (rules settings) $ mapFromList [ (key, e)
                                                                               | (Entity key e) <- taxEntities
                                                                               ]
    bucket'rates <- loadBucketSummary (entityKey report)

    return $ mapFromList (zip (toList configBucket'rates)
                                          (repeat ConfigBucket)
                                     ) <> (const ExtraBucket <$> bucket'rates)
  


-- * Rule
faTransToRuleInput :: (Text -> FA.DebtorsMasterId -> Maybe Text) -> FA.TransTaxDetail -> Maybe Int64 -> RuleInput
faTransToRuleInput custCategoryFinder FA.TransTaxDetail{..} riEntity = let
  riTransType = maybe (error "DB Problem") toEnum transTaxDetailTransType
  riTaxType = fromIntegral $ transTaxDetailTaxTypeId
  riTaxRate = transTaxDetailRate -- 1% = 1
  riAmount = transTaxDetailAmount
  riCustCategoryFinder category = do -- Maybe
    guard (isCustomer riTransType)
    customer <- riEntity
    custCategoryFinder category (FA.DebtorsMasterKey $ fromIntegral customer)
  in RuleInput{..}


computeBoxes :: Set (Bucket, Entity FA.TaxType) -> Map (Bucket, Entity FA.TaxType) TaxSummary -> [TaxBox] -> [(TaxBox, Decimal )]
computeBoxes bucket'rates bucketMap boxes = let
  values = map (computeBox bucket'rates bucketMap) boxes
  in case lefts values of
    [] -> zip boxes $ rights values
    errors -> error $ unpack $ intercalate "\n" errors

computeBox :: Set (Bucket, Entity FA.TaxType) -> Map (Bucket, Entity FA.TaxType) TaxSummary -> TaxBox -> Either Text Decimal 
computeBox bucket'rates bucketMap1 box = let
  -- initialize all bucket with 0
  bucketMap0 = mapFromList $ map (, mempty) (toList bucket'rates)
  bucketMap = unionWith (<>) bucketMap1 bucketMap0
  buckets = groupAsMap (fst . fst) snd $ mapToList bucketMap
  in computeBoxAmount (tbRule box) (tbRound0 box) buckets

reportDetailToBucketMap :: Map FA.TaxTypeId (Entity FA.TaxType)
                        -> TaxReportDetail
                        -> Map (Bucket, Entity FA.TaxType) TaxSummary
reportDetailToBucketMap taxTypeMap detail@TaxReportDetail{..} = let
  taxType = fromMaybe (mkTaxFromDetail detail) $ lookup (FA.TaxTypeKey taxReportDetailFaTaxType) taxTypeMap
  key = (taxReportDetailBucket, taxType)
  in singletonMap key (taxSummary detail)

-- | Create a tax entity in case the one used as been deleted
mkTaxFromDetail TaxReportDetail{..} = mkTaxEntity (FA.TaxTypeKey taxReportDetailFaTaxType) taxReportDetailRate

mkTaxEntity taxId rate = Entity taxId FA.TaxType{..} where
  taxTypeRate = rate
  taxTypeSalesGlCode = "0"
  taxTypePurchasingGlCode = "0"
  taxTypeName = "<deleted>"
  taxTypeInactive = True