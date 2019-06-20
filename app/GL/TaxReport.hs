module GL.TaxReport where
import ClassyPrelude 
import qualified FA as FA
import GL.FA
import GL.Utils
-- import qualified GL.FA as FA
import Data.Aeson
import Import.NoFoundation
import GL.TaxReport.Settings as GL.TaxReport
import GL.TaxReport.Types 
import Database.Persist.Sql (fromSqlKey)
import Util.Decimal
-- * Types
-- | 
data TaxSummary = TaxSummary
   { netAmount :: Amount
   , taxAmount :: Amount
   } deriving (Eq, Read, Show, Ord)

grossAmount TaxSummary{..} = netAmount + taxAmount

reverseTaxSummary :: TaxSummary -> TaxSummary
reverseTaxSummary TaxSummary{..} = TaxSummary (-netAmount) (-taxAmount)

data TaxReportStatus
  = Early -- ^ before start date
  | Open -- ^ 
  | Ready -- ^ to be processed
  | Closed -- ^ close , read to submit
  | Late Day -- ^ with deadline
  | Submitted UTCTime -- ^ With submissioon date
  deriving (Eq, Show, Read)
-- * Instance
instance Semigroup TaxSummary where
  (TaxSummary net tax) <> (TaxSummary net' tax') = TaxSummary (net + net') (tax + tax')
instance Monoid TaxSummary where
  mempty = TaxSummary 0 0
  mappend = (<>)

deriving instance Eq FA.TaxType
deriving instance Ord FA.TaxType
-- type TaxBins = (Map TaxBin TaxSummary)
-- Everything which has a net and tax amount
class HasTaxSummary a where
  taxSummary :: a -> TaxSummary

instance HasTaxSummary FA.TransTaxDetail where
  taxSummary FA.TransTaxDetail{..} = let
    ex = (* transTaxDetailExRate)
    in TaxSummary (ex transTaxDetailNetAmount) (ex transTaxDetailAmount)
instance HasTaxSummary TaxReportDetail where
  taxSummary TaxReportDetail{..} = TaxSummary taxReportDetailNetAmount taxReportDetailTaxAmount

instance HasTaxSummary e => HasTaxSummary (Entity e) where
  taxSummary = taxSummary . entityVal

-- * Rules
infix 4 *==, *==?
[] *== _ = True
as *== b = b `elem` as
_ *==? Nothing = False
as *==? (Just b) = as *== b


applyTaxRule :: Rule -> RuleInput -> Bucket
applyTaxRule rule input = fromMaybe defaultBucket (applyTaxRuleM rule input)
applyTaxRuleM :: Rule -> RuleInput -> Maybe Bucket
applyTaxRuleM (BucketRule bucket) _ = Just bucket
applyTaxRuleM (RuleList rules ) input = asum $ [ applyTaxRuleM rule input | rule <- rules ]
applyTaxRuleM (TransactionRule ttype rule ) input = 
  guard (ttype *== riTransType input) >> (applyTaxRuleM rule input)
applyTaxRuleM (CustomerRule cust rule ) input = 
  guard (isCustomer (riTransType input) && cust *==? riEntity input) >> (applyTaxRuleM rule input)
applyTaxRuleM (SupplierRule cust rule ) input = 
  guard (isSupplier (riTransType input) && cust *==? riEntity input) >> (applyTaxRuleM rule input)
applyTaxRuleM (TaxTypeRule taxtype rule ) input = 
  guard (taxtype *== riTaxType input ) >> (applyTaxRuleM rule input)
applyTaxRuleM (TaxRateRule taxrate rule ) input = 
  guard (taxrate *== riTaxRate input ) >> (applyTaxRuleM rule input)
applyTaxRuleM (InputRule rule ) input = 
  guard (riAmount input < 0) >> (applyTaxRuleM rule input)
applyTaxRuleM (OutputRule rule ) input = 
  guard (riAmount input > 0) >> (applyTaxRuleM rule input)

isCustomer = (`elem` customerFATransactions)
isSupplier = (`elem` supplierFATransactions)

-- * Boxes
computeBoxAmount :: TaxBoxRule -> RoundingMethod -> Map Bucket TaxSummary -> Either Text Decimal
computeBoxAmount rule rounding buckets = applyRounding rounding <$> go rule buckets
  where
    go rule0 buckets = case rule0 of
      TaxBoxSum rules -> sum <$> mapM amountFor rules
      TaxBoxNet bucket -> findBucket bucket netAmount 
      TaxBoxTax bucket -> findBucket bucket taxAmount
      TaxBoxGross bucket -> findBucket bucket (grossAmount)
      TaxBoxSub rule1 rule2 -> liftA2 (-) (amountFor rule1) (amountFor rule2)
      TaxBoxNegate rule -> negate <$>  amountFor rule
      TaxBoxTaxWith rate bucket -> (\a -> a *. (rate / 100)) <$> findBucket bucket netAmount
      TaxBoxFloor dec rule -> applyRounding (RoundDown dec) <$>  amountFor rule
      TaxBoxCeil dec rule -> applyRounding (RoundUp dec)<$> amountFor rule
      TaxBoxRound dec rule -> applyRounding (Round dec)<$> amountFor rule
      TaxBoxBanker dec rule -> applyRounding (RoundBanker dec) <$> amountFor rule
    amountFor :: TaxBoxRule -> Either Text Decimal
    amountFor rule = realFracToDecimal tbDefaultDecimal  <$> go rule buckets
    findBucket bucket fn = case lookup bucket buckets of
          Nothing -> Left $ "Bucket: " <> bucket <> " doesn't exit."
          Just summary -> Right . realFracToDecimal tbDefaultDecimal $ fn summary

deriving instance (Show FA.TaxType)

-- * Misc
-- | Return the list of bucket rates possible combination
computeBucketRates :: Rule -> Map FA.TaxTypeId FA.TaxType -> Set (Bucket, Entity FA.TaxType)
computeBucketRates rule0 rateMap = let
  rates :: [Entity FA.TaxType]
  rates = map (uncurry Entity) $ mapToList rateMap
  rateIds = map (fromIntegral . FA.unTaxTypeKey . entityKey) rates
  go :: Rule -> [(Bucket, Entity FA.TaxType)]
  go (BucketRule bucket) = map (bucket,) rates
  -- for tax rules
  -- transform what's next as if it was an else clause
  -- ie a rules with the other tax
  go (RuleList []) = []
  go (RuleList (rule:rules)) = go rule <> case rule of
    TaxTypeRule taxIds sub |  ruleCatchesAll sub   -> let otherIds = filter (`notElem` taxIds) rateIds
                              in go (TaxTypeRule otherIds (RuleList rules))
    _ -> go (RuleList rules) 
  go (TransactionRule _ rule) = go rule
  go (CustomerRule _ rule)  = go rule
  go (SupplierRule _ rule) = go rule
  go (TaxTypeRule  taxIds rule) = filter ((taxIds *==) . fromIntegral . FA.unTaxTypeKey . entityKey . snd) $ go rule
  go (TaxRateRule rates   rule) = filter ((rates *==) . FA.taxTypeRate . entityVal . snd) $ go rule
  go (InputRule rule) = go rule
  go (OutputRule rule) = go rule
  in setFromList (go rule0)

-- | return true if it catches everyting
-- usefull to now if a config is complete or not
ruleCatchesAll :: Rule -> Bool
ruleCatchesAll (BucketRule _) = True
ruleCatchesAll (RuleList rules) = any ruleCatchesAll rules
-- ruleCatchesAll (TransactionRule [] rule) = ruleCatchesAll rule
-- ruleCatchesAll (SupplierRule [] rule) = ruleCatchesAll rule
-- ruleCatchesAll (CustomerRule [] rule) = ruleCatchesAll rule
-- ruleCatchesAll (TaxTypeRule [] rule) = ruleCatchesAll rule
-- ruleCatchesAll (TaxRateRule [] rule) = ruleCatchesAll rule
ruleCatchesAll _ = False

-- * Status

taxReportDeadline :: TaxReportSettings -> TaxReport -> Day
taxReportDeadline TaxReportSettings{..} TaxReport{..} = maybe id calculateDate deadline  $ taxReportEnd

--                          |start|                 |end|          |deadline|
-- open:     too early                 current             read(orange)y                  late
-- close:                                                  to submit(orgg)              late
-- submitted:                                                                       ok

taxReportDateStatus :: TaxReportSettings -> Day -> TaxReport -> TaxReportStatus
taxReportDateStatus settings today report@TaxReport{..} = let
  deadline' =  taxReportDeadline settings report
  in case (taxReportStatus, taxReportSubmittedAt) of
    (_, Just submitted) -> Submitted submitted
    (_ , _) | today < taxReportStart -> Early
    (_, _) | today > deadline' -> Late deadline'
    (_, _) | today < taxReportEnd -> Open
    -- between end and deadline
    (Pending,_) -> Ready
    (Process,_) -> Closed
