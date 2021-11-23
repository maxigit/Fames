{-# OPTIONS_GHC -Wno-orphans #-}
module GL.TaxReport where
import ClassyPrelude 
import qualified FA
import GL.FA
import GL.Utils
-- import qualified GL.FA as FA

import Import.NoFoundation hiding(CustomerCategory)
import GL.TaxReport.Settings as GL.TaxReport
import GL.TaxReport.Types 

import Util.Decimal
-- * Types 
-- | 
data TaxSummary = TaxSummary
   { netAmount :: Amount
   , taxAmount :: Amount
   } deriving (Eq, Read, Show, Ord)

grossAmount :: TaxSummary -> Amount
grossAmount TaxSummary{..} = netAmount + taxAmount

reverseTaxSummary :: TaxSummary -> TaxSummary
reverseTaxSummary TaxSummary{..} = TaxSummary (-netAmount) (-taxAmount)

data TaxReportStatus
  = Early --  ^ before start date
  | Open --  ^ 
  | Ready --  ^ to be processed
  | Closed --  ^ close , read to submit
  | Late Day --  ^ with deadline
  | Submitted UTCTime --  ^ With submissioon date
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
(*==) :: Eq a => [a] -> a -> Bool
[] *== _ = True
as *== b = b `elem` as
(*==?) :: Eq a => [a] -> Maybe a -> Bool
_ *==? Nothing = False
as *==? (Just b) = as *== b


applyTaxRule :: Rule -> RuleInput -> Bucket
applyTaxRule rule_ input = fromMaybe defaultBucket (applyTaxRuleM rule_ input)
applyTaxRuleM :: Rule -> RuleInput -> Maybe Bucket
applyTaxRuleM (BucketRule bucket) _ = Just bucket
applyTaxRuleM (RuleList rules ) input = asum $ [ applyTaxRuleM rule_ input | rule_ <- rules ]
applyTaxRuleM (TransactionRule ttype rule_ ) input = 
  guard (ttype *== riTransType input) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (CustomerRule cust rule_ ) input = 
  guard (isCustomer (riTransType input) && cust *==? riEntity input) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (SupplierRule cust rule_ ) input = 
  guard (isSupplier (riTransType input) && cust *==? riEntity input) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (TaxTypeRule taxtype rule_ ) input = 
  guard (taxtype *== riTaxType input ) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (TaxRateRule taxrate rule_ ) input = 
  guard (taxrate *== riTaxRate input ) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (InputRule rule_ ) input = 
  guard (riAmount input < 0) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (OutputRule rule_ ) input = 
  guard (riAmount input > 0) >> (applyTaxRuleM rule_ input)
applyTaxRuleM (CustomerCategory category suffix) input = do
  guard (isCustomer (riTransType input))
  case riCustCategoryFinder input category of
    Just "" -> Nothing -- Just $ cat <> maybe "" (";" <>) suffix
    Just cat -> Just $ cat <> maybe "" (";" <>) suffix
    Nothing -> Nothing
    -- _ -> error . unpack $ "Customer Category '" <> category <> "' for customer " <> tshow (riEntity input) <> ". Please refresh customer categories"

isCustomer, isSupplier :: FATransType -> Bool
isCustomer = (`elem` customerFATransactions)
isSupplier = (`elem` supplierFATransactions)

-- * Boxes 
computeBoxAmount :: TaxBoxRule -> RoundingMethod -> Map Bucket TaxSummary -> Either Text Decimal
computeBoxAmount rule rounding buckets = applyRounding rounding <$> go rule
  where
    go rule0 = case rule0 of
      TaxBoxSum rules -> sum <$> mapM amountFor rules
      TaxBoxNet bucket -> findBucket bucket netAmount 
      TaxBoxTax bucket -> findBucket bucket taxAmount
      TaxBoxGross bucket -> findBucket bucket (grossAmount)
      TaxBoxSub rule1 rule2 -> liftA2 (-) (amountFor rule1) (amountFor rule2)
      TaxBoxNegate rule_ -> negate <$>  amountFor rule_
      TaxBoxTaxWith rate bucket -> (\a -> a *. (rate / 100)) <$> findBucket bucket netAmount
      TaxBoxFloor dec rule_ -> applyRounding (RoundDown dec) <$>  amountFor rule_
      TaxBoxCeil dec rule_ -> applyRounding (RoundUp dec)<$> amountFor rule_
      TaxBoxRound dec rule_ -> applyRounding (Round dec)<$> amountFor rule_
      TaxBoxBanker dec rule_ -> applyRounding (RoundBanker dec) <$> amountFor rule_
    amountFor :: TaxBoxRule -> Either Text Decimal
    amountFor rule_ = realFracToDecimal tbDefaultDecimal  <$> go rule_
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
  go (RuleList (rule_:rules)) = go rule_ <> case rule_ of
    TaxTypeRule taxIds sub |  ruleCatchesAll sub   -> let otherIds = filter (`notElem` taxIds) rateIds
                              in go (TaxTypeRule otherIds (RuleList rules))
    _ -> go (RuleList rules) 
  go (TransactionRule _ rule_) = go rule_
  go (CustomerRule _ rule_)  = go rule_
  go (SupplierRule _ rule_) = go rule_
  go (TaxTypeRule  taxIds rule_) = filter ((taxIds *==) . fromIntegral . FA.unTaxTypeKey . entityKey . snd) $ go rule_
  go (TaxRateRule rates_   rule_) = filter ((rates_ *==) . FA.taxTypeRate . entityVal . snd) $ go rule_
  go (InputRule rule_) = go rule_
  go (OutputRule rule_) = go rule_
  go (CustomerCategory _ _) = []
  in setFromList (go rule0)

-- | return true if it catches everyting
-- usefull to now if a config is complete or not
ruleCatchesAll :: Rule -> Bool
ruleCatchesAll (BucketRule _) = True
ruleCatchesAll (RuleList rules) = any ruleCatchesAll rules
-- ruleCatchesAll (TransactionRule [] rule_) = ruleCatchesAll rule_
-- ruleCatchesAll (SupplierRule [] rule_) = ruleCatchesAll rule_
-- ruleCatchesAll (CustomerRule [] rule_) = ruleCatchesAll rule_
-- ruleCatchesAll (TaxTypeRule [] rule_) = ruleCatchesAll rule_
-- ruleCatchesAll (TaxRateRule [] rule_) = ruleCatchesAll rule_
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
