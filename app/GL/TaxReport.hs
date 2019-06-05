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
-- * Types
-- | 
data TaxSummary = TaxSummary
   { netAmount :: Amount
   , taxAmount :: Amount
   } deriving (Eq, Read, Show, Ord)

grossAmount TaxSummary{..} = netAmount + taxAmount
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
class HasTaxAmounts a where
  taxSummary :: a -> TaxSummary

instance HasTaxAmounts FA.TransTaxDetail where
  taxSummary FA.TransTaxDetail{..} = let
    ex = (* transTaxDetailExRate)
    in TaxSummary (ex transTaxDetailNetAmount) (ex transTaxDetailAmount)
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

isCustomer = (`elem` customerFATransactions)
isSupplier = (`elem` supplierFATransactions)

-- * Boxes
computeBoxAmount :: TaxBoxRule -> Map Bucket TaxSummary -> Either Text Amount
computeBoxAmount rule0 buckets = case rule0 of
    TaxBoxSum rules -> sum <$> mapM amountFor rules
    TaxBoxNet bucket -> findBucket bucket netAmount 
    TaxBoxTax bucket -> findBucket bucket taxAmount
    TaxBoxGross bucket -> findBucket bucket (grossAmount)
    TaxBoxSub rule1 rule2 -> liftA2 (-) (amountFor rule1) (amountFor rule2)
    TaxBoxNegate rule -> negate <$>  amountFor rule
    TaxBoxTaxWith rate bucket -> (\a -> a * rate / 100) <$> findBucket bucket netAmount
    TaxBoxFloor rule -> fromIntegral . floor <$>  amountFor rule
    TaxBoxCeil rule -> fromIntegral . ceiling <$> amountFor rule
    TaxBoxRound rule -> Left "not implemented yet"
    TaxBoxBanker rule -> fromIntegral . round <$> amountFor rule
  where amountFor rule = computeBoxAmount rule buckets
        findBucket bucket fn = case lookup bucket buckets of
          Nothing -> Left $ "Bucket: " <> bucket <> " dosen't exit."
          Just summary -> Right $ fn summary

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
