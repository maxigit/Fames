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

-- * Types
-- | 
data TaxSummary = TaxSummary
   { netAmount :: Amount
   , taxAmount :: Amount
   } deriving (Eq, Read, Show, Ord)

-- * Instance
instance Semigroup TaxSummary where
  (TaxSummary net tax) <> (TaxSummary net' tax') = TaxSummary (net + net') (tax + tax')
instance Monoid TaxSummary where
  mempty = TaxSummary 0 0
  mappend = (<>)

-- type TaxBins = (Map TaxBin TaxSummary)
-- Everything which has a net and tax amount
class HasTaxAmounts a where
  taxSummary :: a -> TaxSummary

instance HasTaxAmounts FA.TransTaxDetail where
  taxSummary FA.TransTaxDetail{..} = let
    ex = (* transTaxDetailExRate)
    in TaxSummary (ex transTaxDetailNetAmount) (ex transTaxDetailAmount)
-- * Rules
infix 4 *==
Nothing *== _ = True
a *== b = a == b
applyTaxRule :: Rule -> RuleInput -> Bucket
applyTaxRule rule input = fromMaybe defaultBucket (applyTaxRuleM rule input)
applyTaxRuleM :: Rule -> RuleInput -> Maybe Bucket
applyTaxRuleM (BucketRule bucket) _ = Just bucket
applyTaxRuleM (RuleList rules ) input = asum $ [ applyTaxRuleM rule input | rule <- rules ]
applyTaxRuleM (TransactionRule ttype rule ) input = 
  guard (ttype == riTransType input) >> (applyTaxRuleM rule input)
applyTaxRuleM (CustomerRule cust rule ) input = 
  guard (isCustomer (riTransType input) && cust *== riEntity input) >> (applyTaxRuleM rule input)
applyTaxRuleM (SupplierRule cust rule ) input = 
  guard (isSupplier (riTransType input) && cust *== riEntity input) >> (applyTaxRuleM rule input)
applyTaxRuleM (TaxTypeRule taxtype rule ) input = 
  guard (taxtype == riTaxType input ) >> (applyTaxRuleM rule input)
applyTaxRuleM (TaxRateRule taxrate rule ) input = 
  guard (taxrate == riTaxRate input ) >> (applyTaxRuleM rule input)

isCustomer = (`elem` customerFATransactions)
isSupplier = (`elem` supplierFATransactions)
