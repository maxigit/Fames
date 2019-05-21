module GL.TaxReport
where
import ClassyPrelude 
import qualified FA as FA
import GL.FA
import GL.Utils
-- import qualified GL.FA as FA
import Data.Aeson
import Import.NoFoundation

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
