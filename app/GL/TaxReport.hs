module GL.TaxReport
( computeTaxBin
)
where

import ClassyPrelude 
import qualified FA as FA
import GL.FA
-- import qualified GL.FA as FA
import Data.Aeson
import Import.NoFoundation

-- * Dummy type
data ReportTransDetail = ReportTransDetail
-- * Types
data BinRule = -- TaxTypeMap (Map TaxRef BinRule) -- case
             -- | Bin TaxBin -- const
             UseTaxRef
             | NoBin -- empty bin
      
type ReportTransDetailDate = ( ReportTransDetail, Maybe Day )
data TransactionInfo = TransactionInfo
   { glEntries :: [ FA.GlTran ] -- ^ Gl entrie corresponding to the transaction
   , taxDetails :: [ FA.TransTaxDetail ] -- ^ transaction details from FA
   , reportDetails :: [ ReportTransDetail ] -- ^ What's been alread submitted
   }
data TaxBin = TaxBin Text
  deriving (Eq, Show, Read, Ord)

data BinError = BalanceError -- ^ amounts doesn't balance
              | NoBinError -- ^ no bin
data TaxSummary = TaxSummary
   { netAmount :: Amount
   , taxAmount :: Amount
   } deriving (Eq, Read, Show, Ord)

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

type RuleResult = Either BinError (Map TaxBin TaxSummary)

-- * Box

-- All bin must add up to the original amount.
-- The rules function deciding of the bin, should not use the
-- amounts (even though they have access to it). This is why
-- we calculate the taxSummary, separately from the bin, instead of having a function returin a bin and a summary.
-- This is to avoid amount to be used twice or 0.
computeTaxBin :: [BinRule] -> TransactionInfo  -> RuleResult
computeTaxBin rules info = do
  binsForDetails <- mapM (\td@FA.TransTaxDetail{..} ->  binForTaxDetail rules td <&> (, taxSummary td))  (taxDetails info)
  Right $ mkTaxBins binsForDetails

-- applyBinRule :: BinRule  -> TransactionInfo -> Either BinError TaxBins
-- applyBinRule rule info = do
--   binsForDetails <- mapM (binForTaxDetail rule)  (taxDetails info)
--   return mkTaxBins binsForDetails


binForTaxDetail :: [BinRule] -> FA.TransTaxDetail -> Either BinError TaxBin
binForTaxDetail _ FA.TransTaxDetail{..} = Right $ TaxBin ("#" <> tshow transTaxDetailTaxTypeId <> " " <> tshow transTaxDetailRate )
  
mkTaxBins :: [(TaxBin, TaxSummary)] -> Map TaxBin TaxSummary
mkTaxBins = groupAsMap fst snd 
-- applyBinRule (TaxTypeMap tmap) TransactionInfo{..} =  map (_applyTaxRule tmap ) taxDetails





  









