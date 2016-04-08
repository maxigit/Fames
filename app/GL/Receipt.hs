-- | Event corresponding to a receipt.
-- It translates in FA to a payment or a supplier invoice.
module GL.Receipt
  (Amount
  , ratePercent
  , Receipt(..)
  , translate
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import Data.Ratio


-- * General types
type Amount = Rational

-- | Rate dimensionless number. Used for tax rates.
newtype  Rate = Rate Rational deriving (Read, Show, Eq, Ord)
ratePercent :: Rational -> Rate 
ratePercent p = Rate (p / 100)

-- *  Receipt

data Receipt = Receipt
  { glAccount :: Int
  , amount :: Amount
  , rate :: Rate
  }
  deriving (Read, Show, Eq)

-- * Translation to FA objects
translate :: Receipt -> FA.Payment
translate (Receipt {..}) = FA.Payment [net, tax] where
  net =  FA.PaymentItem glAccount (f amount) Nothing Nothing Nothing Nothing
  tax =  FA.PaymentItem 2200 (f $ amount*rate')  (Just . f $ amount) Nothing Nothing Nothing
  f = fromRational
  Rate rate' = rate

         



