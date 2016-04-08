-- | Event corresponding to a receipt.
-- It translates in FA to a payment or a supplier invoice.
module GL.Receipt
  ( Amount
  , TaxType(..)
  , Receipt(..)
  , translate
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import Data.Ratio


-- * General types
type Amount = Rational
-- | Rate dimensionless number. Used for tax rates.
type Rate = Rational

type GLAccount = Int


-- | Tax type 
data TaxType = TaxType
  { taxRate :: Rate
  , taxAccount :: GLAccount
  } deriving (Read, Show, Eq)

-- *  Receipt

data Receipt = Receipt
  { glAccount :: GLAccount
  , amount :: Amount
  , taxType :: TaxType
  }
  deriving (Read, Show, Eq)

-- * Translation to FA objects
translate :: Receipt -> FA.Payment
translate (Receipt {..}) = FA.Payment [net, tax] where
  net =  FA.PaymentItem glAccount (f amount) Nothing Nothing Nothing Nothing
  tax =  FA.PaymentItem taxAccount (f $ amount*rate')  (Just . f $ amount) Nothing Nothing Nothing
  f = fromRational
  TaxType rate' taxAccount  = taxType

         



