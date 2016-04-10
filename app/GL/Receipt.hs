-- | Event corresponding to a receipt.
-- It translates in FA to a payment or a supplier invoice.
module GL.Receipt
  ( Amount
  , TaxType(..)
  , Receipt(..)
  , ReceiptItem(..)
  , translate
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import Data.Ratio
import qualified Data.Map as Map


-- * General types
type Amount = Rational
-- | Rate dimensionless number. Used for tax rates.
type Rate = Rational

type GLAccount = Int


-- | Tax type 
data TaxType = TaxType
  { taxRate :: Rate
  , taxAccount :: GLAccount
  } deriving (Read, Show, Eq, Ord)

-- *  Receipt

data Receipt = Receipt { items :: [ReceiptItem] }
data ReceiptItem = ReceiptItem
  { glAccount :: GLAccount
  , amount :: Amount
  , taxType :: TaxType
  }
  deriving (Read, Show, Eq)

-- * Translation to FA objects
translate receipt = FA.Payment (nets ++ taxes) where
  groupByTax =
    Map.fromListWith (<>)
                 [ (taxType , [FA.PaymentItem glAccount (f amount) Nothing Nothing Nothing Nothing])
                 | (ReceiptItem {..}) <- items receipt
                 ]

  nets = concat (Map.elems groupByTax)

  -- tax =  FA.PaymentItem taxAccount (f $ amount*rate')  (Just . f $ amount) Nothing Nothing Nothing
  taxes = []
  f = fromRational
  -- TaxType rate' taxAccount  = taxType

         



