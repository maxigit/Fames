-- | Event corresponding to a receipt.
-- It translates in FA to a payment or a supplier invoice.
-- The main difference between an event and an FA payment is :
-- an even can generate more than one payment and can have
-- and can abstract things like GLAccount and GLDimension into one abstraction (to decide)
module GL.Receipt
  ( Amount
  , TaxType(..)
  , Receipt(..)
  , ReceiptItem(..)
  , translate
  , ReceiptTemplate(..)
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import qualified Data.Map as Map


-- * General types
type Amount = Rational
-- | Rate dimensionless number. Used for tax rates.
type Rate = Rational
type GLAccount = Int
type Counterparty = Text
type BankAccount = Text


-- | Tax type 
data TaxType = TaxType
  { taxRate :: Rate
  , taxAccount :: GLAccount
  } deriving (Read, Show, Eq, Ord)

-- *  Receipt

data Receipt = Receipt
  { date :: Text
  , counterparty :: Counterparty
  , bankAccount :: BankAccount
  , totalAmount :: Amount
  , items :: [ReceiptItem]
  } deriving (Read, Show, Eq)

data ReceiptItem = ReceiptItem
  { glAccount :: GLAccount
  , amount :: Amount
  , taxType :: TaxType
  } deriving (Read, Show, Eq)

-- * Translation to FA objects
translate :: Receipt -> FA.Payment
translate receipt = FA.Payment (nets ++ taxes) where
  groupByTax =
    Map.fromListWith (<>)
                 [ (taxType item , [item])
                 | item <- items receipt
                 ]

  nets = [ FA.PaymentItem glAccount (f amount) Nothing Nothing Nothing Nothing
         | ReceiptItem {..} <- items receipt
         ]
           
  taxes = [ FA.PaymentItem taxAccount (f $ netAmount*rate)  (Just . f $ netAmount) Nothing Nothing Nothing
          | (TaxType rate taxAccount, items) <- Map.toList groupByTax

          , let netAmount = sum (map amount items)
          ]

  f = fromRational
  -- TaxType rate' taxAccount  = taxType

         
  


-- * Template
-- Templates allow to prefill or "guess" missing values from csv
-- such as counterparty, GLAccount but also compute Tax backward etc

data ReceiptTemplate
  = CounterPartySetter Text -- ^ set the counterparty
  | BankAccountSetter Text -- ^ set a bank account
  | CompoundTemplate [ReceiptTemplate] 
  | ItemVATDeducer  Double Text -- ^ compute the net and tax from the gross
  | ItemMemoSetter  Text
