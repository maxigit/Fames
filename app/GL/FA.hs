-- | Exposes FrontAccounting GL Concepts such as JournalEntry, GLAccount etc ...
-- It's a one to one mapping, so we there is not ADT etc ...
-- It can be used to talk to FA (via API or even forms)
-- or load them straight away from the database.
-- This module is meant to be imported qualified (as FA) as some
-- of it's concepts can conflicts with more general ones.
module GL.FA where

import ClassyPrelude

-- * General types
type Amount = Double -- that's how there are in FA
type GLAccount = Int  -- to change
type DimensionId = Int

-- * Payment related
-- | Correspond to the payment form to "Misc"
data Payment = Payment [PaymentItem] deriving (Show, Read, Eq)


-- | A row in the payment. Will generate one line in the General Ledger.
-- taxThroughput is the tax input or tax output, ie the net amount
-- a tax payment refers too. For example a payment of 100 + 20 VAT corresponds
-- to two items : one with an amount of 100 and no taxThroughput and one
-- with an amont of 20 (the TAX) and a taxThroughput of 100. (the 20 relates to 100)
data PaymentItem = PaymentItem
  { glAccount :: GLAccount
  , amount :: Amount
  , taxThroughput :: Maybe Amount
  , dimension1 :: Maybe DimensionId
  , dimension2 :: Maybe DimensionId
  , memo :: Maybe Text
  } deriving (Show, Read, Eq)
               




