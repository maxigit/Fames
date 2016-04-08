-- | Event corresponding to a receipt.
-- It translates in FA to a payment or a supplier invoice.
module GL.Receipt
  (Amount
  , Receipt(..)
  , translate
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import Data.Ratio


-- * General types
type Amount = Rational

-- *  Receipt

data Receipt = Receipt { amount :: Amount}
  deriving (Read, Show, Eq)

-- * Translation to FA objects
translate :: Receipt -> FA.Payment
translate = error "not implemented"

         



