--  | Types corresponding to stock related Frontaccounting concepts.
module WH.FA.Types where

import ClassyPrelude

-- * StockAdjustment
-- | Information needed to post a FA stock adjustment
data StockAdjustment = StockAdjustment
  { adjReference :: !Text
  , adjLocation :: !Text
  , adjDate :: !Day
  , adjDetails :: [StockAdjustmentDetail]
  , adjAdjType :: AdjustmentType
  } deriving (Eq, Show)

data StockAdjustmentDetail = StockAdjustmentDetail
  { adjSku :: !Text
  , adjQuantity :: !Double
  , adjCost :: !Double
  } deriving (Eq, Show)

data AdjustmentType = PositiveAdjustment  | NegativeAdjustment
  deriving (Eq, Show, Enum)

-- * Location Transfer
-- | needed to post a FA stock adjustment
data LocationTransfer = LocationTransfer
  { ltrReference:: !Text
  , ltrLocationFrom :: !Text
  , ltrLocationTo :: !Text
  , ltrDate1 :: !Day
  , ltrDetails1 :: [LocationTransferDetail]
  } deriving (Eq, Show)

data LocationTransferDetail = LocationTransferDetail
  { ltrSku :: !Text
  , ltrQuantity :: !Int
  } deriving (Eq, Show)
