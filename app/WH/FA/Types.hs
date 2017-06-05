{-# LANGUAGE DuplicateRecordFields #-}
--  | Types corresponding to stock related Frontaccounting concepts.
module WH.FA.Types where

import ClassyPrelude

-- * StockAdjustment
-- | Information needed to post a FA stock adjustment
data StockAdjustment = StockAdjustment
  { reference :: !Text
  , location :: !Text
  , date :: !Day
  , details :: [StockAdjustmentDetail]
  , adjType :: AdjustmentType
  } deriving (Eq, Show)

data StockAdjustmentDetail = StockAdjustmentDetail
  { sku :: !Text
  , quantity :: !Double
  , cost :: !Double
  } deriving (Eq, Show)

data AdjustmentType = PositiveAdjustment  | NegativeAdjustment
  deriving (Eq, Show, Enum)

-- * Location Transfer
-- | needed to post a FA stock adjustment
data LocationTransfer = LocationTransfer
  { locationFrom :: !Text
  , locationTo :: !Text
  , date1 :: !Day
  , details1 :: [LocationTransferDetail]
  } deriving (Eq, Show)

data LocationTransferDetail = LocationTransferDetail
  { sku :: !Text
  , quantity :: !Int
  } deriving (Eq, Show)

