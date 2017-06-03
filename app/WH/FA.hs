-- | Exposes FrontAccounting Stock related concepts such as
-- StockAdjustment, StockTransfer.
-- There are not persistent but can be used to talked to FA
-- or loaded straight from the database.
-- This module is meant to be imported qualified (as FA) as some
-- of its names might conflict with more general ones.
module WH.FA (
module Types
)where

import WH.FA.Types as Types

