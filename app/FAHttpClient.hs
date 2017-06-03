-- | Utility function to Post requests to FA via Http
module FAHttpClient
(
)
where

import ClassyPrelude

data FATransactionType
  = InventoryAdjustment
  | InventoryTransfer
  deriving (Eq, Show, Enum)

data FATransaction = FATransaction
  { transType :: !FATransactionType
  , transId :: !Int
  } deriving (Eq, Show)

data PostRequestResult
  = Success (Maybe FATransaction)
  | Failure Text
  deriving (Eq, Show)



-- | Log to FrontAccounting and post a set of event
-- 
withSession :: a -> IO a
withSession = undefined




