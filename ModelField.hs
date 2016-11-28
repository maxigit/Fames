-- | For TH stage problem
module ModelField where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
-- | Where as a transaction has been processed or not.
data PendingStatus = Pending | Process deriving (Eq, Read, Show, Enum, Bounded, Ord)

derivePersistField "PendingStatus"


