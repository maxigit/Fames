-- | For TH stage problem
module ModelField where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
-- | Where as a transaction has been processed or not.
data PendingStatus = Pending | Process deriving (Eq, Read, Show, Enum, Bounded, Ord)

derivePersistField "PendingStatus"



-- Trick to use in Persistent model declaration
-- as using normal tuples doesn't seem to work
type Pair a b = (a,b)
type Pairs a b = [(a,b)]

