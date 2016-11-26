{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import qualified FA as FA
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/


-- | Where as a transaction has been processed or not.
data PendingStatus = Pending | Process deriving (Eq, Read, Show, Enum, Bounded, Ord)

instance PersistField PendingStatus

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
