{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module FAxx88 where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/tables/xx88")
