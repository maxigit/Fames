{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module DC154 where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/dc-tables/dcx154")
