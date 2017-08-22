{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module FAxx97 where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/tables/xx97")
