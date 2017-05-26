{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module FAxx84 where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/tables/xx84")
