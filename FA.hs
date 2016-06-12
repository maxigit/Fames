{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module FA where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

  
share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/fa-models")
