{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module FAX where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

  
share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/fax-models")
