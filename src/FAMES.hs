{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies, UndecidableInstances, DataKinds #-}
-- Not needed at it duplicates all classes from model
-- Just easier to get a quick view of db
module FAMES where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

  
share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/fames-models")
