{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies, UndecidableInstances, DataKinds, TypeOperators #-}
module FAX where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

  
share [mkPersist sqlSettings] -- , mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/fax-models")
