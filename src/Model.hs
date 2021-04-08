{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import ModelField
import Data.Decimal
import Data.ISO3166_CountryCodes

import qualified FA as FA
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      , mkDeleteCascade sqlSettings
      ]
    $(persistFileWith lowerCaseSettings "config/models")

