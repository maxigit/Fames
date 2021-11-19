{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies, UndecidableInstances, DataKinds #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.Sql

import ModelField
import Data.Decimal
import Data.ISO3166_CountryCodes

import qualified FA as FA
import qualified Language.Haskell.TH as TH
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      , mkDeleteCascade sqlSettings
      ]
    $(persistFileWith lowerCaseSettings "config/models")

$(do
  dss <- mapM
    (\eName -> do
       let eType  = return $ TH.ConT eName
           keyname = TH.nameBase eName ++ "Key"
       Just keyCon <- TH.lookupValueName $ keyname
       Just keyUn <- TH.lookupValueName $ "un" ++ keyname
       [d| instance ToBackendKey SqlBackend $eType where
            toBackendKey =  $(return $ TH.VarE keyUn)
            fromBackendKey =  $(return $ TH.ConE keyCon)
         |])
    [''PackingList
    ,''TaxReport
    ,''ItemCostValidation
    , ''StockAdjustment
    , ''Operator
    , ''Batch
    , ''User
    ]
  return $ concat dss
  )

instance ToBackendKey SqlBackend ShippingDetails where
  toBackendKey = unShippingDetailsKey
  fromBackendKey = ShippingDetailsKey'

