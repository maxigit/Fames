{-# LANGUAGE PartialTypeSignatures, ImplicitParams #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}

module BenchImport
    ( module BenchImport
    , module X
    ) where


import Import as X
import qualified Application  as A
import Database.MySQL.Base (ConnectInfo(..))
import Database.Persist.MySQL (MySQLConf(..))
import System.Environment(withArgs)



overrideDB :: AppSettings -> AppSettings
overrideDB settings = let
   dbConf = appDatabaseConf settings
   cinfo  = myConnInfo dbConf 
   in settings { appDatabaseConf = dbConf {myConnInfo = cinfo { connectPort = 3309
                                                              , connectUser = "root"
                                                              , connectHost = "127.0.0.1"
                                                              , connectPassword  = "bench"
                                                              , connectDatabase = "fa"
                                                              } }
               , appForecastProfilesDir = "../bench-data/Forecasts"
               }

-- | Create a handler but read settings once
-- Clear arguments so that they don't interfere with criterion ones.
-- Moreover we don't want external things to modify the behavior of the benchmarks.
-- All setting modification must be done through `AppSettings -> AppSettings`
benchmarkFoundationWith args f = withArgs args do 
   A.getAppSettings >>= A.makeFoundation . f . overrideDB 
  
h ::  App -> Handler a -> IO a
h foundation =  unsafeHandler foundation


   
