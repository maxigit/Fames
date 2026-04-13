{-# LANGUAGE PartialTypeSignatures #-}
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
                                                              } }}

-- | Create a handler but read settings once
-- Clear arguments so that they don't interfere with criterion ones.
-- Moreover we don't want external things to modify the behavior of the benchmarks.
-- All setting modification must be done through `AppSettings -> AppSettings`
benchHandlerWithH :: [String] -> (AppSettings -> AppSettings)  -> IO (Handler a -> IO a)
benchHandlerWithH args f = do
   withArgs args do
      handler <- A.makeHandlerWith (f . overrideDB)
      return $ \h -> handler (clearAppCache >> h)
  


   
