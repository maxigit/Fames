{-# LANGUAGE PartialTypeSignatures #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

withApp :: (Text -> Bool) -- ^ filter tables to truncate
        -> _ -- ^ Hook before o beforeAll
        -> SpecWith (TestApp App) -> Spec
withApp tablePredicate  hook = hook $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB tablePredicate foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

withAppNoDB = withApp keepAllTables beforeAll
withAppWipe = withApp keepFA before
-- This function will truncate all of the tables matching the predicates in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.

allTables, keepFA, keepAllTables :: Text -> Bool 
allTables _ = True
keepFA = X.isPrefixOf "0_"
keepAllTables _ = False


wipeDB :: (Text -> Bool) -> App -> IO ()
wipeDB toKeep app = runDBWithApp app $ do
    tables <- filter (not . toKeep) <$> getTables
    sqlBackend <- ask
    let queries = map (\t -> "TRUNCATE TABLE " ++ connEscapeName sqlBackend (DBName t)) tables

    -- In MySQL, a table cannot be truncated if another table references it via foreign key.
    -- Since we're wiping both the parent and child tables, though, it's safe
    -- to temporarily disable this check.
    rawExecute "SET foreign_key_checks = 0;" []
    forM_ queries (\q -> rawExecute q [])
    rawExecute "SET foreign_key_checks = 1;" []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SHOW TABLES;" []
    return $ map unSingle tables
