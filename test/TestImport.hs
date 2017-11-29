{-# LANGUAGE PartialTypeSignatures #-}
module TestImport
    ( module TestImport
    , module X
    , AuthMode(..)
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import Yesod.Test            as X
import Yesod.Auth (Route(LoginR))
import Settings(appRoleFor)
import Role(Role(Administrator), RoleFor(..))
import System.IO.Temp (openTempFile)
import Handler.Util               as X

-- Log as administrator, In theory gives access to every page
logAsAdmin = do
     get (AuthR LoginR)
     request $ do
       setMethod "POST"
       setUrl ("/auth/page/fa/login" :: String)
       addToken_ "form#login-form"
       addPostParam "username" "admin"
       addPostParam "password" "password"

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

data AuthMode = BypassAuth | CheckAuth deriving (Eq, Read, Show)
withApp ::  (Text -> KeepOrWipe) -- ^ filter tables to truncate
        -> _ -- ^ Hook before o beforeAll
        -> AuthMode -- ^ Bypass authorization
        -> SpecWith (TestApp App)
        -> Spec
withApp tablePredicate  hook authMode = hook $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation ( if authMode == BypassAuth
                                   then settings {appRoleFor = RoleFor (const Administrator) }
                                   else settings
                                 )
    wipeDB tablePredicate foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

withAppNoDB = withApp keepAllTables beforeAll
withAppWipe = withApp keepFA before
-- This function will truncate all of the tables matching the predicates in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.

data KeepOrWipe = Keep | Wipe deriving (Eq, Show)
keepFA, keepAllTables :: Text -> KeepOrWipe 
keepFA table = if X.isPrefixOf "0_" table
           then Keep
           else Wipe
keepAllTables _ = Keep


wipeDB :: (Text -> KeepOrWipe) -> App -> IO ()
wipeDB toKeep app = runDBWithApp app $ do
    tables <- filter ((== Wipe) . toKeep) <$> getTables
    sqlBackend <- ask
    let queries = map (\t -> "TRUNCATE TABLE " ++ connEscapeName sqlBackend (DBName t)) tables

    -- In MySQL, a table cannot be truncated if another table references it via foreign key.
    -- Since we're wiping both the parent and child tables, though, it's safe
    -- to temporarily disable this check.
    rawExecute "SET foreign_key_checks = 0;" []
    forM_ queries (\q -> rawExecute q [])
    rawExecute "SET foreign_key_checks = 1;" []

-- getTables ::  SqlHandler [Text]
getTables = do
    tables <- rawSql "SHOW TABLES;" []
    return $ map unSingle tables


-- * Utility function
-- | Save a Text in a temporary file so it can be uploaded
-- Returns a path
saveToTempFile :: MonadIO io => Text -> io FilePath
saveToTempFile content = liftIO $ do
  (path, handle) <- openTempFile "/tmp" "fames-test"
  hPut handle (encodeUtf8 content)
  hClose handle
  return path

