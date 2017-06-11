{-# LANGUAGE OverloadedStrings #-}
{-# Language CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod hiding(throw)
import Control.Exception          (throw)
import Data.Aeson                 (Result (..), fromJSON, withObject, (.!=),
                                   (.:?))
import Data.FileEmbed             (embedFile)
import Data.Yaml                  (decodeEither')
import Database.Persist.MySQL     (MySQLConf (..))
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp   (HostPreference)
import Yesod.Default.Config2      (applyEnvValue, configSettingsYml)
import Yesod.Default.Util         (WidgetFileSettings, widgetFileNoReload,
                                   widgetFileReload)
import qualified Database.MySQL.Base as MySQL
import Yesod.Fay
import  Role
import WH.Barcode
import WH.FA.Types
import qualified Data.Map as Map

data AuthMode = BypassAuth | CheckAuth deriving (Read, Show, Eq)
-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.

-- | Allowed prefixes. Read from configuration file.

data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: MySQLConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    , appRoleFor :: RoleFor 
    -- ^ Roles for each users. Can be overridden for tests.
    -- Must return a Role for everybody, even a "empty" role.
    , appStockLocations :: Map Text [Text]
    -- ^ All FA locations and their shelves
    , appStockLocationsInverse :: Map Text Text
    -- ^ inverse of stockLocations. Gives the FA location from a shelf
    , appFALostLocation :: Text
    -- ^ FA location use to loose object
    , appFADefaultLocation :: Text
    , appFAStockLikeFilter :: Text
    -- ^ SQL LIKE expression to filter what's is considered stock
    , appBarcodeParams :: [BarcodeParams]
    , appFAURL :: String -- ^ URL to connect to FrontAccounting to post transactions
    , appFAUser :: String -- ^ User to connect to FrontAcounting
    , appFAPassword :: String -- ^ User passwrod to connect to FA to post transactions.
    } deriving Show


instance FromJSON AppSettings  where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        fromYamlAppDatabaseConf   <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        barcodeParamsMap          <- o .:? "barcodes" .!= (mempty :: Map Text (Text -> BarcodeParams))
        let appBarcodeParams = [ bp prefix
                               | (prefix, bp)
                               <- Map.toList (barcodeParamsMap)
                               ]

        roleForMap                <- o .:? "roles" .!= (mempty :: Map Text Role)
        let types = roleForMap  :: Map Text Role
            appRoleFor = RoleFor $
                \user -> fromMaybe (RoleGroup [])
                                   (asum [lookup u roleForMap
                                         | u <- (maybeToList user >>= \u -> [u, "<authenticated>"]) ++ ["<anonymous>"]
                                         ]
                                   )
        appStockLocations      <- o .:? "stock-locations" .!= Map.fromList []
        let appStockLocationsInverse = Map.fromList [(shelf, fa)
                                                    | (fa, shelves) <- Map.toList appStockLocations
                                                    , shelf <- shelves
                                                    ]
        appFALostLocation  <- o .:? "fa-lost-location" .!= "LOST"
        appFADefaultLocation  <- o .:? "fa-default-location" .!= "DEF"
        appFAStockLikeFilter  <- o .:? "fa-stock-like-filter" .!= "%"
        appFAURL <- o .:? "fa-url" .!= "http://127.0.0.1"
        appFAUser <- o .:? "fa-user" .!= "admin"
        appFAPassword <- o .:? "fa-password" .!= "password"
        

        -- This code enables MySQL's strict mode, without which MySQL will truncate data.
        -- See https://github.com/yesodweb/persistent/wiki/Database-Configuration#strict-mode for details
        -- If you choose to keep strict mode enabled, it's recommended that you enable it in your my.cnf file so that it's also enabled for your MySQL console sessions.
        -- (If you enable it in your my.cnf file, you can delete this code).
        let appDatabaseConf = fromYamlAppDatabaseConf { myConnInfo = (myConnInfo fromYamlAppDatabaseConf) {
                MySQL.connectOptions =
                  ( MySQL.connectOptions (myConnInfo fromYamlAppDatabaseConf)) ++ [MySQL.InitCommand "SET SESSION sql_mode = 'STRICT_ALL_TABLES';\0"]
              }
            }
            appBypassAuth = CheckAuth

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

fayFile' :: Exp -> FayFile
fayFile' staticR moduleName =
    (if appReloadTemplates compileTimeAppSettings
        then fayFileReload
        else fayFileProd)
     settings
  where
    settings = (yesodFaySettings moduleName)
        { yfsSeparateRuntime = Just ("static", staticR)
        -- , yfsPostProcess = readProcess "java" ["-jar", "closure-compiler.jar"]
        , yfsExternal = Just ("static", staticR)
        , yfsPackages = ["fay-dom", "fay-jquery", "fay-text", "fay"]
        }

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
