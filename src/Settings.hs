{-# LANGUAGE OverloadedStrings #-}
{-# Language CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception          (throw)
import Data.Aeson                 (Result (..), fromJSON, withObject, (.!=), Object,
                                   (.:?))
import Data.Aeson.Types (camelTo2, Parser)
import Data.Aeson.KeyMap (toMapText)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson as JSON
import Data.Aeson.TH(deriveToJSON, deriveJSON, defaultOptions, Options(..), SumEncoding(..))
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
import  CategoryRule
import WH.Barcode
import GL.Payroll.Settings
import GL.TaxReport.Settings
import qualified GL.Check.ItemCostSettings as ItemCost
import GL.Receipt
import qualified Data.Map as Map
import Customers.Settings
import Fay.Config(Config(..))
import ModelField(Var)


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
    , appRunMigration            :: Bool

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
    , appCategoryRules :: [Map Text (ItemCategoryRule)]  
    , appSkipStyleCategory:: Bool -- ^ if true don't load style and colour category but compute them
    , appCustomerCategoryRules :: [Map Text (CustomerCategoryRule)]  
    , appOrderCategoryRules :: [Map Text (OrderCategoryRule)]  
    , appDeliveryCategoryRules :: [Map Text (DeliveryCategoryRule)]
    , appBarcodeParams :: [BarcodeParams]
    , appFAURL :: String -- ^ URL to connect to FrontAccounting to post transactions
    , appFAUser :: String -- ^ User to connect to FrontAcounting
    , appFAPassword :: String -- ^ User passwrod to connect to FA to post transactions.
    , appFAExternalURL :: String -- ^ User passwrod to connect to FA to post transactions.
    , appVariations :: Map Var Text -- ^ Variation description. Used to adjust item description in index.
    , appVariationGroups :: Map Text [Var] -- ^ group of variations. Can intersect
    , appPayroll :: PayrollSettings
    , appBankStatements :: Map Text (Map Text  BankStatementSettings) -- ^ How to parse and displays bank statements
    , appForecastProfilesDir :: FilePath -- ^ path to directory containing subdirectories
    -- each subdirectory containing one a "collection_profiles.csv" file
    -- and one or more "*sku_speed.csv" Files
    , appForecastCollectionCategory :: Text -- ^ The category used in collection profile file
    , appForecastDefaultProfile :: FilePath -- ^ current profile for forecast report
    , appForecastPreviousProfile :: FilePath -- ^ previous year profile
    , appForecastPreviousProfileStart :: Maybe Day -- ^ previous year profile start date
    , appPlannerDir :: FilePath -- ^ path to directory containing subdirectories with planner files.
    -- each files will be concatenated in alphabetical order.
    , appReceiptTemplates :: Map Text ReceiptTemplate
    , appReportDeduceTax :: Bool -- ^ weither to deduce tax from tax included transaction
    , appTaxReportSettings :: Map Text TaxReportSettings
    , appPackingListStickerTemplate :: Text -- ^ GLabels file to print stickers from packing list
    , appFavicon:: Text
    , appStocktakeHistoryCategory :: Text
    , appCheckItemCostSetting :: Maybe ItemCost.Settings
    , appDPDSettings :: Maybe DPDSettings
    , appOpenStreetMapKey :: Maybe Text
    , appGoogleApiKey :: Maybe Text
    } deriving Show

data BankAutoReconciliateRule = BankAutoTransfer !Int
  | BankAutoSupplier !Int
  | BankAutoCustomer !Int
  | BankAutoSkip -- used to force rule ignore a specific regex 
  -- usually caugh otherwise by a too lax auto-generated rule
  deriving (Show, Eq, Ord)
data BankStatementMode = BankUseStatement
  { bsPath :: FilePath -- Where to find the statement files
  , bsStatementGlob :: Text
  , bsDailyGlob :: Text -- Glob pattern to filter daily statement
  }
                       | BankNoStatement 
                       deriving (Show, Eq, Ord)
data BankStatementSettings = BankStatementSettings
  { bsStartDate :: Maybe Day -- point in time to start from. Should correspond to 0-discrepency
  -- between FA and the given statemet
  , bsMode :: BankStatementMode
  , bsBankAccount :: Int -- id of the bank account in FA
  , bsPosition:: Maybe Int -- order of display
  , bsLightBlacklist :: [Text] -- what to hide from light mode
  , bsInitialBalance :: Maybe Double
  , bsDiscardRegex :: Maybe Text
  , bsCollapsed :: Maybe Bool -- initial collapsed or no
  , bsRecSummaryPageSize :: Maybe Int -- initial datatable page size for non-rec section
  , bsSummaryPageSize :: Maybe Int -- initital datatable page size for statment section
  , bsSummaryDateCalculator :: Maybe DateCalculator -- When to start the statement section
  , bsSummaryLimit :: Maybe Int -- Number max to load
  , bsRules:: [Map Text BankAutoReconciliateRule]  -- rules to generate FA transaction statement entry
  } deriving (Show, Eq, Ord)

-- TODO clean
instance ToJSON MySQLConf  where
  toJSON = const $  JSON.String "MySQL Conf..."
instance ToJSON HostPreference  where
  toJSON = const $ JSON.String "Host Preference..."
instance ToJSON RoleFor  where
  toJSON = const $ JSON.String "RoleFor..."
  

$(deriveToJSON defaultOptions ''BarcodeTemplate)
$(deriveToJSON defaultOptions ''BarcodeParams)
$(deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '-' . drop 2
                            , sumEncoding= ObjectWithSingleField
                            , constructorTagModifier = camelTo2 '-' . drop (length ("BankAuto" :: Text))} ''BankAutoReconciliateRule)
$(deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '-' . drop 2
                            , sumEncoding= ObjectWithSingleField
                            , constructorTagModifier = camelTo2 '-' . drop (length ("Bank" :: Text))} ''BankStatementMode)
$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '-' . drop 2} ''BankStatementSettings)
$(deriveToJSON defaultOptions ''AppSettings)
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

        appRunMigration           <- o .:? "run-migration"   .!= defaultDev
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
        let -- types = roleForMap  :: Map Text Role
            appRoleFor = RoleFor $
                \user -> fromMaybe (RoleGroup [])
                                   (asum [lookup u roleForMap
                                         | u <- (maybeToList user >>= \u -> [u, "<authenticated>"]) ++ ["<anonymous>"]
                                         ]
                                   )
        stockLocations      <- o .:? "stock-locations" .!= Map.fromList []
        let appStockLocations = fmap (concatMap expandLocation) (stockLocations :: Map Text [Text])
        let appStockLocationsInverse = Map.fromList [(shelf, fa)
                                                    | (fa, shelves) <- Map.toList appStockLocations
                                                    , shelf <- shelves
                                                    ]
        appFALostLocation  <- o .:? "fa-lost-location" .!= "LOST"
        appFADefaultLocation  <- o .:? "fa-default-location" .!= "DEF"
        appFAStockLikeFilter  <- o .:? "fa-stock-like-filter" .!= "%"
        appCategoryRules <- concatFromPrefix "category-rules" o .!= []
        appSkipStyleCategory <- o .:? "skip-style-category" .!= defaultDev
        appCustomerCategoryRules <- concatFromPrefix "customer-category-rules" o .!= []
        appOrderCategoryRules <- concatFromPrefix "order-category-rules" o .!= []
        appDeliveryCategoryRules <- concatFromPrefix "delivery-category-rules" o  .!= []
        appFAExternalURL <- o .:? "fa-x-url" .!= "http://127.0.0.1" -- for outsideworld 
        appFAURL <- o .:? "fa-url" .!= "http://127.0.0.1" -- from inside the Fames container
        appFAUser <- o .:? "fa-user" .!= "admin"
        appFAPassword <- o .:? "fa-password" .!= "password"  

        appVariations <- o .:? "variations" .!= Map.fromList []
        appVariationGroups <- o .:? "variationGroups" .!= Map.fromList []
        appPayroll <- o .:? "payroll" .!= PayrollSettings (Map.fromList [])
                                                          (fromGregorian 2017 03 31)
                                                          (fromGregorian 2017 03 26)
                                                          0 "" "" 0
                                                          (Map.fromList [])
                                                          (mempty)
        -- statemens settings should be a list, each element corresponding to a page
        -- if a map is found, we consider there is only one page
        appBankStatements <-  (o .: "bank-statements" <&> Map.singleton "Main") <|>  (o .:? "bank-statements" .!= mempty)
        appForecastProfilesDir <- o .:? "forecast-profiles-dir" .!= "Forecast"
        appForecastCollectionCategory <- o .:? "forecast-collection-category" .!= "forecast-profile"
        appForecastDefaultProfile <- o .:? "forecast-default-profile" .!= "Repeat"
        appForecastPreviousProfile <- o .:? "forecast-previous-profile" .!= appForecastDefaultProfile
        appForecastPreviousProfileStart <- o .:? "forecast-previous-profile-start"
        appPlannerDir <- o .:? "planner-dir" .!= "Planner"
        appReceiptTemplates <- o .:? "receipt-templates" .!= Map.fromList []
        appReportDeduceTax <- o .:? "report-deduce-tax" .!= False
        appTaxReportSettings <- o .:? "tax-reports" .!= Map.fromList []
        -- let appTaxReportSettings  = either (error . unpack) id $ traverse alterTaxReportSettings appTaxReportSettings'
        
          
        appPackingListStickerTemplate <- o .:? "packing-list-sticker-template" .!= "/config/delivery-stickers.glabels"
        appFavicon <- o .:? "favicon" .!= (if defaultDev then "Orange" else "" )
        appStocktakeHistoryCategory  <- o .:? "stocktake-history-category" .!= "style"
        appCheckItemCostSetting <- o .:? "check-item-cost" .!= Nothing
        appDPDSettings <- o .:? "shipping-dpd"
        appOpenStreetMapKey <- o .:? "open-street-map" .!= Nothing 
        appGoogleApiKey <- o .:? "google-api-key" .!= Nothing 

        -- This code enables MySQL's strict mode, without which MySQL will truncate data.
        -- See https://github.com/yesodweb/persistent/wiki/Database-Configuration#strict-mode for details
        -- If you choose to keep strict mode enabled, it's recommended that you enable it in your my.cnf file so that it's also enabled for your MySQL console sessions.
        -- (If you enable it in your my.cnf file, you can delete this code).
        let appDatabaseConf = fromYamlAppDatabaseConf { myConnInfo = (myConnInfo fromYamlAppDatabaseConf) {
                MySQL.connectOptions =
                  ( MySQL.connectOptions (myConnInfo fromYamlAppDatabaseConf)) ++ [MySQL.InitCommand "SET SESSION sql_mode = 'STRICT_ALL_TABLES';\0"]
              }
            }

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
        else fayFileProdWithConfig modifier )
     settings
  where
    modifier config = config {configTypecheck = False }
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

-- | Extract all the keys starting with the given prefix
-- and concat them in the order. Usefull to split
-- a entry in different files (like category-rules).
-- This is only needed for list as hashmap are merged
-- automatically.
concatFromPrefix :: FromJSON a => Text -> Object -> Parser (Maybe [a])
concatFromPrefix prefix o = do
  let keys_ = filter (prefix `isPrefixOf`)  $ keys $ toMapText o  :: [Text]
  objects <- mapM (o .: ) (sort $ map fromText keys_)
  return $ concat objects
