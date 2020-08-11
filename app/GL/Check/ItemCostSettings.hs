module GL.Check.ItemCostSettings
where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, {- fieldLabelModifier, -} sumEncoding, SumEncoding(..))
import Data.Aeson.Types
-- * Type
data Account = Account { fromAccount:: Text } deriving (Eq, Show, Read, Ord)
-- * Settings
data Settings =  Settings
  {  stockFilter ::  Text  -- to convert to FilterExpression
  ,  accounts :: Map Account AccountSettings
  ,  extraAccounts :: Maybe [Account]
  ,  defaultFixAccount :: Maybe Account
  }
  deriving (Show, Read, Eq, Ord)


data AccountSettings = AccountSettings
  {  items :: Map Text ItemSettings
  ,  fixAccount :: Maybe Account
  }
  deriving (Show, Read, Eq, Ord)



data ItemSettings = ItemSettings
  { initial :: Maybe InitialSettings
  , closingBalance :: Maybe Double
  , closingQoh :: Maybe  Double
  , closingDate :: Maybe  Day
  }
  deriving (Show, Read, Eq, Ord)

data InitialSettings
  = FromAccount Account
  -- ^ if a item has been setup with an previous account
  -- use the item/account summary as initial balance
  | InitialData { initialBalance :: Double
                , initialQoh :: Double
                , startDate :: Day -- ^ filter transaction before that date
                } 
  deriving (Show, Read, Eq, Ord)

  
-- * JSON
instance FromJSON Account where
  parseJSON v = withText ("Account as Text")  (return . Account)  v
                <|> (do
                          i <- parseJSON v  
                          return (Account $ tshow (i :: Int))
                          )

instance ToJSON Account where
   toJSON (Account account) = String account
  
instance ToJSONKey Account where
  toJSONKey = toJSONKeyText fromAccount
instance FromJSONKey Account where
  fromJSONKey = FromJSONKeyText Account
$(deriveJSON defaultOptions ''AccountSettings)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField }  ''InitialSettings)
$(deriveJSON defaultOptions ''ItemSettings)
$(deriveJSON defaultOptions ''Settings)
