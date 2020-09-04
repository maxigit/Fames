module GL.Check.ItemCostSettings
where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, {- fieldLabelModifier, -} sumEncoding, SumEncoding(..))
import Data.Aeson.Types
import Data.Text(splitOn)
-- * Type
data Account = Account { fromAccount:: Text } deriving (Eq, Show, Read, Ord)

data Behavior
  = Skip -- as didn't exist
  | FAOnly -- like skip but update the FA running balance
  | UsePreviousCost
  | UseMoveCost
  | GenerateError 
  | BehaveIf BehaviorSubject Behavior
  | BehaveIfe BehaviorSubject Behavior Behavior
  deriving (Eq, Show, Read, Ord)

data BehaviorSubject
  = ForTransaction Int Int
  -- | ForMove Int
  | ForGrnWithoutInvoice
  | ForGrnProvision
  | ForSku (Maybe Text)
  | ForNullCost
  deriving (Eq, Show, Read, Ord)

-- * Settings
data Settings =  Settings
  { stockFilter ::  Text  -- to convert to FilterExpression
  , accounts :: Map Account AccountSettings
  , extraAccounts :: Maybe [Account]
  , defaultFixAccount :: Maybe Account
  , defaultDate :: Maybe Day
  , batchSize :: Maybe Int -- maximum number of item to process when posting to FA
  , behaviors:: Maybe BehaviorMap
  }
  deriving (Show, Read, Eq, Ord)

type BehaviorMap = Map BehaviorSubject Behavior

data AccountSettings = AccountSettings
  { items :: Map Text ItemSettings
  , fixAccount :: Maybe Account
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
----------------------------------------------------
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

----------------------------------------------------
instance FromJSONKey BehaviorSubject where
  fromJSONKey = FromJSONKeyTextParser $ parseJSON . String
instance  ToJSONKey BehaviorSubject  where
  toJSONKey = toJSONKeyText behaviorSubjectToText

instance FromJSON BehaviorSubject where
  parseJSON = withText ("BehaviorSubject") p where
    p s | ws@[_,_] <- splitOn "/" s  =
      case map readMay ws of
        [Just type_, Just no] -> return $ ForTransaction type_ no
        _ -> fail $ unpack s <>  " no of the shape Int/Int "
    p "grn-without-invoice" = return ForGrnWithoutInvoice
    p "grn-provision" = return ForGrnProvision
    p "cost=0" = return ForNullCost
    p "no-sku" = return $ ForSku Nothing
    p (stripPrefix "sku=" -> Just sku) = return $ ForSku (Just sku)
    p s  = fail $ unpack s  <> " is not BehaviorSubject"

instance  ToJSON BehaviorSubject where
  toJSON = String . behaviorSubjectToText  where


behaviorSubjectToText :: BehaviorSubject -> Text
behaviorSubjectToText = go where
    go (ForTransaction type_ no) = tshow type_ <> "/" <> tshow no
    go ForGrnWithoutInvoice = "grn-without-invoice"
    go ForGrnProvision = "grn-provision"
    go (ForSku Nothing)  = "no-sku"
    go (ForSku (Just sku))  = "sku=" <> sku
    go ForNullCost = "cost=0"
----------------------------------------------------
$(deriveJSON defaultOptions ''AccountSettings)
$(deriveJSON defaultOptions { sumEncoding = UntaggedValue}  ''Behavior)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField }  ''InitialSettings)
$(deriveJSON defaultOptions ''ItemSettings)
$(deriveJSON defaultOptions ''Settings)
