module GL.Check.ItemCostSettings
where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON)
import Data.Aeson.Types
import Data.Text(splitOn)
import Control.Monad.Fail (MonadFail(..))
-- * Type 
data Account = Account { fromAccount:: Text } deriving (Eq, Show, Read, Ord)

data Behavior
  = Skip -- as didn't exist
  | Close -- don't process any transaction
  | FAOnly -- like skip but update the FA running balance
  | UsePreviousCost
  | UseMoveCost
  | SetMoveCost Double
  | GenerateError 
  | WaitForGrn Int -- invoice only matching the given GRN
  | WaitForStock Double -- wait for given quantity
  | BehaveIf BehaviorSubject Behavior
  | BehaveIfe BehaviorSubject Behavior Behavior
  deriving (Eq, Show, Read, Ord)

data BehaviorSubject
  = ForTransaction Int Int
  -- -| ForMove Int
  | ForGrnWithoutInvoice
  | ForGrnProvision
  | ForSku (Maybe Text)
  | ForNullCost
  | ForAccount Text
  | ForSalesWithoutInvoice
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
                , startDate :: Day --  ^ filter transaction before that date
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
instance FromJSON Behavior where
  parseJSON v = 
    withText ("Behavior") p v
    <|> withArray ("Ife") a v
    <|> withObject ("GRN") grn v
    <|> withObject ("Stock") stock v
    <|> withObject ("Set") set v
    where
    p "Skip" = return Skip
    p "Close" = return Close
    p "FAOnly" = return FAOnly
    p "UsePreviousCost" = return UsePreviousCost
    p "UseMoveCost" = return UseMoveCost
    p "GenerateError" = return GenerateError
    p s = fail $ unpack s <> " is not a Behavior"
    a as = case toList as of
      [condv, thenv] -> do
        cond <- parseJSON condv
        then_ <- parseJSON thenv
        return $ BehaveIf  cond then_
      [condv, thenv, elsev] -> do
        cond <- parseJSON condv
        then_ <- parseJSON thenv
        else_ <- parseJSON elsev
        return $ BehaveIfe  cond then_ else_
      (condv:thenv: elsev@(_cond2:_)) | True -> do -- chain all cond1 then1 cond2 then2 cond3 then3 ...
        cond <- parseJSON condv
        then_ <- parseJSON thenv
        else_ <- a (fromList elsev)
        return $ BehaveIfe  cond then_ else_
      _ -> fail "Not an ife conditions"
    grn o = do
      grnId <- o .: "WaitForGrn"
      return $ WaitForGrn grnId
    stock o = WaitForStock <$>  o .: "WaitForStock"
    set o = do
      cost <- o .: "SetMoveCost"
      return $ SetMoveCost cost

    -- a [cond, t] = BehaveIf cond t

instance ToJSON Behavior where
  toJSON b = case b of
    Skip -> String "Skip"
    Close -> String "Close"
    FAOnly -> String "FAOnly"
    UsePreviousCost -> String "UsePreviousCost"
    UseMoveCost -> String "UseMoveCost"
    SetMoveCost cost -> object [ "SetMoveCost" .= toJSON cost ]
    GenerateError -> String "GenerateError"
    WaitForGrn grn -> object [ "WaitForGrn" .= toJSON grn] 
    WaitForStock qty -> object [ "WaitForStock" .= toJSON qty] 
    BehaveIf cond then_ -> toJSON [toJSON cond, toJSON then_]
    BehaveIfe cond then_ else_ -> toJSON [toJSON cond, toJSON then_, toJSON else_]
----------------------------------------------------
instance FromJSONKey BehaviorSubject where
  fromJSONKey = FromJSONKeyTextParser $ parseJSON . String
instance  ToJSONKey BehaviorSubject  where
  toJSONKey = toJSONKeyText behaviorSubjectToText

instance FromJSON BehaviorSubject where
  parseJSON = withText ("BehaviorSubject") p where
    p (stripPrefix "sku=" -> Just sku) = return $ ForSku (Just sku)
    p s | ws@[_,_] <- splitOn "/" s  =
      case map readMay ws of
        [Just type_, Just no] -> return $ ForTransaction type_ no
        _ -> fail $ unpack s <>  " no of the shape Int/Int "
    p "grn-without-invoice" = return ForGrnWithoutInvoice
    p "grn-provision" = return ForGrnProvision
    p "sales-without-invoice" = return ForSalesWithoutInvoice
    p "cost=0" = return ForNullCost
    p "no-sku" = return $ ForSku Nothing
    p (stripPrefix "account=" -> Just account) = return $ ForAccount account
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
    go (ForAccount account)  = "account=" <> account
    go ForNullCost = "cost=0"
    go ForSalesWithoutInvoice = "sales-without-invoice"
----------------------------------------------------
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField }  ''InitialSettings)
$(deriveJSON defaultOptions ''ItemSettings)
$(deriveJSON defaultOptions ''AccountSettings)
$(deriveJSON defaultOptions ''Settings)
