module GL.Check.ItemCostSettings
where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions) --, fieldLabelModifier, sumEncoding, SumEncoding(..))
-- import Data.Aeson.Types
-- * Type
data Account = Account { fromAccount:: Text } deriving (Eq, Show, Read, Ord)
-- * Settings
data Settings =  Settings
  {  stockFilter ::  Text  -- to convert to FilterExpression
  -- ,  accounts :: Map Account AccountSettings
  }
  deriving (Show, Read, Eq, Ord)


data AccountSettings = AccountSettings
-- * JSON
$(deriveJSON defaultOptions ''Settings)
