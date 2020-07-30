module GL.Check.ItemCostSettings
where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions) --, fieldLabelModifier, sumEncoding, SumEncoding(..))
-- import Data.Aeson.Types
-- * Type
data Account = Account { fromAccount:: Text } deriving (Eq, Show)
-- * Settings
data Settings =  Settings
  { 
  }
  deriving (Show, Read, Eq, Ord)

-- * JSON
$(deriveJSON defaultOptions ''Settings)
