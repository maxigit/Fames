module Customers.Settings where

import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions) -- , fieldLabelModifier, sumEncoding, SumEncoding(..))

-- | Constants to fill DPD shipping import files.
data DPDSettings = DPDSettings
  { shipperAddressLine1_property'street :: Text
  , shipperAddressLine2 :: Maybe Text
  , shipperAddressLine3_city :: Text
  , shipperAddressLine4 :: Maybe Text
  , shipperPostcode :: Text
  , shipperCountryCode :: Text
  , shipperContactName :: Text
  , shipperContactTelephone :: Text
  , shipperOrganisation :: Text
  , shipperEORI :: Text
  } deriving (Show, Eq, Ord)


-- * JSON 
$(deriveJSON defaultOptions ''DPDSettings)
