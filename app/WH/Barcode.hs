module WH.Barcode where
import ClassyPrelude.Yesod
import Data.Text.Lazy.Builder (fromLazyText)
import qualified Data.Text.Lazy as LT
import qualified Data.Map as Map
import Data.Char (ord, chr)
import Data.List (iterate)
import Data.Aeson                 (Result (..), fromJSON, withObject, (.!=),
                                   (.:?))
-- * Types
-- | Allowed prefixes. Read from configuration file.
data BarcodeParams = BarcodeParams
  { bpPrefix :: Text
  , bpDescription :: Text
  , bpTemplates :: [BarcodeTemplate]
  } deriving (Eq, Read, Show)

data BarcodeTemplate = BarcodeTemplate
  { btPath :: Text
  , btNbPerPage :: Int
  } deriving (Eq, Read, Show)

data OutputMode = Csv | GLabels deriving (Eq, Ord, Read, Show)

-- * FromJSON
instance FromJSON (Text -> BarcodeParams) where
  parseJSON (Object o) =  do
    bpDescription <- o .: "description"
    templates <- o .: "templates" -- .!= []
    let bpTemplates = map (uncurry BarcodeTemplate) (Map.toList templates)
    -- let bpPrefix = "toto"
    return $ \bpPrefix -> BarcodeParams {..}
    
  parseJSON _ = error "Barcode templates should be an object"
  

-- * Functions
-- | Month abbreviation on letter
-- month2 :: Day -> Text
month2 day = let (_, month, _) = toGregorian day
  in fromLazyText $ go month  where
  go 1 = "JA"
  go 2 = "FE"
  go 3 = "MR"
  go 4 = "AP"
  go 5 = "MY"
  go 6 = "JU"
  go 7 = "JL"
  go 8 = "AU"
  go 9 = "SE"
  go 10 = "OC"
  go 11 = "NV"
  go 12 = "DE"

-- | checksum
checksum :: LT.Text -> LT.Text
checksum text = let
 c = sum $ zipWith (*) (map ord (reverse $ toList text)) (iterate (*10) 7)
 in singleton . chr $ c `mod` 26 + ord 'A'
