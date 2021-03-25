{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Generate CSV to be imported
-- using MyDPD local (shipping)
module Handler.Customers.DPD
( Delivery(..)
, ProductDetail(..)
, InvoiceType(..)
, ReasonForExport(..)
, YesNo(..)
, ServiceCode(..)
, PRD(..)
, makeDPDSource
) where
import Import    hiding((.=))
import Data.Csv
import qualified Data.ByteString.Lazy as L
import Data.ISO3166_CountryCodes

-- | Shipping Import Mandatory Fields
-- all optional field have are Maybe
data Delivery = Delivery
  { reference :: Text
  , organisation'name :: Text
  , addressLine1'property'street :: Text
  , addressLine2'locality :: Maybe Text
  , addressLine3'City :: Text
  , addressLine4'County'State :: Maybe Text
  , postCode_7 :: Text
  , countryCode_2 :: CountryCode
  , additional_information :: Text
  , contactName :: Text
  , contactTelephoneNumber :: Text
  , customValue :: Double -- | excluding shipping cost
  , description :: Text
  , noOfPackages :: Int
  , notificationEmail :: Text
  , notificationSMSNumber :: Text
  , serviceCode :: ServiceCode
  , totalWeightKg :: Double
  , generateCustomData :: YesNo
  , invoiceType :: InvoiceType
  , invoiceReference :: Text
  , shipperAddressLine1_property'street :: Text
  , shipperAddressLine2 :: Maybe Text
  , shipperAddressLine3_city :: Text
  , shipperAddressLine4 :: Maybe Text
  , shipperPostcode :: Text
  , shipperCountryCode :: Text
  , shipperContactName :: Text
  , shipperContactTelephone :: Text
  , shipperOrganisation :: Text
  , shipperEORI :: Text
  , countryOfOrigin :: CountryCode -- GB origin of the parcel
  , shipping'freightCost :: Double
  , reasonForExport :: ReasonForExport
  , receiverVAT'PID'EORI :: Text
  } deriving (Show, Read)

data YesNo = Y | N deriving (Show, Read, Eq)
data ServiceCode = ParcelTwoDay
                 | ParcelNextDay
                 | ExpressPak1NextDay
                 | ExpressPak5NextDay
                 | InternationalClassic
                 deriving (Show, Read, Eq, Ord, Bounded, Enum)
data ReasonForExport = Sale
  deriving (Show, Read)

data InvoiceType = Commercial -- vs Proforma
  deriving (Show, Read)


data ProductDetail = ProductDetail
  { identifier :: PRD -- fixed value
  , productCode :: Text
  , harmonisedCode :: Text
  , unitWeight :: Either Text Double
  , parcel :: Either Text Int -- in which box
  , description :: Text
  , productType :: Text
  , itemOrigin :: Text
  , quantity :: Int
  , unitValue :: Double
  } deriving (Show, Read)

data PRD = PRD deriving (Show, Read)

-- * Instances
-- ** Fields
instance ToField CountryCode where
  toField = toField . show
instance ToField ServiceCode where
  toField sc = "2^" ++ case sc of
    ParcelTwoDay -> "11"
    ParcelNextDay -> "12"
    ExpressPak1NextDay -> "68"
    ExpressPak5NextDay ->  "32"
    InternationalClassic -> "19"

instance ToField YesNo where
  toField Y = "Y"
  toField N = "N"

instance ToField InvoiceType where
  toField Commercial = "2"

instance ToField PRD where
  toField PRD = "PRD"

instance ToField ReasonForExport where
  toField Sale = "01"
    
-- ** NamedRecord
instance ToNamedRecord Delivery where
  toNamedRecord = namedRecord . deliveryToFields
deliveryToFields Delivery{..} =
    [ "Delivery customer ref. 1" .= reference
    , "Delivery organisation/name" .= organisation'name
    , "Delivery address line1 (property/street)" .= addressLine1'property'street
    , "Delivery address line2 (locality)" .= addressLine2'locality
    , "Delivery address line3 (City)" .= addressLine3'City
    , "Delivery address line4 (County/State)" .= addressLine4'County'State
    , "Delivery post code" .= postCode_7
    , "Delivery country code" .= countryCode_2
    , "Delivery additional information" .= additional_information
    , "Delivery contact name" .= contactName
    , "Delivery contact telephone number" .= contactTelephoneNumber
    , "Delivery custom value" .= customValue
    , "Delivery description" .= description
    , "Delivery no of packages" .= noOfPackages
    , "Delivery notification email" .= notificationEmail
    , "Delivery notification SMS Number" .= notificationSMSNumber
    , "Delivery service code" .= serviceCode
    , "Delivery total weight (kg)" .= totalWeightKg
    , "Generate customData" .= generateCustomData
    , "Invoice Type" .= invoiceType
    , "Invoice Reference" .= invoiceReference
    , "Country of Origin" .= countryOfOrigin
    , "Shipping/Freight Cost" .= shipping'freightCost
    , "Reason for Export" .= reasonForExport
    , "Receiver's VAT/PID/EORI No." .= receiverVAT'PID'EORI
    , "shipperAddressLine1_property'street" .= shipperAddressLine1_property'street
    , "shipperAddressLine2" .= shipperAddressLine2
    , "shipperAddressLine3_city" .= shipperAddressLine3_city
    , "shipperAddressLine4" .= shipperAddressLine4
    , "shipperPostcode" .= shipperPostcode
    , "shipperCountryCode" .= shipperCountryCode
    , "shipperContactName" .= shipperContactName
    , "shipperContactTelephone" .= shipperContactTelephone
    , "shipperOrganisation" .= shipperOrganisation
    , "shipperEORI" .= shipperEORI
    ]

instance ToNamedRecord ProductDetail where
  toNamedRecord = namedRecord . productDetailToFields

productDetailToFields ProductDetail{..} =
    [ "PRD" .= identifier
    , "Parcel" .= either toField toField parcel
    , "ProductCode" .= productCode
    , "ProductType" .= productType
    , "Description" .= description
    , "ItemOrigin" .= itemOrigin
    , "HarmonisedCode" .= harmonisedCode
    , "UnitWeight" .= either toField (toField . max 0.01) unitWeight
    , "Quantity" .= quantity
    , "UnitValue" .= unitValue
    ]

-- * Stream
makeDPDSource :: (Double -> Delivery ) -> [ProductDetail] -> ConduitT () (L.ByteString) Handler ()
makeDPDSource _ [] = error "Invoice without product details"
makeDPDSource delivery' details@(detail:_) = do
  let totalCost = sum $ map (liftA2 (*) unitValue (fromIntegral . quantity)) details
      delivery = delivery' totalCost
      devHeader = header $ map fst $ deliveryToFields delivery
      detailHeader = header $ map fst $ productDetailToFields detail
      opt = defaultEncodeOptions
      optPipe = defaultEncodeOptions { encDelimiter = 124 }
      noHeader o = o {encIncludeHeader = False }
  --------------------------------------------------
  -- Headers first ...
  yield $ encodeByNameWith optPipe devHeader ([] :: [Delivery])
  yield $ encodeByNameWith opt detailHeader ([] :: [ProductDetail])
  -- Then no header
  yield $ encodeByNameWith (noHeader optPipe) devHeader [delivery]
  if generateCustomData delivery == Y
     then yield $ encodeByNameWith (noHeader opt) detailHeader details
     else return ()


