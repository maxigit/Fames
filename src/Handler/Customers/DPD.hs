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
  { organisation'name :: Text
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
  , totalWeightKg :: Either Text Double
  , generateCustomData :: YesNo
  , invoiceType :: InvoiceType
  , invoiceReference :: Text
  -- , shipperAddressLine1'property'street :: Text
  -- , shipperAddressLine2'locality :: Maybe Text
  -- , shipperAddressLine3'City :: Text
  -- , shipperAddressLine4'County'State :: Maybe Text
  -- , shipperContactName :: Text
  , countryOfOrigin :: CountryCode -- GB origin of the parcel
  , shipping'freightCost :: Double
  , reasonForExport :: ReasonForExport
  , receiverVAT'PID'EORI :: Text
  } deriving (Show, Read)

data YesNo = Y | N deriving (Show, Read)
data ServiceCode = ParcelTwoDay
                 | ParcelNextDay
                 | ExpressPak1NextDay
                 | ExpressPak5NextDay
                 | InternationalClassic
                 deriving (Show, Read)
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
  toField sc = case sc of
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
    [ "Delivery organisation/name" .= organisation'name
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
    , "Delivery totar weight (kg)" .= either toField toField totalWeightKg
    , "Generate customData" .= generateCustomData
    , "Invoice Type" .= invoiceType
    , "Invoice Reference" .= invoiceReference
    -- , shipper addressLine1'property'street :: Text
    -- , shipper addressLine2'locality :: Maybe Text
    -- , shipper addressLine3'City :: Text
    -- , shipper addressLine4'County'State :: Maybe Text
    -- , shipper contactName :: Text
    , "Country of Origin" .= countryOfOrigin
    , "Shipping/Freight Cost" .= shipping'freightCost
    , "Reason for Export" .= reasonForExport
    , "Receiver's VAT/PID/EORI No." .= receiverVAT'PID'EORI
    ]

instance ToNamedRecord ProductDetail where
  toNamedRecord = namedRecord . productDetailToFields

productDetailToFields ProductDetail{..} =
    [ "Identifier" .= identifier
    , "ProductCode" .= productCode
    , "HarmonisedCode" .= harmonisedCode
    , "UnitWeight" .= either toField toField unitWeight
    , "Parcel" .= either toField toField parcel
    , "Description" .= description
    , "ProductType" .= productType
    , "ItemOrigin" .= itemOrigin
    , "Quantity" .= quantity
    , "UnitValue" .= unitValue
    ]

-- * Stream
makeDPDSource :: (Double -> Either Text Double -> Delivery ) -> [ProductDetail] -> ConduitT () (L.ByteString) Handler ()
makeDPDSource _ [] = error "Invoice without product details"
makeDPDSource delivery' details@(detail:_) = do
  let totalCost = sum $ map (liftA2 (*) unitValue (fromIntegral . quantity)) details
      totalWeight = sum <$> traverse weightE details
      weightE ProductDetail{..} = do
        w <- unitWeight
        Right $ w * fromIntegral quantity
      delivery = delivery' totalCost totalWeight
      devHeader = header $ map fst $ deliveryToFields delivery
      detailHeader = header $ map fst $ productDetailToFields detail
      opt = defaultEncodeOptions
      noHeader = opt {encIncludeHeader = False }
  --------------------------------------------------
  -- Headers first ...
  yield $ encodeByNameWith opt devHeader ([] :: [Delivery])
  yield $ encodeByNameWith opt detailHeader ([] :: [ProductDetail])
  -- Then no header
  yield $ encodeByNameWith noHeader devHeader [delivery]
  yield $ encodeByNameWith noHeader detailHeader details

  


