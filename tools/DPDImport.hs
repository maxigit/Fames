
import Import
import DPDLib
import qualified Data.Csv  as Csv
import qualified Data.ByteString.Lazy as BL
import Application (db)
import Handler.Customers.ShippingDetails
import Data.ISO3166_CountryCodes


main :: IO ()
main = do
  csv <- BL.readFile "address-book.csv"
  case Csv.decodeByName csv of
    Left e -> error $ show e
    Right (_, infos) -> db $ do
      deleteWhere ([ShippingDetailsSource ==. "DPDImport"] :: [Filter ShippingDetails])
      mapM_ importInfo infos


importInfo :: CustomerInfo -> SqlHandler ()
importInfo customer = do
  let details = mkShippingDetails customer
  saveShippingDetails details


mkShippingDetails :: CustomerInfo -> ShippingDetails
mkShippingDetails CustomerInfo{..} = details {shippingDetailsKey = unDetailsKey $ computeKey details } where
  details = ShippingDetails{..}
  shippingDetailsCourrier = "DPD"
  shippingDetailsSource = "DPDImport"
  shippingDetailsShortName =  shortName
  shippingDetailsPostCode = postCode
  shippingDetailsCountry = readCountry country
  shippingDetailsOrganisation =organisation
  shippingDetailsAddress1 = address1
  shippingDetailsAddress2 =  address2
  shippingDetailsTown = town
  shippingDetailsCounty = county'state
  shippingDetailsContact = contact
  shippingDetailsTelephone = textToMaybe telephone
  shippingDetailsNotificationEmail = textToMaybe notification_email
  shippingDetailsNotificationText =  textToMaybe notification_text
  shippingDetailsTaxId = Nothing
  shippingDetailsLastUsed = Nothing
  shippingDetailsKey = "" --  unDetailsKey $ computeKey details


readCountry :: Text -> Maybe CountryCode
readCountry name0 =
    readMay name <|> lookup name (map (fanl $ toLower . readableCountryName ) [minBound..maxBound])
                 <|> lookup name (map (fanl $ toLower . countryNameFromCode ) [minBound..maxBound])
                 where name = toLower $ unpack name0

  
