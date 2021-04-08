
import Import
import DPDLib
import qualified Data.Csv  as Csv
import qualified Data.ByteString.Lazy as BL
import Application (db)
import Handler.Customers.ShippingDetails


main :: IO ()
main = do
  csv <- BL.readFile "address-book.csv"
  case Csv.decodeByName csv of
    Left e -> error $ show e
    Right (_, infos) -> db $ do
      deleteWhere ([] :: [Filter ShippingDetails])
      mapM_ importInfo infos


importInfo :: CustomerInfo -> SqlHandler ()
importInfo customer = do
  let details = mkShippingDetails customer
  saveShippingDetails details
  saveShippingDetails $ clearContact details


mkShippingDetails :: CustomerInfo -> ShippingDetails
mkShippingDetails CustomerInfo{..} = details {shippingDetailsKey = unDetailsKey $ computeKey details } where
  details = ShippingDetails{..}
  shippingDetailsCourrier = "DPD"
  shippingDetailsShortName =  shortName
  shippingDetailsPostCode = postCode
  shippingDetailsCountry = Just country
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
  shippingDetailsKey = "" --  unDetailsKey $ computeKey details

  
