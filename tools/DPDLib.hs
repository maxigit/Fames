{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module DPDLib
    ( dumpFromWebsite
    , CustomerInfo(..)
    ) where

import Prelude
import Test.WebDriver
-- import Test.WebDriver.Commands
import Test.WebDriver.Commands.Wait
-- import qualified Test.WebDriver.Common.Keys as K
import Data.Text
import Debug.Trace
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
data CustomerInfo = CustomerInfo
  { shortName :: !Text
  , postCode :: !Text
  , country :: !Text
  , organisation :: !Text
  , address1 :: !Text
  , address2 :: !Text
  , town :: !Text
  , contact :: !Text
  , county'state :: !Text
  , telephone :: !Text
  , notification_email :: !Text
  , notification_text :: !Text
  } deriving (Show, Read, Eq, Generic)

instance ToNamedRecord CustomerInfo
instance FromNamedRecord CustomerInfo

instance DefaultOrdered CustomerInfo

dumpFromWebsite :: Text -> Text -> IO ()
dumpFromWebsite user password = do
  let config = useBrowser chrome defaultConfig 
  -- runSession config  . finallyClose $ do
  customers <- runSession config  $ do
    logDPD user password
    fetchAddressBook
  let bs = encodeDefaultOrderedByName customers
  BS.writeFile "address-book.csv" bs


    


fetchAddressBook :: WD [CustomerInfo]
fetchAddressBook = do
  openPage "https://www.dpdlocal.co.uk/app/addressbook.html#address-book"
  waitUntil 5 $ findElem (ByXPath  "//*[starts-with(.,'Showing 1')]")
  loop 0 []
  where 
    loop count founds = do    
          customers <- getCustomersOnPage
          let newCount = count + Prelude.length customers
          np <- nextPage newCount
          if np
            then loop newCount (founds ++ customers)
            else return $ founds ++ customers

getCustomersOnPage :: WD [CustomerInfo]
getCustomersOnPage = do
  rows <- waitUntil 4 $ findElems (ByCSS "div#addressbook_search_fulllist table tr.x-grid-row")
  traceShowM ("Rows", rows)
  case rows of
    row:_ -> click row
    _ -> return ()
  mapM getCustomerInfo rows

getCustomerInfo :: Element -> WD CustomerInfo
getCustomerInfo tr = do
  click tr
  -- make sure page is loaded
  shortName <- findElemFrom tr (ByCSS "td:first-child") >>= getText
  form <- waitUntil 5 $ do
    form <- findElem (ByCSS "div#AddressBookForm")
    -- findElemFrom form (ByXPath $ "//*[.='" <> shortName <> "']")
    -- findElemFrom form (ByXPath $ "//*[starts-with(.,\"" <> Data.Text.take 15 shortName <> "\")]")
    -- findElemFrom form (ByXPath $ "//*[@*=\"" <> shortName <> "\"]")

    traceShowM("FORM", form)
    return form
  Just country <- findElemFrom form (ByCSS "[name=addressbook_country]") >>= flip attr "value"
  Just postCode <- findElemFrom form (ByCSS "[name=addressbook_postcode]") >>= flip attr "value"
  Just organisation <- findElemFrom form (ByCSS "[name=addressbook_organisation]") >>= flip attr "value"
  Just address1 <- findElemFrom form (ByCSS "[name=addressbook_address1]") >>= flip attr "value"
  Just address2 <- findElemFrom form (ByCSS "[name=addressbook_address2]") >>= flip attr "value"
  Just town <- findElemFrom form (ByCSS "[name=addressbook_town]") >>= flip attr "value"
  Just county'state <- findElemFrom form (ByCSS "[name=addressbook_county]") >>= flip attr "value"
  Just contact <- findElemFrom form (ByCSS "[name=addressbook_contact]") >>= flip attr "value"
  Just telephone <- findElemFrom form (ByCSS "[name=addressbook_telephone]") >>= flip attr "value"
  Just notification_email <- findElemFrom form (ByCSS "[name=addressbook_notification_email]") >>= flip attr "value"
  Just notification_text <- findElemFrom form (ByCSS "[name=addressbook_notification_text]") >>= flip attr "value"
  let customer = CustomerInfo{..}
  traceShowM("Customer", customer)
  return customer

nextPage :: Int -> WD Bool
nextPage last = do
  es <- findElems (ByXPath "//button[.='Next ->'][not(@disabled)]")
  case es of
    [] -> return False
    e:_ ->  do
      click e
      waitUntil 5 $ do
        -- make sure we wait for the next page
        _ <- findElem (ByXPath $ "//*[starts-with(.,'Showing "  <> pack (show (last +1)) <> " ')]")
        return True

    

logDPD :: Text -> Text -> WD ()
logDPD user password = do
  -- openPage "https://www.dpdlocal.co.uk/umslogon" -- app/addressbook.html#address-book"
  openPage "https://www.dpdlocal.co.uk/app/addressbook.html#address-book"
  waitUntil 5 $ do
    findElem (ById "logon_username") >>= sendKeys user
  -- findElem (ByCSS "input") >>= sendKeys user
  findElem (ById "logon_password") >>= sendKeys password
  findElem (ByClass "submit_button") >>= click
  -------------------------------------
  -- wait we 
  waitUntil 5 $ do
    findElem (ByLinkText "Create Shipment") >>= click
  return ()


  


