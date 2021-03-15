module Handler.Customers.Invoices
( getCustInvoicesR
, getCustInvoiceCCodesR
, postCustInvoiceDPDR
) where

import Import
import qualified FA as FA
import Yesod.Form.Bootstrap3
import Handler.Items.Category.Cache
import Data.List(nub)
import Data.Maybe(fromJust)

--  _____                      
-- |_   _|   _ _ __   ___  ___ 
--   | || | | | '_ \ / _ \/ __|
--   | || |_| | |_) |  __/\__ \
--   |_| \__, | .__/ \___||___/
--       |___/|_|              
--
--

data InvoiceInfo = InvoiceInfo
  { iiKey :: Int64
  , iiInvoice :: FA.DebtorTran
  , iiComments :: [ Text ]
  , iiDelivery'orders :: [(FA.DebtorTran, FA.SalesOrder)]
  , iiDetails :: [FA.DebtorTransDetail]
  }


  

--                     __                  _   _                 
--                    / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
--                   | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
--                   |  _| |_| | | | | (__| |_| | (_) | | | \__ \
--                   |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
                                                              
getInvoiceDatesForm startm endm = renderBootstrap3 BootstrapInlineForm form where
  form = (,) <$> areq dayField "Start" startm
             <*> areq dayField "End" endm
-- | Displays the list of last customers invoice
getCustInvoicesR :: Handler Html
getCustInvoicesR = do
  today <- todayH
  ((dates, form),  encType) <- runFormGet $ getInvoiceDatesForm (Just today) (Just today)
  let (start,end) = case dates of
                        FormSuccess ds -> ds
                        _ -> (today, today)

  invoices <- runDB $ selectList [ FA.DebtorTranType ==. fromEnum ST_SALESINVOICE
                         , FA.DebtorTranTranDate >=. start
                         , FA.DebtorTranTranDate <=. end
                         ]
                         [
                         ]
  table <- renderInvoices invoices

  defaultLayout $ do
    [whamlet|
      <div.well>
        <form.form.form-inline method=GET action="@{CustomersR CustInvoicesR}" enctype="#{encType}">
          ^{form}
          <button.btn.btn-default type=submit>Submit
    |]
    primaryPanel "Last Invoices" table


renderInvoices invoices = do
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  nameMap <- entityNameMH True 
  let name FA.DebtorTran{..} = fromMaybe (tshowM debtorTranDebtorNo) $
          nameMap ST_SALESINVOICE $ fmap fromIntegral debtorTranDebtorNo
      -- sort by date desc then by customer name
      sorted = sortOn (first $ Down . FA.debtorTranTranDate)
                      [ (inv, name inv) | (Entity _ inv) <- invoices ]
  return [whamlet|
   <table *{datatable}>
    <thead>
      <th> Trans No
      <th> Customer
      <th> Date
      <th> Amount
      <th>
    <tbody>
      $forall (inv, cust) <- sorted
         <tr>
          <td> #{transNoWithLink (urlForFA faUrl) "" ST_SALESINVOICE $ FA.debtorTranTransNo inv}
          <td> #{cust}
          <td> #{tshow $ FA.debtorTranTranDate inv}
          <td> #{formatDouble $ total inv}
          <td> <a href="@{CustomersR $ CustInvoiceCCodesR $ fromIntegral $ FA.debtorTranTransNo inv}">
                details
  |]
  where total invoice = sum $ map ($ invoice) [FA.debtorTranOvAmount, FA.debtorTranOvGst, FA.debtorTranOvFreight, FA.debtorTranOvFreightTax]
  

getCustInvoiceCCodesR :: Int64 -> Handler Html
getCustInvoiceCCodesR key = do
  info@(InvoiceInfo{..}) <- runDB $ loadInvoiceInfo key
  categoryFinder <- categoryFinderCached 
  sForm <- dpdExportFrom info
  let categoryFor cat detail = categoryFinder cat $ FA.StockMasterKey 
                                                  $ FA.debtorTransDetailStockId detail
  let ccode = fromMaybe "<Commodity Code>" . categoryFor "commodity_code"
  let weight = fromMaybe "<Weight>" . categoryFor "weight"
  let duty = fromMaybe "<Duty>" . categoryFor "duty"
  let table = [whamlet|
      <table *{datatable}>
        <thead>
          <th> SKU
          <th> Quantity
          <th> Unit Price
          <th> Unit Price With PPD
          <th> Commodity Code
          <th> Weight
          <th> Duty
        <tbody>
          $forall detail <- iiDetails
            <tr>
              <td> #{FA.debtorTransDetailStockId detail}
              <td> #{tshow $ FA.debtorTransDetailQuantity detail}
              <td> #{formatDouble $ FA.debtorTransDetailUnitPrice detail}
              <td> #{formatDouble $ FA.debtorTransDetailUnitPrice detail - FA.debtorTransDetailPpd detail}
              <td> #{ccode detail}
              <td> #{weight detail}
              <td> #{duty detail}
    |]

  defaultLayout $ do
    primaryPanel ("Invoice #" <> tshow key) table
    primaryPanel ("DPD Shipping") sForm

-- | Upload a cs
postCustInvoiceDPDR :: Int64 -> Handler TypedContent
postCustInvoiceDPDR key =  do
  let csv =  ["Hello"
             ,"De lu"
             ] :: [Text]
      source = yieldMany csv
      filename = show key <> ".csv"
  setAttachment $ fromString filename
  respondSource ("text/csv") (source .| mapC toFlushBuilder)
  
  
data ShippingForm = ShippingForm
  { shCustomerName :: Text
  , shCountry :: Text
  , shPostalCode :: Text
  , shAddress1 :: Text
  , shAddress2 :: Text
  , shCity :: Text
  , shCountyState :: Text
  , shContact :: Text
  , shTelephone :: Text
  , shNotificationEmail :: Text
  , shNotificationText :: Text
  , shNoOfPackages :: Int
  , shWeight :: Double
  -- , shCustomValue ::  Double
  -- , shService :: Text
  } deriving Show

  
-- | prefill a shipping form, allowing the end user to check
-- and modify it if necessary.
-- Also returns an extra widget with extra information like
-- full address, order comments etc ...
fillShippingForm :: InvoiceInfo -> Maybe FA.CrmPerson  -> Handler ShippingForm
fillShippingForm info personm = runDB $ do
  -- Get address and Co
  customer <- getJust (FA.DebtorsMasterKey $ fromMaybe 0 $ FA.debtorTranDebtorNo $ iiInvoice info)
  let order = case reverse (iiDelivery'orders info) of
                 (_,o):_ -> o
                 [] -> error "Unexpected happend. Invoice should have an order"


  let shCustomerName =  FA.debtorsMasterName customer
      shCountry = ""
      (shAddress1, shAddress2, shCity, shPostalCode, shCountyState) =
        case lines (FA.salesOrderDeliveryAddress order) of
          [add1,city,zip] -> (add1, "", city, zip, "")
          [add1, add2, city, zip, county] -> (add1, add2, city, zip, county)
          [add1, add2, city, zip] -> (add1, add2, city, zip, "")
          _ ->                       (""  , ""  , ""  , "" , "")
      shContact = intercalate " " $ catMaybes [ FA.crmPersonName <$> personm
                                              , personm >>= FA.crmPersonName2
                                              ]
      shTelephone = fromMaybe "<Phone>" (personm >>= FA.crmPersonPhone)
      shNotificationEmail = fromMaybe "<Email>" (personm >>= FA.crmPersonEmail)
      shNotificationText = fromMaybe "<Text>" ((personm >>= FA.crmPersonPhone2) <|> (personm >>= FA.crmPersonPhone))
      shNoOfPackages = 1
      shWeight = 9
      -- shCustomValue ::  Double
      -- shService = ""
  return ShippingForm{..}
 
shippingForm ShippingForm{..} = renderBootstrap3 BootstrapBasicForm form where
  form = ShippingForm <$> areq textField "Customer Name" (Just shCustomerName)
                      <*> areq textField "Country" (Just shCountry)
                      <*> areq textField "Postal/Zip Code" (Just shPostalCode)
                      <*> areq textField "Address 1" (Just shAddress1)
                      <*> areq textField "Address 2" (Just shAddress2)
                      <*> areq textField "City" (Just shCity)
                      <*> areq textField "County/State" (Just shCountyState)
                      <*> areq textField "Contact" (Just shContact)
                      <*> areq textField "Telephone" (Just shTelephone)
                      <*> areq textField "Notification Email" (Just shNotificationEmail)
                      <*> areq textField "Notification Text" (Just shNotificationText)
                      <*> areq intField "No of Packages" (Just shNoOfPackages)
                      <*> areq doubleField "Weight" (Just shWeight)
    
  
-- | Create form allowing to check and prefill information 
-- to be generated for DPD.
-- The order is one of the orders if many possibly the first or the last.
-- We use it for the address
-- dpdExportFrom :: FA.DebtorTrans -> FA.SalesOrder -> Form
dpdExportFrom info = do
  let FA.DebtorTran{..} = iiInvoice info
  contactm <- runDB $ loadContactFor (fromJust debtorTranDebtorNo) $ Just debtorTranBranchCode
  shipping <- fillShippingForm info contactm
  (form, encType) <- generateFormPost $ shippingForm shipping
  return [whamlet|
    ^{invoiceSummary info}
    ^{contactSummary contactm}
    <form.form method=POST action="@{CustomersR $ CustInvoiceDPDR $ iiKey info}" enctype="#{encType}">
      ^{form}
      <button.btn.btn-warning> Download
  |]
   
-- | Displays all information
-- related to the invoice including
-- order comments, delivery address(es)
invoiceSummary info = do
  mapM_ deliverySummary (iiDelivery'orders info)
  [whamlet|
    <div.well>
      $forall comment <- iiComments info
        <p> #{comment}
  |]

deliverySummary (FA.DebtorTran{..}, FA.SalesOrder{..}) = let
   name = "Order #" <> tshow debtorTranOrder <> " | " <> salesOrderReference
   in infoPanel name [whamlet|
    <table.table>
      <tr>
        <td> Order Date
        <td> #{tshow salesOrderOrdDate}
      <tr>
        <td> Delivery Address
        <td>
          $forall l <- lines salesOrderDeliveryAddress
            <p> #{l}
      $maybe comments <- salesOrderComments
        <tr>
          <td> Comments
          <td>
            $forall l <- lines comments
              <p> #{ l }
   |]
  
contactSummary Nothing = [whamlet||]
contactSummary (Just FA.CrmPerson{..}) =
  [whamlet|
    <div.well>
      <table.table>
        $forall (col, valuem) <- col'vals
          $maybe value <- valuem
            <tr>
              <td> #{col}
              <td> #{value}
  |]
  where col'vals =
         [ ("Reference" :: String,  Just crmPersonRef)
         , ("Name",  Just crmPersonName)
         , ("Name2",  crmPersonName2)
         , ("Email",  crmPersonEmail)
         , ("Phone",  crmPersonPhone)
         , ("Phone2",  crmPersonPhone2)
         -- , ("Notes",  crmPersonNotes)
         ]
--                                      _   _ _     
--                                _   _| |_(_) |___ 
--                               | | | | __| | / __|
--                               | |_| | |_| | \__ \
--                                \__,_|\__|_|_|___/
                                                 
  
loadInvoiceInfo key = do
  [(Entity _ invoice)] <- selectList [ FA.DebtorTranTransNo ==. fromIntegral key
                                     , FA.DebtorTranType ==. fromEnum ST_SALESINVOICE
                                     ]
                                     []
  commentEs <- selectList [ FA.CommentId_ ==. fromIntegral key
                          , FA.CommentType ==. fromEnum ST_SALESINVOICE
                          ]
                          []
  let comments = concatMap (maybe [] lines . FA.commentMemo . entityVal) commentEs
  details <- selectList [ FA.DebtorTransDetailDebtorTransType ==. Just (fromEnum ST_SALESINVOICE)
                                , FA.DebtorTransDetailDebtorTransNo ==. Just (fromIntegral key)
                                , FA.DebtorTransDetailQuantity !=. 0
                                ]
                                [ Asc FA.DebtorTransDetailStockId
                                ]
  -- needed to get the delivery no corresponding to the invoice
  deliveryDetails <- selectList [ FA.DebtorTransDetailId <-. map (maybe (error "???") FA.DebtorTransDetailKey . FA.debtorTransDetailSrcId . entityVal) details
                                , FA.DebtorTransDetailDebtorTransType ==. Just (fromEnum ST_CUSTDELIVERY) -- shouldn't be necessary
                                ]
                                []
  let deliveryIds = nub $ sort $ map (FA.debtorTransDetailDebtorTransNo . entityVal) deliveryDetails

  deliveries <- selectList [ FA.DebtorTranTransNo <-. catMaybes deliveryIds
                           , FA.DebtorTranType ==. fromEnum ST_CUSTDELIVERY
                           ]
                           [
                           ]
  orders <- selectList [ FA.SalesOrderTransType ==. fromEnum ST_SALESORDER
                       , FA.SalesOrderOrderNo <-. map (FA.debtorTranOrder . entityVal) deliveries
                       ]
                       []
  return $ InvoiceInfo  key 
                        invoice 
                        comments
                        (zip (map entityVal deliveries)
                             (map entityVal orders)
                        )
                        (map entityVal details)


loadContactFor :: Int -> Maybe Int -> SqlHandler (Maybe FA.CrmPerson)
loadContactFor debtor branchm = do
  let actions = ["general", "delivery"]
      forDelivery (Entity _ contact) = FA.crmContactType contact == "delivery"
  -- load customer contacts
  --
  --l
  customerContacts <- selectList [ FA.CrmContactEntityId ==. Just (tshow debtor)
             , FA.CrmContactType ==. "customer"
             , FA.CrmContactAction <-. actions
             ] []

  branchContacts <- case branchm of
    Just branch -> selectList [ FA.CrmContactEntityId ==. Just (tshow branch)
                              , FA.CrmContactType ==. "cust_branch"
                              , FA.CrmContactAction <-. actions
                              ] []
    Nothing -> return []

  -- sort things to get the highest priority first
  let (custD, custG) = partition forDelivery customerContacts
      (branchD, branchG) = partition forDelivery branchContacts
  case branchD ++ custD ++ branchG ++ custG of
    [] -> return Nothing
    ((Entity _ contact):_) -> get $ FA.CrmPersonKey $ FA.crmContactPersonId contact
  

  

