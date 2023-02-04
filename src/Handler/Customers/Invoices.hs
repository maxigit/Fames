{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Handler.Customers.Invoices
( getCustInvoicesR
, getCustInvoiceCCodesR
, postCustInvoiceDPDR
) where

import Import
import qualified FA as FA
import Customers.Settings
import Yesod.Form.Bootstrap3
import Handler.Items.Category.Cache
import Data.List(nub)
import Data.Maybe(fromJust)
import Handler.Customers.DPD
import Handler.Customers.ShippingDetails
import Data.ISO3166_CountryCodes
import qualified Data.Map as Map
import Data.Text(strip)
import Database.Persist.Sql (toSqlKey)
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

data CustomerInfo = CustomerInfo
  { cuDebtorNo :: Int64
  , cuDebtor :: FA.DebtorsMaster
  , cuBranch  :: FA.CustBranch
  , cuContact :: Maybe  FA.CrmPerson
  , custBranchArea :: FA.Area
  } 

--   __                  _   _                 
--  / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
-- | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- |  _| |_| | | | | (__| |_| | (_) | | | \__ \
-- |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
                                                              
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
          <td> <a href="@{CustomersR $ CustInvoiceCCodesR (fromIntegral $ FA.debtorTranTransNo inv) Nothing}">
                details
  |]
  where total invoice = sum $ map ($ invoice) [FA.debtorTranOvAmount, FA.debtorTranOvGst, FA.debtorTranOvFreight, FA.debtorTranOvFreightTax]
  

getCustInvoiceCCodesR :: Int64 -> Maybe Int64 -> Handler Html
getCustInvoiceCCodesR key detailIdM = do
  info@(InvoiceInfo{..}) <- runDB $ loadInvoiceInfo key
  [commodity_code, unit_weight, duty_] <- mapM categoryFinderCached ["commodity_code", "unit-weight-g", "duty"]
  (sForm, encType) <- dpdExportFrom info (toSqlKey <$> detailIdM)
  let categoryFor getCat detail =  getCat $ FA.StockMasterKey 
                                                  $ FA.debtorTransDetailStockId detail
  let ccode = fromMaybe "<Commodity Code>" . categoryFor commodity_code
  let weight = fromMaybe "<Weight>" . categoryFor unit_weight
  let duty = fromMaybe "<Duty>" . categoryFor duty_
  let table = [whamlet|
      <table *{datatable} data-tab-index=-1>
        <thead>
          <th> SKU
          <th> Quantity
          <th> Box Number
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
              <td>
                <input.input name="#{FA.debtorTransDetailStockId detail}-box" type=text>
              $with price <- FA.debtorTransDetailUnitPrice detail * (1 - FA.debtorTransDetailDiscountPercent detail)
                <td> #{formatDouble $ price }
                <td> #{formatDouble $ price * (1 - FA.debtorTransDetailPpd detail)}
              <td> #{ccode detail}
              <td> #{weight detail}
              <td> #{duty detail}
    |]
      info = infoPanel ("Info") [whamlet|$newline always
    <p.text-break>
      If custom data are required and there is more than one parcel,
        please fill for each items the Box Number cell in the following table.

        Box number can be a number, ex 
    <p.text-break>

            <code>2</code> :  the parcel number 2

        or in case of many items of the same style in different parcels,

        a space separated list  of quantity@box_number ex
    <p.text-break>

          <code>10@1 2@3</code> : 10 in box 1 and 2 in box 3
      |] 


  defaultLayout $ [whamlet|
    <form.form method=POST action="@{CustomersR $ CustInvoiceDPDR iiKey }" enctype="#{encType}">
      ^{info}
      ^{primaryPanel ("Invoice #" <> tshow key) table}
      ^{primaryPanel ("DPD Shipping") sForm}
    <form.form method=GET action="@{CustomersR $ DPDLookupR iiKey }">
        <button.btn.btn-info> Lookup
      |]

-- | Upload a cs
postCustInvoiceDPDR :: Int64 -> Handler TypedContent
postCustInvoiceDPDR key =  do
  info <- runDB$ loadInvoiceInfo key
  today <- todayH
  userm <- currentFAUser
  settingsm <- getsYesod (appDPDSettings . appSettings)
  let settings = fromMaybe (error "Shipping settings missing. Please contact your administrator.")
                           settingsm


  ((resp, __formW), __encType) <- runFormPost (shippingForm Nothing Nothing Nothing)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess params -> do
      categoryFinder <- categoryFinderCachedFor
                          [ "commodity_code"
                          , "unit-weight-g"
                          , "dpd-description"
                          , "dpd-product-type"
                          , "dpd-origin"
                          ]
      boxNumberMap <- getBoxNumberMap
      let delivery = mkDelivery info params settings
          productDetails = concatMap (applyBoxNumber boxNumberMap (shNoOfPackages params)
                                     . mkProductDetail categoryFinder UsePPD ) (iiDetails info)
      
          filename = shShortName params <> "-Invoice-" <> tshow  key <> ".csv"
          -- save shipping details
          -- we need as well as the shipping details key
          -- to save the details with the exact key as the one
          -- we use to search the detail initially
          shippingDetails = (toDetails userm "DPD" params )
                            { shippingDetailsLastUsed = Just today }
      let FA.DebtorTran{..} = iiInvoice info
      customerInfo <- runDB $ loadCustomerInfo (fromJust debtorTranDebtorNo) debtorTranBranchCode
      (_detailsFromInfo, (faDetails, _)) <- fillShippingForm info customerInfo Nothing
      if (shSave params) 
        then do
        runDB $ saveShippingDetails shippingDetails { shippingDetailsKey = unDetailsKey $ computeKey faDetails }
        else do -- update lastUsed if exist
         runDB $ updateWhere [ ShippingDetailsKey ==. unDetailsKey ( computeKey shippingDetails) ]
                             [ ShippingDetailsLastUsed =. Just today ]
          
      ---
      setAttachment $ fromString $ unpack filename
      respondSource ("text/csv") (makeDPDSource delivery productDetails .| mapC toFlushBuilder)

-- | Extract box number (in which box an item is)
-- from post data.
-- It's a tuple Quantity BoxNumber
-- It parse either : box or quantity@box
getBoxNumberMap :: Handler (Map Text (Either Int [(Int, Int)]))
getBoxNumberMap = do
  (pp,_) <- runRequestBody
  let parseQn :: Text -> Maybe (Either Int [(Int, Int)])
      parseQn t = case readMay t of
        Just q -> Just $ Left q
        Nothing -> Right <$>  traverse parseQn' (words t)
      -- parse quantity@box_number
      parseQn' t = case break (=='@') (unpack t) of
                        (q, '@':n) -> liftA2 (,) (readMay q) (readMay n)
                        _ -> Nothing
        
  return $ mapFromList $ [(sku, qn)
                         | (skuBox, t) <- pp
                         , Just sku <- [stripSuffix "-box" skuBox]
                         , Just qn <- [parseQn t]
                         , qn /= Right []
                         ]



applyBoxNumber :: Map Text (Either Int [(Int, Int)]) -> Int ->  ProductDetail -> [ ProductDetail ]
applyBoxNumber boxNumberMap noOfPackages detail =
  case (noOfPackages, lookup (productCode detail) boxNumberMap) of
    (1, _ ) -> [ detail { parcel = Right 1} ]
    (_, Nothing) -> [ detail ]
    (no, Just (Left p)) ->
      if p <= no 
      then [ detail { parcel = Right p } ]
      else [ notEnoughParcel p ]
    (no, Just (Right q'ns)) -> let
      totals = sum $ map fst q'ns
      in if totals > quantity detail
         then [ detail {parcel = Left $ pack $ "<To many items : " <> show totals <> " for " <> show (quantity detail) <> ">"} ]
         else map (\(q,n) -> if n > no
                             then notEnoughParcel n
                             else detail { parcel = Right n, quantity = q }
                  ) q'ns
  where notEnoughParcel n = detail {parcel = Left . pack $ "<" <> show n <> " > " <> show noOfPackages <> ">"}

    

  
mkDelivery :: InvoiceInfo -> ShippingForm -> DPDSettings -> Double -> Delivery
mkDelivery info ShippingForm{..} DPDSettings{..} customValue =
  Delivery{
    shipperContactTelephone = mconcat $ words shipperContactTelephone
    ,..
  } where
  reference = shShortName
  organisation'name = shOrganisation
  addressLine1'property'street = shAddress1
  addressLine2'locality = shAddress2
  addressLine3'City = shCity
  addressLine4'County'State = shCountyState
  postCode_7 = mconcat $ words shPostalCode -- remove space to fit in 7 chars
  countryCode_2 = fromJust shCountry
  additional_information = ""
  contactName = shContact
  contactTelephoneNumber = mconcat $ words shTelephone
  -- customValue = customValue
  description = "<description>"
  noOfPackages = shNoOfPackages
  notificationEmail = fromMaybe "" shNotificationEmail
  notificationSMSNumber = mconcat $ words $ fromMaybe "" shNotificationText
  serviceCode = shServiceCode
  totalWeightKg = shWeight
  generateCustomData = if shGenerateCustomData then Y else N
  invoiceType = Commercial
  invoiceReference = FA.debtorTranReference $ iiInvoice info
  countryOfOrigin = GB
  shipping'freightCost = FA.debtorTranOvFreight $ iiInvoice info
  reasonForExport = Sale
  receiverVAT'PID'EORI = fromMaybe "" shTaxId
  
data UsePPD = UsePPD | NoPPD deriving (Show)
   
mkProductDetail :: (Text -> FA.StockMasterId -> Maybe Text) -> UsePPD -> FA.DebtorTransDetail -> ProductDetail
mkProductDetail categoryFor usePPD FA.DebtorTransDetail{..} = 
  let 
    identifier = PRD
    productCode = debtorTransDetailStockId
    harmonisedCode = take 8 $  fromCat "commodity_code" "<CoCode>"
    unitWeight = maybe (Left  "<Unit Weight (Kg)>")
                       (Right . (/1000))
                       (fromCat' "unit-weight-g" >>= readMay)
    parcel = Left "<Box number>"
    description = fromCat "dpd-description" "<Description>"
    productType = fromCat "dpd-product-type" "<Product Type>"
    itemOrigin = fromCat "dpd-origin" "<Item Origin>"
    quantity = round $ debtorTransDetailQuantity
    unitValue = case usePPD of
                  UsePPD -> debtorTransDetailUnitPrice * (1-debtorTransDetailDiscountPercent) * (1-debtorTransDetailPpd) 
                  NoPPD  -> debtorTransDetailUnitPrice * (1 - debtorTransDetailDiscountPercent)
    ---------------------------------------------------------------------------
    fromCat cat def = fromMaybe def $ fromCat' cat
    fromCat' cat = categoryFor cat $ FA.StockMasterKey debtorTransDetailStockId
    ---------------------------------------------------------------------------
    -- If the weight is to low we need to make the item as a pack.
    -- For example 250 feathers should be seen as 1x250 with the calculated weight
    -- and amount. Without that, because of the minimum weigh of 10g, 250 feathers will be seen
    -- weighing 250x0.01= 2.5kg
  in case unitWeight of
      Right w | w {-  * debtorTransDetailQuantity -} < 0.01 ->
           ProductDetail{ quantity=1
                        , description= (fromString $ show quantity <> " x ") <>  description
                        , unitValue = fromIntegral quantity * unitValue
                        , unitWeight = fmap (fromIntegral quantity *) unitWeight
                        ,..
                        }
      _ -> ProductDetail{..} where
  
  
data ShippingForm = ShippingForm
  { shShortName :: Text
  , shCountry :: Maybe CountryCode -- mandatory but we need to not have a default value
  , shPostalCode :: Text
  , shOrganisation :: Text
  , shAddress1 :: Text
  , shAddress2 :: Maybe Text
  , shCity :: Text
  , shCountyState :: Maybe Text
  , shContact :: Text
  , shTelephone :: Text
  , shNotificationEmail :: Maybe Text
  , shNotificationText :: Maybe Text
  , shNoOfPackages :: Int
  , shWeight :: Double
  , shGenerateCustomData :: Bool
  , shTaxId :: Maybe Text
  -- , shCustomValue ::  Double
  , shServiceCode :: ServiceCode
  , shSave :: Bool
  } deriving Show

  
-- | prefill a shipping form, allowing the end user to check
-- and modify it if necessary.
-- Also returns an extra widget with extra information like
-- full address, order comments etc ...
fillShippingForm :: InvoiceInfo -> CustomerInfo -> Maybe (Key ShippingDetails)
                 -> Handler (ShippingForm, (ShippingDetails
                                           , Maybe (Match, ShippingDetails)))
fillShippingForm info customerInfo detailKeyM = do
  userm <- currentFAUser
  runDB $ do
    -- Get address and Co
    let customer = cuDebtor customerInfo
        personm = cuContact customerInfo
    let order = case reverse (iiDelivery'orders info) of
                   (_,o):_ -> o
                   [] -> error "Unexpected happend. Invoice should have an order"


    let shOrganisation = decodeHtmlEntities $  FA.debtorsMasterName customer
        shShortName = decodeHtmlEntities $  FA.debtorsMasterDebtorRef customer
        (shCountry, shGenerateCustomData, shServiceCode) = customerCountryInfo customerInfo
        (shAddress1, shAddress2, shCity, shPostalCode, shCountyState) =
          case lines (decodeHtmlEntities $ FA.salesOrderDeliveryAddress order) of
            [add1,city,zip] -> (add1, Nothing, city, zip, Nothing)
            (add1: add2: city: zip: county:_) -> (add1, Just add2, city, zip, Just county)
            [add1, add2, city, zip] -> (add1, Just add2, city, zip, Nothing)
            ls ->                       (unlines ls  , Nothing  , ""  , "" , Nothing)
        shContact = intercalate " " $ catMaybes [ FA.crmPersonName <$> personm
                                                , personm >>= FA.crmPersonName2
                                                ]
        shTelephone = fromMaybe "<Phone>" (personm >>= FA.crmPersonPhone)
        shNotificationEmail =  (personm >>= FA.crmPersonEmail) <|> (Just "<Email>")
        shNotificationText =  ((personm >>= FA.crmPersonPhone2) <|> (personm >>= FA.crmPersonPhone)) <|> (Just "<Text>")
        shNoOfPackages = 1
        shWeight = 9
        shTaxId = Just $ FA.debtorsMasterTaxId customer
        -- shCustomValue ::  Double
        -- shService = ""
        shSave = True
        form = truncateForm ShippingForm{..}
        details0 = toDetails userm "DPD" form
    detailsm <- case detailKeyM of
                  Just detailKey -> (\d -> (FullKeyMatch, Entity detailKey d)) <$$> get detailKey
                  Nothing -> getShippingDetails [minBound..maxBound] details0
    -- traceShowM ("DETAILS", detailsm)
    case detailsm of
      Nothing -> return (form, (details0, Nothing))
      Just (match, Entity _ details) -> do
        return  (fromDetails form details
                , (details0 , Just (match, details))
                )

customerCountryInfo :: CustomerInfo -> (Maybe CountryCode, Bool, ServiceCode)
customerCountryInfo CustomerInfo{..} = 
  case FA.areaDescription custBranchArea of 
    "UK" -> (Just GB, False, Parcel12AM)
    "Northern Ireland" -> (Just GB, False, ParcelTwoDay)
    "Rep. of Ireland" -> (Just IE, True, ParcelTwoDay)
    "Europe" -> -- extract the code from the VAT
      case lookup (take 2 $ FA.debtorsMasterTaxId cuDebtor) countryMap of
        _code@(Just JE) -> ( Just GB, True, ParcelTwoDay)
        code -> ( code
                , True
                , InternationalClassic
                )
    _ -> (Nothing, True, InternationalClassic)


countryMap :: Map Text CountryCode
countryMap = Map.fromList $ map (fanl tshow) [minBound..maxBound]


-- | Displays a form in the shape of a table showing also the values
-- present in FrontAccounting DPD
shippingForm :: Maybe ShippingDetails -> Maybe (Match, ShippingDetails) ->  Maybe ShippingForm 
              -> Html -> MForm Handler (FormResult ShippingForm, Widget)
shippingForm fam m'dpdm (shipm)  extra =  do
    shortName <- mreq textField (f 35 "Short Name") (ship <&> take 35 . shShortName)
    country <- mopt (selectField countryOptions) "Country" (ship <&> shCountry)
    postalCode <- mreq textField (f 7 "Postal/Zip Code") (ship <&> take 7 . shPostalCode)
    organisation <- mreq textField (f 35 "Organisation") (ship <&> take 35 . shOrganisation)
    address1 <- mreq textField (f 35 "Address 1") (ship <&> take 35 . shAddress1)
    address2 <- mopt textField (f 35 "Address 2") (ship <&> fmap (take 35) . shAddress2)
    city <- mreq textField (f 35 "City") (ship <&> take 35 . shCity)
    countyState <- mopt textField (f 35 "County/State") (ship <&> fmap (take 35) . shCountyState)
    contact <- mreq textField (f 25 "Contact") (ship <&> take 25 . shContact)
    telephone <- mreq textField (f 15 "Telephone") (ship <&> take 15 .  shTelephone)
    notificationEmail <- mopt textField (f 35 "Notification Email") (ship <&> fmap (take 35) . shNotificationEmail)
    notificationText <- mopt textField (f 35 "Notification Text") (ship <&>  fmap (take 35) .shNotificationText)
    noOfPackages <- mreq intField "No of Packages" Nothing
    weight <- mreq doubleField "Weight" (ship <&> shWeight)
    generateCustomData <- mreq boolField "Custom Data" (ship <&> shGenerateCustomData)
    taxId <- mopt textField (f 14 "EORI") (ship <&>  fmap (take 35) .shTaxId)
    serviceCode <- mreq (selectField serviceOptions) "Service" (ship <&> shServiceCode)
    save <- mreq boolField "Save" (ship <&> shSave)
    let widget = [whamlet|
     #{extra}
      <table.table.table-border>
        <thead>
          <th> 
          <th> 
          <th> FrontAccounting
          <th> DPD
        <tbody>
          ^{renderRow normalize shippingDetailsShortName shortName}
          ^{renderRow normalize shippingDetailsOrganisation organisation}
          ^{renderRow id (maybe "" readableCountryName . shippingDetailsCountry) country}
          ^{renderRow (mconcat . words)  shippingDetailsPostCode postalCode}
          ^{renderRow normalize shippingDetailsAddress1 address1}
          ^{renderRow normalize shippingDetailsAddress2 address2}
          ^{renderRow normalize shippingDetailsTown city}
          ^{renderRow normalize shippingDetailsCounty countyState}
          ^{renderRow normalize shippingDetailsContact contact}
          ^{renderRow normalize (fromMaybe "" . shippingDetailsTelephone) telephone}
          ^{renderRow normalize (fromMaybe "" . shippingDetailsNotificationEmail) notificationEmail}
          ^{renderRow normalize (fromMaybe "" . shippingDetailsNotificationText)  notificationText}
          ^{renderRow0'  noOfPackages (tlabel "Source") source}
          ^{renderRow0'  weight (tlabel "LastUsed") lastUsed}
          ^{renderRow0  generateCustomData}
          ^{renderRow id (fromMaybe "" . shippingDetailsTaxId) taxId}
          ^{renderRow0  serviceCode}
          ^{renderRow0' save         (tlabel "Match") keyMatch }
    |]
        normalize = toLower . strip
        keyMatch =  [whamlet|
            $case matchm
              $of Just FullKeyMatch 
               <td.text-success.bg-success> #{maybe "" tshow matchm}
              $of Nothing
               <td.bg-danger>
              $of _
               <td.text-warning.bg-warning> #{maybe "" tshow matchm}
               |]
        lastUsed = [whamlet| <td> #{tshowM (dpdm >>= shippingDetailsLastUsed)} |]
        source = [whamlet| <td> #{maybe "" shippingDetailsSource dpdm} |]

    return (noinline ( ShippingForm <$> fst shortName
                 <*> fst country
                 <*> fst postalCode
                 <*> fst organisation
                 <*> fst address1
                 <*> fst address2
                 <*> fst city
                 <*> fst countyState
                 <*> fst contact
                 <*> fst telephone
                 ) -- work around for GHC bug 
                 -- https://gitlab.haskell.org/ghc/ghc/-/issues/19695#note_34670
                 <*> fst notificationEmail
                 <*> fst notificationText
                 <*> fst noOfPackages
                 <*> fst weight
                 <*> fst generateCustomData
                 <*> fst taxId
                 <*> fst serviceCode
                 <*> fst save
          , widget)

  where
  {-# NOINLINE noinline #-}
  noinline = id
  ship = truncateForm <$> shipm
  (matchm, dpdm) = maybe (Nothing, Nothing) (\(a,b) -> (Just a, Just b)) m'dpdm
  countryOptions = optionsPairs $ map (fanl (p . readableCountryName)) [minBound..maxBound]
  serviceOptions = optionsPairs $ map (fanl tshow) [minBound..maxBound]
  -- fixed length text
  f n f = f { fsAttrs=[("maxlength", tshow n)] }
  -- help type inference
  p :: String -> Text
  p = pack
  shipd = toDetails Nothing "" <$> shipm
  renderRow :: _ => (a -> a) ->  (ShippingDetails -> a) -> _ -> _b
  renderRow f get (_res,view) = let
    faOk = fmap f (get <$> fam) == fmap f  valuem
    dpdOk = fmap f (get <$> dpdm) == fmap f valuem
    valuem = get <$> shipd
    (faClass, dpdClass) = case (faOk, dpdOk) of
      _ | matchm == Just FullKeyMatch -> ("", "text-success bg-success")
      (True, False) -> ("", "text-danger bg-danger") :: (Text, Text)
      (False, True) -> ("text-danger bg-danger", "")
      (True, True) -> ( "text-success bg-success","")

      _ -> ("", "")
    in [whamlet|
    <tr ##{fvId view} >
      <td> <label for=#{fvId view}> #{fvLabel view}
      <td> ^{fvInput view}
      <td class="#{faClass}"> #{maybe "" get fam}
      <td class="#{dpdClass}"> #{maybe "" get dpdm}
  |]
  renderRow0'(_res,view) w1 w2 = [whamlet|
    <tr ##{fvId view} >
      <td> <label for=#{fvId view}> #{fvLabel view}
      <td> ^{fvInput view}
      ^{w1}
      ^{w2}
  |]
  renderRow0 v = renderRow0' v td0 td0
  td :: Html -> Widget
  td h = [whamlet|<td>#{h}|]
  td0 = td ""
  tlabel :: Text -> Widget
  tlabel l = td [shamlet|<label>#{l}|]
    
-- | Truncate each fields of a form to its max length
truncateForm :: ShippingForm -> ShippingForm
truncateForm ShippingForm{..} =
  ShippingForm (take 35 shShortName)
               (shCountry)
               (take 7 $ joinSpaces shPostalCode)
               (take 35 shOrganisation)
               (take 35 shAddress1)
               (fmap (take 35) shAddress2)
               (take 35 shCity)
               (fmap (take 35) shCountyState)
               (take 25 shContact)
               (take 15  shTelephone)
               (fmap (take 35 . joinSpaces) shNotificationEmail)
               ( fmap (take 35 . joinSpaces)shNotificationText)
               (shNoOfPackages)
               (shWeight)
               (shGenerateCustomData)
               ( fmap (take 35)shTaxId)
               (shServiceCode)
               (shSave)
  
-- | Create form allowing to check and prefill information 
-- to be generated for DPD.
-- The order is one of the orders if many possibly the first or the last.
-- We use it for the address
-- dpdExportFrom :: FA.DebtorTrans -> FA.SalesOrder -> Form
dpdExportFrom info detailKeyM  = do
  let FA.DebtorTran{..} = iiInvoice info
  customerInfo <- runDB $ loadCustomerInfo (fromJust debtorTranDebtorNo) debtorTranBranchCode
  (shipping, (faDetails, dpdDetails)) <- fillShippingForm info customerInfo detailKeyM
  (form, encType) <- generateFormPost $ shippingForm (Just faDetails) dpdDetails
                                      $ Just shipping
  return ([whamlet|
      ^{invoiceSummary info}
      ^{contactSummary $ cuContact customerInfo}
      ^{form}
      <button.btn.btn-warning> Download
  |], encType)
   
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
         [ ("Reference" :: String,  Just $ decodeHtmlEntities  crmPersonRef)
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

loadCustomerInfo :: Int ->Int -> SqlHandler CustomerInfo
loadCustomerInfo debtorNo branchNo = do
  let cuDebtorNo = fromIntegral debtorNo
  cuDebtor <- getJust $ FA.DebtorsMasterKey debtorNo
  cuBranch <- getJust $ FA.CustBranchKey branchNo debtorNo
  cuContact <- loadContactFor debtorNo (Just branchNo)
  custBranchArea <- getJust $ FA.AreaKey $ fromJust $ FA.custBranchArea cuBranch

  return $ CustomerInfo{..}



toDetails :: Maybe FA.User  -> Text ->  ShippingForm -> ShippingDetails
toDetails userm shippingDetailsCourrier ShippingForm{..} = details {shippingDetailsKey = key } where
  shippingDetailsShortName = shShortName
  shippingDetailsPostCode =  shPostalCode
  shippingDetailsCountry = shCountry
  shippingDetailsOrganisation = shOrganisation
  shippingDetailsAddress1 = shAddress1
  shippingDetailsAddress2 = fromMaybe "" shAddress2
  shippingDetailsTown = shCity
  shippingDetailsCounty = fromMaybe "" shCountyState
  shippingDetailsContact = shContact
  shippingDetailsTelephone = Just shTelephone
  shippingDetailsNotificationEmail = shNotificationEmail
  shippingDetailsNotificationText = shNotificationText
  shippingDetailsTaxId = shTaxId
  shippingDetailsLastUsed = Nothing
  shippingDetailsSource = maybe "<Anonymous>" FA.userUserId userm
  details = ShippingDetails{shippingDetailsKey="",..}
  DetailsKey key = computeKey details



  


fromDetails :: ShippingForm -> ShippingDetails -> ShippingForm
fromDetails template ShippingDetails{..} = ShippingForm{..} where
  shShortName = shippingDetailsShortName
  shCountry = shippingDetailsCountry
  shPostalCode =  shippingDetailsPostCode
  shOrganisation = shippingDetailsOrganisation
  shAddress1 = shippingDetailsAddress1
  shAddress2 = Just shippingDetailsAddress2
  shCity = shippingDetailsTown
  shCountyState = Just shippingDetailsCounty
  shContact = shippingDetailsContact
  shTelephone = fromMaybe "" shippingDetailsTelephone
  shNotificationEmail = shippingDetailsNotificationEmail
  shNotificationText = shippingDetailsNotificationText
  shTaxId = shippingDetailsTaxId
  ShippingForm{shNoOfPackages, shWeight, shGenerateCustomData, shServiceCode, shSave} = template
  
  
