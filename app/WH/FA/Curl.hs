{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
-- * Import
-- | Post event to FA using Curl
module WH.FA.Curl
( postStockAdjustment
, postLocationTransfer
, testFAConnection
, postBankPayment
, postBankPaymentOrDeposit
, postGRN
, postPurchaseInvoice
, postSupplierPayment
, postPurchaseCreditNote
, postVoid
) where

import ClassyPrelude
import WH.FA.Types
import Network.Curl
import Control.Monad.Except
import Control.Monad.State
import Data.List(isPrefixOf)
import qualified Data.Map as Map
import Data.Monoid(Sum(Sum))
import Data.Decimal

import qualified Prelude as Prelude
import Text.HTML.TagSoup 
import Text.HTML.TagSoup.Match
import Text.Regex.TDFA
import Curl

-- * Misc
-- ** FA specific
faDateFormat :: String
faDateFormat = "%Y/%m/%d"
-- faURL = "http://172.18.0.1"
toAjax :: URLString -> URLString
toAjax url = url <> "?jsHttpRequest=0-xml"

toFADate :: Day -> String
toFADate = formatTime defaultTimeLocale faDateFormat

-- | Open a Session  to FrontAccounting an execute curl statement
withFACurlDo :: (?baseURL :: URLString)
             => String -> String -> ((?curl :: Curl) => ExceptT Text IO a) -> ExceptT Text IO a
withFACurlDo user password m = do
  let opts = [{-CurlCookieJar "cookies" ,-} CurlUserAgent "curl/7.47.0", CurlVerbose True ]
  let loginOptions = curlPostFields [ "user_name_entry_field" <=> user
                                    , "password" <=> password
                                    , "company_login_name" <=>  (0 :: Int)
                                    ] : method_POST
  withCurl $ do
    curl <- lift initialize
    let ?curl = curl
    lift $ setopts curl opts
    -- r <- docurl ?baseURL (loginOptions <> [CurlCookieJar "cookies", CurlCookieSession True])
    _ <- curlSoup ?baseURL (loginOptions <> [CurlCookieJar "cookies", CurlCookieSession True])
                  [200] "log in FrontAccounting"
    lift $ setopts curl [CurlCookieFile "cookies"]
    m

-- | send a request using curl an return a tag soup if successfull
curlSoup :: (?curl :: Curl)
         => URLString -> [CurlOption] -> [Int] -> Text -> ExceptT Text IO [Tag String]
curlSoup = doCurlWith (const go) where
  go body = let
    tags = parseTags body
    in case (extractErrorMsgFromSoup tags) of
      Nothing -> return tags
      Just err -> throwError $ {- traceShowId -} err

instance CurlPostField Day where 
  toCurlPostField = Just . toFADate

-- ** Util
-- | Extract the Id from the process adjustment response
extractAddedId' :: String -> String -> [Tag String] -> Either Text Int
extractAddedId' addedTag info tags = let
  metas = sections (~== TagOpen ("meta" :: String) [("http-equiv","Refresh"), ("content","")]) tags
  in case metas of
      [meta:_] -> let
        url = fromAttrib "content" meta
        in case mrSubList $ url =~ (addedTag <> "=([0-9]+)(&|$)" :: String) of
             [s,_] -> Right $ Prelude.read  s
             _ -> Left (pack $ "Error, can't find " ++ info ++ " Id.")
      _ -> Left (fromMaybe "" $ extractErrorMsgFromSoup tags)
        
  
extractErrorMsgFromSoup :: [Tag [Element String]] -> Maybe Text
extractErrorMsgFromSoup tags = let
  errors = sections (~== TagOpen ("div" :: String) [("class", "err_msg")]) tags
  -- ^ get ALL div.err_msg (they can be many)
  msgs = map (takeWhile (~/= (TagClose ("div" :: String)))) errors
  in case msgs of
    [] -> Nothing
    _ -> Just . pack $ unlines (concatMap (mapMaybe maybeTagText) msgs)

extractSuccessMsgFromSoup :: [Tag [Element String]] -> Either Text Text
extractSuccessMsgFromSoup tags = let
  successs = sections (~== TagOpen ("div" :: String) [("class", "note_msg")]) tags
  msgs = map (headEx .drop 1) successs -- get next tag
  in case msgs of
    [] -> maybe (Left "Can't find success message") Left (extractErrorMsgFromSoup tags)
    _ -> Right . pack $ unlines (mapMaybe maybeTagText msgs)
-- | Extract value from a input field. This allows for example to use the generated
-- reference instead of having to specify one.
extractInputValue :: String -> [Tag String] -> Maybe Text
extractInputValue inputName tags = let
  inputs = sections (~== TagOpen ("input" :: String) [ ("name", inputName), ("value","") ] ) tags
  r = case inputs of
    [input:_] -> case fromAttrib "value" input of
                       "" -> Nothing
                       value -> Just $ pack value
    _ -> Nothing -- 0 or to many matches
  in r
-- |
extractAllInputValues :: [Tag String] -> [(String, String)]
extractAllInputValues tags = mapMaybe extractInputValue' tags

extractInputValue' :: Tag String -> Maybe (String, String)
extractInputValue' tag | (tag ~== TagOpen ("input" :: String) [("name", ""), ("value", "")]) =
  Just (fromAttrib "name" tag, fromAttrib "value" tag)
extractInputValue' _ = Nothing

extractInputsToCurl :: [Tag String] -> CurlOption
extractInputsToCurl tags = curlPostFields $ [name <=> value | (name, value) <- extractAllInputValues tags]


-- ** Test
testFAConnection :: FAConnectInfo -> IO (Either Text ())
testFAConnection connectInfo = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    return ()
-- * Transactions
-- ** Urls
-- *** Items
inventoryAdjustmentURL :: (?baseURL :: URLString) => URLString
inventoryAdjustmentURL = ?baseURL <> "/inventory/adjustments.php"
newAdjustmentURL ::  (?baseURL :: URLString) => URLString
newAdjustmentURL = inventoryAdjustmentURL <> "?NewAdjustment=1"
ajaxInventoryAdjustmentURL  :: (?baseURL :: URLString) => URLString
ajaxInventoryAdjustmentURL = toAjax inventoryAdjustmentURL
-- *** GL
bankPaymentURL :: (?baseURL :: URLString) => URLString
bankPaymentURL = ?baseURL <> "/gl/gl_bank.php"
newBankPaymentURL :: (?baseURL :: URLString) => URLString
newBankPaymentURL = bankPaymentURL <>  "?NewPayment=Yes"
ajaxBankPaymentItemURL :: (?baseURL :: URLString) => URLString
ajaxBankPaymentItemURL = toAjax bankPaymentURL 

bankDepositURL :: (?baseURL :: URLString) => URLString
bankDepositURL = ?baseURL <> "/gl/gl_bank.php"
newBankDepositURL :: (?baseURL :: URLString) => URLString
newBankDepositURL = bankDepositURL <>  "?NewDeposit=Yes"
ajaxBankDepositItemURL :: (?baseURL :: URLString) => URLString
ajaxBankDepositItemURL = toAjax bankDepositURL 

-- *** Purchases
grnURL, newGRNURL, ajaxGRNURL :: (?baseURL :: URLString) => URLString
grnURL = ?baseURL <> "/purchasing/po_entry_items.php"
newGRNURL = grnURL <> "?NewGRN=Yes"
ajaxGRNURL = toAjax grnURL

purchaseInvoiceURL, newPurchaseInvoiceURL, ajaxPurchaseInvoiceURL :: (?baseURL :: URLString) => URLString
purchaseInvoiceURL = ?baseURL <> "/purchasing/supplier_invoice.php"
newPurchaseInvoiceURL = purchaseInvoiceURL <> "?New=Yes"
ajaxPurchaseInvoiceURL = toAjax purchaseInvoiceURL

purchaseCreditNoteURL, ajaxPurchaseCreditNoteURL :: (?baseURL :: URLString) => URLString
purchaseCreditNoteURL = ?baseURL <> "/purchasing/supplier_credit.php"
ajaxPurchaseCreditNoteURL = toAjax purchaseCreditNoteURL
newPurchaseCreditNoteURL :: (?baseURL :: URLString) => Maybe Int -> URLString
newPurchaseCreditNoteURL Nothing = purchaseCreditNoteURL <> "?New=1"
newPurchaseCreditNoteURL (Just invoiceNo) = newPurchaseCreditNoteURL Nothing <> "&invoice_no=" <> show invoiceNo
  

supplierPaymentURL, newSupplierPaymentURL, ajaxSupplierPaymentURL :: (?baseURL :: URLString) => URLString
supplierPaymentURL = ?baseURL <> "/purchasing/supplier_payment.php"
newSupplierPaymentURL = supplierPaymentURL
ajaxSupplierPaymentURL = toAjax supplierPaymentURL
-- *** Void
voidTransactionUrl :: (?baseURL :: String) => String
voidTransactionUrl = ?baseURL <> "/admin/void_transaction.php" 
ajaxVoidTransactionUrl = toAjax $ voidTransactionUrl

-- ** Items
-- *** Stock Adjustment
addAdjustmentDetail :: (?curl :: Curl, ?baseURL:: String)
                    => StockAdjustmentDetail -> ExceptT Text IO [Tag String]
addAdjustmentDetail StockAdjustmentDetail{..} = do
  let items = curlPostFields [ Just "AddItem=Add%20Item"
                             , "stock_id" <=> adjSku
                             , "std_cost" <=> adjCost
                             , "qty" <=> adjQuantity
                             ] : method_POST
  curlSoup ajaxInventoryAdjustmentURL items [200] "add items"
  

postStockAdjustment :: FAConnectInfo -> StockAdjustment -> IO (Either Text Int)
postStockAdjustment connectInfo stockAdj = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newAdjustmentURL method_GET [200] "Problem trying to create a new inventory adjustment"
    _ <- mapM addAdjustmentDetail (adjDetails (stockAdj :: StockAdjustment))
    let process = curlPostFields [ "ref" <=> (unpack $ adjReference (stockAdj :: StockAdjustment))
                                 , Just "Process=Process"
                                 , "AdjDate" <=>  toFADate (adjDate stockAdj)
                                 , "StockLocation" <=> adjLocation stockAdj 
                                 , Just "type=1" -- Adjustment @TODO config file
                                 , "Increase" <=> if adjAdjType stockAdj == PositiveAdjustment 
                                                     then "1" else "0" :: String
                                 ] : method_POST
    tags <- curlSoup ajaxInventoryAdjustmentURL process [200] "process inventory adjustment"
    case extractAddedId' "AddedID" "adjustment" tags  of
            Left e -> throwError $ "Inventory Adjustment creation failed:" <> e
            Right faId -> return faId

-- *** Location Transfer

postLocationTransfer :: FAConnectInfo -> LocationTransfer -> IO (Either Text Int)
postLocationTransfer connectInfo locTrans = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newLocationTransferURL method_GET [200] "Problem trying to create a new location transfer"
    _ <- mapM addLocationTransferDetail (ltrDetails (locTrans :: LocationTransfer))
    let process = curlPostFields [ "ref" <=> ltrReference locTrans
                                 , Just "Process=Process"
                                 , "AdjDate" <=>  toFADate (ltrDate locTrans)
                                 , "FromStockLocation" <=> unpack (ltrLocationFrom locTrans) 
                                 , "ToStockLocation" <=> unpack (ltrLocationTo locTrans) 
                                 , Just "type=0" -- @TODO config file
                                 ] : method_POST
    tags <- curlSoup (toAjax locationTransferURL) process [200] "process location transfer"
    case extractAddedId' "AddedID" "location transfer" tags  of
            Left e -> throwError $ "Location Transfer creation failed:" <> e
            Right faId -> return faId

locationTransferURL :: (?baseURL :: URLString) => URLString
locationTransferURL = ?baseURL <> "/inventory/transfers.php"
newLocationTransferURL :: (?baseURL :: URLString) => URLString
newLocationTransferURL = locationTransferURL <> "?NewTransfer=1"


addLocationTransferDetail  :: (?baseURL :: URLString, ?curl :: Curl)
                           => LocationTransferDetail -> ExceptT Text IO [Tag String]
addLocationTransferDetail LocationTransferDetail{..} = do
  let items = curlPostFields [ Just "AddItem=Add%20Item"
                             , "stock_id" <=> unpack ltrSku
                             , Just "std_cost=0"
                             , "qty" <=> show ltrQuantity
                             ] : method_POST
  curlSoup (toAjax locationTransferURL) items [200] "add items"


-- ** GL
postBankPayment :: FAConnectInfo -> BankPayment -> IO (Either Text Int)
postBankPayment connectInfo payment = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    new <- curlSoup newBankPaymentURL method_GET [200] "Problem trying to create new bank payment"
    _ <- mapM addBankPaymentItems (bpItems payment)
    let ref = case extractInputValue "ref" new of
                  Nothing -> Left "Can't find GRN reference"
                  Just r -> Right r
    let process = curlPostFields [ "date_" <=> bpDate payment
                                 , "ref" <=> either error id ref
                                 , Just "CheckTaxBalance=1"
                                 , Just "PayType=0" -- miscellaneous
                                 -- , Just "_ex_rate=1" -- exchange rate
                                 , "person_id" <=> bpCounterparty payment
                                 , "bank_account" <=> bpBankAccount payment
                                 , "memo_"  <=> bpMemo payment
                                 , Just "Process=Process"
                                 ] : method_POST
    tags <- curlSoup (toAjax bankPaymentURL) process [200] "Create bank payment"
    case extractAddedId' "AddedID" "bank payment" tags of
      Left e -> throwError $ "Bank payment creation failed:" <> e
      Right faId -> return faId

addBankPaymentItems :: (?baseURL :: URLString, ?curl :: Curl)
                    => GLItemD -> ExceptT Text IO [Tag String]
addBankPaymentItems GLItem{..} = do
  let fields = curlPostFields [  "code_id" <=> gliAccount
                              , "amount" <=> gliAmount
                              , "tax_net_amount" <=> gliTaxOutput
                              , "dimension_id" <=> fromMaybe 0 gliDimension1
                              , "dimension2_id" <=> fromMaybe 0 gliDimension2
                              , "LineMemo" <=> gliMemo
                              , Just "AddItem=Add%20Item"
                              ] :method_POST
  curlSoup (ajaxBankPaymentItemURL) fields [200] "add GL items"

-- | TODO factorize with postBankPayment
postBankDeposit :: FAConnectInfo -> BankDeposit -> IO (Either Text Int)
postBankDeposit connectInfo deposit = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    new <- curlSoup newBankDepositURL method_GET [200] "Problem trying to create new bank deposit"
    _ <- mapM addBankDepositItems (bdItems deposit)
    let ref = case extractInputValue "ref" new of
                  Nothing -> Left "Can't find GRN reference"
                  Just r -> Right r
    let process = curlPostFields [ "date_" <=> bdDate deposit
                                 , "ref" <=> either error id ref
                                 , Just "CheckTaxBalance=1"
                                 , Just "PayType=0" -- miscellaneous
                                 -- , Just "_ex_rate=1" -- exchange rate
                                 , "person_id" <=> bdCounterparty deposit
                                 , "bank_account" <=> bdBankAccount deposit
                                 , "memo_"  <=> bdMemo deposit
                                 , Just "Process=Process"
                                 ] : method_POST
    tags <- curlSoup (toAjax bankDepositURL) process [200] "Create bank deposit"
    case extractAddedId' "AddedDep" "bank deposit" tags of
      Left e -> throwError $ "Bank deposit creation failed:" <> e
      Right faId -> return faId

postBankPaymentOrDeposit :: FAConnectInfo -> Either BankDeposit BankPayment -> IO (Either Text (Int, FATransType))
postBankPaymentOrDeposit connectInfo =
  either (fmap (fmap . fmap . fmap $ (, ST_BANKDEPOSIT))  postBankDeposit connectInfo)
         (fmap (fmap .fmap . fmap $ (, ST_BANKPAYMENT)) postBankPayment connectInfo)

addBankDepositItems :: (?baseURL :: URLString, ?curl :: Curl)
                    => GLItemD -> ExceptT Text IO [Tag String]
addBankDepositItems GLItem{..} = do
  let fields = curlPostFields [  "code_id" <=> gliAccount
                              , "amount" <=> gliAmount
                              , "tax_net_amount" <=> gliTaxOutput
                              , "dimension_id" <=> fromMaybe 0 gliDimension1
                              , "dimension2_id" <=> fromMaybe 0 gliDimension2
                              , "LineMemo" <=> gliMemo
                              , Just "AddItem=Add%20Item"
                              ] :method_POST
  curlSoup (ajaxBankDepositItemURL) fields [200] "add GL items"
  
-- ** Purchase
-- *** GRN
postGRN :: FAConnectInfo -> GRN -> IO (Either Text Int)
postGRN connectInfo grn = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    new <- curlSoup newGRNURL method_GET [200] "Problem trying to create a new GRN"
    _ <- mapM addGRNDetail (grnDetails grn)
    
    let ref = case extractInputValue "ref" new of
                  Nothing -> error "Can't find GRN reference"
                  Just r -> r
    let process = curlPostFields [ "supplier_id" <=> grnSupplier grn
                                 , "OrderDate" <=> grnDeliveryDate grn
                                 , "ref" <=> (fromMaybe ref (grnReference grn))
                                 , "supp_ref" <=> grnSupplierReference grn
                                 , "delivery_address" <=> grnDeliveryInformation grn
                                 , "StkLocation" <=> grnLocation grn
                                 , "Comments" <=> grnMemo grn
                                 , Just "Commit=Process%20GRN" -- Pressing commit button
                                 ] : method_POST
    tags <- curlSoup (ajaxGRNURL) process [200] "Create GRN"
    case extractAddedId' "AddedGRN" "grn" tags of
      Left e -> throwError $ "GRN creation failed:" <> e
      Right faId -> return faId

addGRNDetail :: (?baseURL :: URLString, ?curl :: Curl)
             => GRNDetail -> ExceptT Text IO [Tag String]
addGRNDetail GRNDetail{..} = do
  let fields = curlPostFields [ Just "EnterLine=Ad%20Item"
                              , "stock_id" <=> unpack grnSku
                              , "qty" <=> show grnQuantity
                              , "price" <=> show grnPrice
                              ] : method_POST
  curlSoup ajaxGRNURL fields [200] "add items"
  
-- *** Invoice
postPurchaseInvoice :: FAConnectInfo -> PurchaseInvoice -> IO (Either Text Int)
postPurchaseInvoice connectInfo PurchaseInvoice{..} = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newPurchaseInvoiceURL  method_GET
                         [200] "Problem trying to create a new purchase invoice"
    -- we need to change the supplier id
    response <- curlSoup ajaxPurchaseInvoiceURL (curlPostFields [ "supplier_id" <=> poiSupplier
                                                                , "tran_date" <=> poiDate] : method_POST)
                        [200] "Problem changing supplier"
    del <- addPurchaseInvoiceDeliveries response poiDeliveryIds
    _ <- mapM addPurchaseInvoiceDetail poiGLItems
    ref <- case extractInputValue "reference" response of
                  Nothing -> throwError "Can't find Invoice reference"
                  Just r -> return r
    let process = curlPostFields [ "supplier_id" <=> poiSupplier
                                 , "reference" <=> fromMaybe ref poiReference
                                 , "supp_reference" <=> poiSupplierReference
                                 , "tran_date" <=> poiDate
                                 , "due_date" <=> poiDueDate
                                 , "Comments" <=> poiMemo
                                 , Just "PostInvoice=Enter%20Invoice" -- Pressing commit button
                                 ] : method_POST
    tags <- curlSoup (ajaxPurchaseInvoiceURL) process [200] "Create Purchase Invoice"
    case extractAddedId' "AddedID" "purchase invoice" tags of
      Left e -> throwError $ "Purchase invoice creation failed:\n" <> e
      Right faId -> return faId

addPurchaseInvoiceDetail:: (?baseURL :: URLString, ?curl :: Curl)
             => GLItem-> ExceptT Text IO [Tag String]
addPurchaseInvoiceDetail GLItem{..} = do
  let fields = curlPostFields [  "gl_code" <=> gliAccount
                              , "amount" <=> gliAmount
                              , "dimension_id" <=> gliDimension1
                              , "dimension2_id" <=> gliDimension2
                              , "memo_" <=> gliMemo
                              , Just "AddGLCodeToTrans=1"
                              ] :method_POST
  curlSoup (ajaxPurchaseInvoiceURL) fields [200] "add GL items"

-- **** Invoice Purchase Order Delivery
-- | Find all the fields corresponding to the invoicable delivery
-- and post then required one. We cheat a bit by simulated a press
-- to Add All GRN whereas we are in fact only supplying the items we want to deliver.
-- This works, because sending 'InvGRNAll' doesn't process All available items,
-- but only the ones provided in the POST parameters.
addPurchaseInvoiceDeliveries :: (?baseURL :: URLString, ?curl :: Curl)
                             => [Tag String] -> [(Int, Maybe Int)] -> ExceptT Text IO [Tag String]
addPurchaseInvoiceDeliveries tags deliveryIds = do
  let extra = curlPostFields [ Just "InvGRNAll=1"
                             ] : method_POST
  fields <- ExceptT $ return $ extractDeliveryItems tags deliveryIds 
  curlSoup (ajaxPurchaseInvoiceURL) (fields ++ extra) [200] "add delivery items"

extractDeliveryItems :: [Tag String] -> [(Int, Maybe Int)] -> Either Text [CurlOption]
extractDeliveryItems tags deliveryId'ns = do -- Either
  -- the description of the deliverable items are found in
  -- table row containing the delivery number in the first column
  -- so we need first, to get all <tr>.. block, and keep the one corresponding
  -- to the desired delivery
  let allRows = partitionWithClose (TagOpen "tr" []) (cleanTags tags)
      -- we match "<tr><td><a ..>deliveryId
      matchDelivery :: Int -> [Tag String] -> Bool
      matchDelivery deliveryId row =  case row of
        ( TagOpen "tr"  _ :
          TagOpen "td" [] :
          TagOpen "a" _   :
          TagText delivery :
          _) | Just d <- readMay (delivery :: String) -> d == deliveryId
        _ -> False
      findForDelivery :: [[Tag String ]] -> (Int, Maybe Int) -> Either Text [[Tag String]]
      findForDelivery rows (deliveryId, n)  = let
        found = filter (matchDelivery deliveryId) rows
        in case (found, n) of
                  (_, Just n) -> if n == length found
                                 then Right found
                                 else Left . pack $  "Found " ++ show (length found) ++ " items in delivery "
                                                     ++ " for delivery #" ++ show deliveryId
                                                     ++ ": " ++ show n ++ " expected."
                  ([] , _ ) -> Left . pack $ "Can't find any items for delivery #"  ++ show deliveryId
                  (_, _ ) -> Right found
      
  fields <- mapM (findForDelivery allRows) deliveryId'ns
  return $ map extractInputsToCurl (concat fields)
   
  

  
-- *** CreditNote
postPurchaseCreditNote :: FAConnectInfo -> PurchaseCreditNote -> IO (Either Text Int)
postPurchaseCreditNote connectInfo PurchaseCreditNote{..} = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup (newPurchaseCreditNoteURL pcnInvoiceNo )  method_GET
                         [200] "Problem trying to create a new Credit Note"
    -- we need to change the supplier id
    response <- curlSoup ajaxPurchaseCreditNoteURL (curlPostFields [ "supplier_id" <=> pcnSupplier
                                                                   , "tran_date" <=> pcnDate
                                                                   , "invoice_no" <=> pcnInvoiceNo
                                                                   ] : method_POST)
                        [200] "Problem changing supplier"
    -- del <- addPurchaseCreditNoteDeliveries response pcnDeliveryIds
    _ <- mapM addPurchaseCreditNoteDetail pcnGLItems
    ref <- case extractInputValue "reference" response of
                  Nothing -> throwError "Can't find CreditNote reference"
                  Just r -> return r
    let process = curlPostFields [ "supplier_id" <=> pcnSupplier
                                 , "reference" <=> fromMaybe ref pcnReference
                                 , "supp_reference" <=> pcnSupplierReference
                                 , "tran_date" <=> pcnDate
                                 , "due_date" <=> pcnDueDate
                                 , "invoice_no" <=> pcnInvoiceNo
                                 , "Comments" <=> pcnMemo
                                 , Just "PostCreditNote=Enter%20CreditNote" -- Pressing commit button
                                 ] : method_POST
    tags <- curlSoup (ajaxPurchaseCreditNoteURL) process [200] "Create Purchase CreditNote"
    case extractAddedId' "AddedID" "purchase invoice" tags of
      Left e -> throwError $ "Purchase invoice creation failed:\n" <> e
      Right faId -> return faId

addPurchaseCreditNoteDetail:: (?baseURL :: URLString, ?curl :: Curl)
             => GLItem-> ExceptT Text IO [Tag String]
addPurchaseCreditNoteDetail GLItem{..} = do
  let fields = curlPostFields [  "gl_code" <=> gliAccount
                              , "amount" <=> gliAmount
                              , "dimension_id" <=> gliDimension1
                              , "dimension2_id" <=> gliDimension2
                              , "memo_" <=> gliMemo
                              , Just "AddGLCodeToTrans=1"
                              ] :method_POST
  curlSoup (ajaxPurchaseCreditNoteURL) fields [200] "add GL items"

-- **** Credit Items from Invoice
-- | Find all the fields corresponding to the invoicable delivery
-- and post then required one. We cheat a bit by simulated a press
-- to Add All GRN whereas we are in fact only supplying the items we want to deliver.
-- This works, because sending 'InvGRNAll' doesn't process All available items,
-- but only the ones provided in the POST parameters.
_addPurchaseCreditNoteDeliveries :: (?baseURL :: URLString, ?curl :: Curl)
                             => [Tag String] -> [(Int, Maybe Int)] -> ExceptT Text IO [Tag String]
_addPurchaseCreditNoteDeliveries tags deliveryIds = do
  let extra = curlPostFields [ Just "InvGRNAll=1"
                             ] : method_POST
  fields <- ExceptT $ return $ extractDeliveryItems tags deliveryIds 
  curlSoup (ajaxPurchaseCreditNoteURL) (fields ++ extra) [200] "add delivery items"

-- *** Payment
postSupplierPayment :: FAConnectInfo -> SupplierPayment -> IO (Either Text Int)
postSupplierPayment connectInfo SupplierPayment{..} = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newSupplierPaymentURL method_GET
                  [200] "Problem creating supplier payment"
    -- we need to change the supplier and reception date to
    -- make correct transactions appears
    response <- curlSoup ajaxSupplierPaymentURL (curlPostFields [ "supplier_id" <=> spSupplier
                                                                , Just "_supplier_id_update=1"
                                                                , "bank_account" <=> spBankAccount
                                                                , Just "_bank_account_update=1"
                                                                , "DatePaid" <=> spDate
                                                                ] : method_POST)
                  [200] "Problem  setting payment supplier"
    let ref = case extractInputValue "ref" response of
                  Nothing -> error "Can't find Payment reference"
                  Just r -> r
    (allocFields, maxAllocated) <-  ExceptT . return $ makeSupplierPaymentAlloctionFields response spAllocatedTransactions
    let process = curlPostFields [ "supplier_id" <=> spSupplier
                                 , "bank_account" <=> spBankAccount
                                 , "DatePaid" <=> spDate
                                 , "amount" <=> spTotalAmount
                                 , "ref" <=> fromMaybe ref spReference
                                 , "charge" <=> spBankCharge
                                 , Just "ProcessSuppPayment=1"
                                 , "TotalNumberOfAllocs" <=> maxAllocated
                                 ] : method_POST
    tags <- curlSoup (ajaxSupplierPaymentURL) (process ++ allocFields) [200] "Create Supplier Payment"
    case extractAddedId' "AddedID" "supplier payment" tags of
      Left e -> throwError $ "Supplier payment creation failed:\n" <> e
      Right faId -> return faId
    
-- | extracts the field name and the amount left to allocate corresponding to the given invoice
extractSupplierPaymentToAllocateInformation :: [Tag String]
                                            ->  Map (Int, FATransType)  (Text, Double) 
extractSupplierPaymentToAllocateInformation tags =
  let allRows = partitionWithClose (TagOpen "tr" []) (cleanTags tags)
      -- goodRows =  filter allocatable allRows
      allocatable :: [Tag String] -> Maybe ((Int, FATransType), (Text, Double))
      allocatable = evalStateT $ do
        _ <- findTag isTagOpen
        transTypeTag <- findTag isTagText
        _ <- findTag $ tagOpenNameLit "a"
        transNoTag <- nextTag $ isTagText
        _ <- findTag $ tagOpen (=="td") (anyAttrValue (Data.List.isPrefixOf "maxval"))
        maxValTag <- nextTag isTagText
        fieldTag <- findTag $ tagOpenNameLit "input"
        transType' <- 
              case fromTagText transTypeTag of
                          "Supplier Invoice" -> return ST_SUPPINVOICE 
                          "Bank Deposit" -> return ST_BANKDEPOSIT
                          _ -> mzero
        transNo' <- maybe mzero return $ readMay $ fromTagText transNoTag
        maxValue <- tagTextToAmount maxValTag
        let field = fromAttrib "name" fieldTag

        return ((transNo', transType'), (pack field, maxValue))

   in  mapFromList $ mapMaybe allocatable allRows

-- findTag :: (a -> Bool) ->  StateT [a] Maybe a
findTag  p = do
  (x:xs) <- get 
  put xs
  if  p x
    then return x
    else findTag p

-- | similar to findTag, but have to be the next one ...
nextTag p = do
  (x:xs) <- get 
  put xs
  if  p x
    then return x
    else mzero

tagTextToAmount tag = let
  t' = fromTagText tag
  -- remove 
  t = filter (/=',') t'
  in maybe mzero return $ readMay t

-- | In order to not over allocate many payments to the same transaction
-- all allocation need to be done in one go
makeSupplierPaymentAlloctionFields :: [Tag String] -> [PaymentTransaction] -> Either Text ([CurlOption], Int)
makeSupplierPaymentAlloctionFields tags transactions = do
  let allocMap = extractSupplierPaymentToAllocateInformation tags
      ns = [ n+1
           | (fieldname, _) <- toList allocMap
           , Just n' <- return $ stripPrefix "amount" fieldname
           , Just n  <- return $ readMay n'
           ]
      maxAlloc = maximum $ ncons 0 ns
  fields <- flip evalStateT allocMap $ do
    fields <- mapM makeSupplierPaymentAlloctionField  (groupPaymentTransactions transactions)
    return $ mergePostFields fields
  return (fields, maxAlloc)
-- | In order to not over allocate many payments to one invoice
-- we need to update the available amount for each transaction.
-- Thus the state monad
makeSupplierPaymentAlloctionField :: PaymentTransaction
                                  -> StateT (Map (Int, FATransType) (Text, Double))  (Either Text)
                                            CurlOption
makeSupplierPaymentAlloctionField toallocate = do
  let PaymentTransaction transNo transType amount = toallocate
      transName = tshow transType <> " #" <> tshow transNo

  allocMap <- get
  (fieldname, available) <- lift $ maybe (Left $ "No transaction found for " <> transName)
                                         Right
                                         (lookup (transNo, transType) allocMap)
  let unallocated = "un_allocated" <> drop 6 fieldname -- change amount/unallocated
  if amount <= available
    then do
          -- we can allocate this payment
          -- we need to update the available amount accordingly
          put $ insertMap (transNo, transType) (fieldname, available -amount) allocMap
          lift . Right $ curlPostFields [ unpack fieldname  <=> amount
                                        , unpack unallocated <=> (amount+1)   -- the amount
                                        -- wrong but should be enough to pass FA Test
                                        ]
    else lift . Left $ "Only " <> tshow available <> " to allocate for " <> transName

-- | Regroup Payment transaction per transaction
groupPaymentTransactions :: [PaymentTransaction] -> [PaymentTransaction]
groupPaymentTransactions transactions = let
  m = Map.fromListWith(<>) [ ((no, typ), Sum amount)
                  | (PaymentTransaction no typ amount) <- transactions
                  ]

  in [ PaymentTransaction no typ amount 
     | ((no, typ), Sum amount) <- mapToList m
     ]
  

-- ** Voiding
postVoid ::  FAConnectInfo -> VoidTransaction -> IO (Either Text Int)
postVoid connectInfo VoidTransaction{..} = do
  let ?baseURL = faURL connectInfo
  let fields = curlPostFields [ "filterType" <=>  fromEnum vtTransType
                              , "trans_no" <=> vtTransNo
                              , "select_id" <=> vtTransNo
                              , "memo_" <=> vtComment
                              , "date_" <=> vtDate
                              , "FromTransNo" <=> vtTransNo
                              , "ToTransNo" <=> vtTransNo
                              , Just "ConfirmVoiding=1"
                              ] : method_POST
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    tags <- curlSoup ajaxVoidTransactionUrl fields [200] "Void Transaction"
    case extractSuccessMsgFromSoup tags of
      Right msg | "Selected transaction has been voided." `ClassyPrelude.isPrefixOf` msg -> return 1
      Right e ->  throwError $ "Unexpected success msg: ["  <> e <> "]"
      Left e  -> throwError e


{-
  
  filterType=4
FromTransNo=1
ToTransNo=999999
trans_no=266
selected_id=266
date_=2019/04/15
memo_=test%20void
ProcessVoiding=Void%20Transaction
_focus=filterType
_modified=0
_token=8e40312b63862417a34b8025a91e6bdbc5055af9960ecae35b1c7646d3c9925e
_random=881903.1138965458
-}
  
