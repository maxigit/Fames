{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
-- * Import
-- | Post event to FA using Curl
module WH.FA.Curl
( postStockAdjustment
, postLocationTransfer
, testFAConnection
, postGRN
, postPurchaseInvoice
) where

import ClassyPrelude
import WH.FA.Types
import Network.Curl
import Control.Monad.Except

import qualified Prelude as Prelude
import Text.HTML.TagSoup 
import Text.Regex.TDFA

-- * Misc
-- ** FA specific
faDateFormat :: String
faDateFormat = "%Y/%m/%d"
-- faURL = "http://172.18.0.1"
toAjax :: URLString -> URLString
toAjax url = url <> "?jsHttpRequest=0-xml"

toFADate :: Day -> String
toFADate = formatTime defaultTimeLocale faDateFormat
-- ** Curl
docurl:: (?curl :: Curl) => URLString -> [CurlOption] -> ExceptT Text IO CurlResponse
docurl url opts = lift $ do_curl_ ?curl url opts

-- | send a request using curl an return a tag soup if successfull
curlSoup :: (?curl :: Curl)
         => URLString -> [CurlOption] -> Int -> Text -> ExceptT Text IO [Tag String]
curlSoup url opts status msg = do
  r <- docurl url opts
  let tags = parseTags (respBody r)
  when (respCurlCode r /= CurlOK || respStatus r /= status) $ do
      throwError $ unlines [ "Failed to : " <> msg
                           , "CURL status: " <> tshow (respCurlCode r)
                           , "HTTP status :" <> pack (respStatusLine r)
                           , "when accessing URL: '" <> tshow url <> "'"
                           , "If the problem persits, contact your administrator."
                           ]
  case (extractErrorMsgFromSoup tags) of
    Nothing -> return tags
    Just err -> throwError err

withCurl :: ExceptT e' IO b -> ExceptT e' IO b
withCurl = mapExceptT withCurlDo
  

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
                  200 "log in FrontAccounting"
    lift $ setopts curl [CurlCookieFile "cookies"]
    m

-- *** Post Paramters
class CurlPostField a where
  toCurlPostField :: a -> Maybe String


instance CurlPostField String where
  toCurlPostField = Just

instance CurlPostField Text where
  toCurlPostField = Just . unpack

instance CurlPostField Day where 
  toCurlPostField = Just . toFADate

instance CurlPostField Double where
  toCurlPostField  = Just . show

instance CurlPostField Int where
  toCurlPostField  = Just . show

instance CurlPostField a => CurlPostField (Maybe a) where
  toCurlPostField = (>>= toCurlPostField)
  
curlPostFields :: [Maybe String] -> CurlOption
curlPostFields = CurlPostFields . catMaybes

-- | Creates a 
(<=>) :: CurlPostField a => String -> a -> Maybe String
field <=> value = fmap ((field <> "=") <> ) (toCurlPostField value)

-- ** Util
-- | Extract the Id from the process adjustment response
extractAddedId' :: String -> String -> [Tag String] -> Either Text Int
extractAddedId' addedTag info tags = let
  metas = sections (~== TagOpen ("meta" :: String) [("http-equiv","Refresh"), ("content","")]) tags
  in case metas of
      [meta:_] -> let
        url = fromAttrib "content" meta
        in case mrSubList $ url =~ (addedTag <> "=([0-9]+)$" :: String) of
             [s] -> Right $ Prelude.read  (traceId s)
             _ -> Left (pack $ "Error, can't find " ++ info ++ " Id.")
      _ -> Left (fromMaybe "" $ extractErrorMsgFromSoup tags)
        
  
extractErrorMsgFromSoup :: [Tag [Element String]] -> Maybe Text
extractErrorMsgFromSoup tags = let
  errors = sections (~== TagOpen ("div" :: String) [("class", "err_msg")]) tags
  msgs = map (headEx .drop 1) errors -- get next tag
  in case msgs of
    [] -> Nothing
    _ -> Just . pack $ unlines (mapMaybe maybeTagText msgs)

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
-- *** Purchases
grnURL :: (?baseURL :: URLString) => URLString
grnURL = ?baseURL <> "/purchasing/po_entry_items.php"
newGRNURL = grnURL <> "?NewGRN=Yes"
ajaxGRNURL = toAjax grnURL

purchaseInvoiceURL :: (?baseURL :: URLString) => URLString
purchaseInvoiceURL = ?baseURL <> "/purchasing/supplier_invoice.php"
newPurchaseInvoiceURL = purchaseInvoiceURL <> "?New=Yes"
ajaxPurchaseInvoiceURL = toAjax purchaseInvoiceURL
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
  curlSoup ajaxInventoryAdjustmentURL items 200 "add items"
  

postStockAdjustment :: FAConnectInfo -> StockAdjustment -> IO (Either Text Int)
postStockAdjustment connectInfo stockAdj = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newAdjustmentURL method_GET 200 "Problem trying to create a new inventory adjustment"
    _ <- mapM addAdjustmentDetail (adjDetails (stockAdj :: StockAdjustment))
    let process = curlPostFields [ "ref" <=> (unpack $ adjReference (stockAdj :: StockAdjustment))
                                 , Just "Process=Process"
                                 , "AdjDate" <=>  toFADate (adjDate stockAdj)
                                 , "StockLocation" <=> adjLocation stockAdj 
                                 , Just "type=1" -- Adjustment @TODO config file
                                 , "Increase" <=> if adjAdjType stockAdj == PositiveAdjustment 
                                                     then "1" else "0" :: String
                                 ] : method_POST
    tags <- curlSoup ajaxInventoryAdjustmentURL process 200 "process inventory adjustment"
    case extractAddedId' "AddedID" "adjustment" tags  of
            Left e -> throwError $ "Inventory Adjustment creation failed:" <> e
            Right faId -> return faId

-- *** Location Transfer

postLocationTransfer :: FAConnectInfo -> LocationTransfer -> IO (Either Text Int)
postLocationTransfer connectInfo locTrans = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newLocationTransferURL method_GET 200 "Problem trying to create a new location transfer"
    _ <- mapM addLocationTransferDetail (ltrDetails (locTrans :: LocationTransfer))
    let process = curlPostFields [ "ref" <=> ltrReference locTrans
                                 , Just "Process=Process"
                                 , "AdjDate" <=>  toFADate (ltrDate locTrans)
                                 , "FromStockLocation" <=> unpack (ltrLocationFrom locTrans) 
                                 , "ToStockLocation" <=> unpack (ltrLocationTo locTrans) 
                                 ] : method_POST
    tags <- curlSoup (toAjax locationTransferURL) process 200 "process location transfer"
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
  curlSoup (toAjax locationTransferURL) items 200 "add items"


-- ** Purchase
--- *** GRN
postGRN :: FAConnectInfo -> GRN -> IO (Either Text Int)
postGRN connectInfo grn = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    new <- curlSoup newGRNURL method_GET 200 "Problem trying to create a new GRN"
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
    tags <- curlSoup (ajaxGRNURL) process 200 "Create GRN"
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
  curlSoup ajaxGRNURL fields 200 "add items"
  
--- *** Invoice
postPurchaseInvoice :: FAConnectInfo -> PurchaseInvoice -> IO (Either Text Int)
postPurchaseInvoice connectInfo PurchaseInvoice{..} = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    new <- curlSoup newPurchaseInvoiceURL method_GET 200 "Problem trying to create a new GRN"
    _ <- mapM addPurchaseInvoiceDetail poiGLItems
    ref <- case extractInputValue "reference" new of
                  Nothing -> throwError "Can't find Invoice reference"
                  Just r -> return r
    let process = curlPostFields [ "supplier_id" <=> poiSupplier
                                 , "reference" <=> fromMaybe ref poiReference
                                 , "supp_reference" <=> poiSupplierReference
                                 , "tran_date" <=> poiDate
                                 , "due_date" <=> poiDate
                                 , "Comments" <=> poiMemo
                                 , Just "PostInvoice=Enter%20Invoice" -- Pressing commit button
                                 ] : method_POST
    tags <- curlSoup (ajaxPurchaseInvoiceURL) process 200 "Create Purchase Invoice"
    traceShowM tags
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
  curlSoup (ajaxPurchaseInvoiceURL) fields 200 "add GL items"
