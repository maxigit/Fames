{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
-- * Import
-- | Post event to FA using Curl
module WH.FA.Curl
( postStockAdjustment
, postLocationTransfer
, testFAConnection
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
      throwError $ "Failed to : " <> msg
                   <> tshow (respCurlCode r)
                   <> tshow (respStatusLine r)
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
  let loginOptions = CurlPostFields [ "user_name_entry_field="<>user
                                    , "password=" <> password
                                    , "company_login_name=0"
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
-- ** Util
-- | Extract the Id from the process adjustment response
extractAddedId :: [Tag String] -> Either Text Int
extractAddedId tags = let
  metas = sections (~== TagOpen ("meta" :: String) [("http-equiv","Refresh"), ("content","")]) tags
  in case metas of
      [meta:_] -> let
        url = fromAttrib "content" meta
        in case mrSubList $ url =~ ("AddedID=([0-9]+)$" :: String) of
             [s] -> Right $ Prelude.read  (traceId s)
             _ -> Left (pack "Error, can't find adjustment Id.")
      _ -> Left (fromMaybe "" $ extractErrorMsgFromSoup tags)
        
  
extractErrorMsgFromSoup :: [Tag [Element String]] -> Maybe Text
extractErrorMsgFromSoup tags = let
  errors = sections (~== TagOpen ("div" :: String) [("class", "err_msg")]) tags
  msgs = map (headEx .drop 1) errors -- get next tag
  in case msgs of
    [] -> Nothing
    _ -> Just . pack $ unlines (mapMaybe maybeTagText msgs)

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
-- ** Items
-- *** Stock Adjustment
addAdjustmentDetail :: (?curl :: Curl, ?baseURL:: String)
                    => StockAdjustmentDetail -> ExceptT Text IO [Tag String]
addAdjustmentDetail StockAdjustmentDetail{..} = do
  let items = CurlPostFields [ "AddItem=Add%20Item"
                             , "stock_id="<> unpack adjSku
                             , "std_cost=" <> show adjCost
                             , "qty=" <> show adjQuantity
                             ] : method_POST
  curlSoup ajaxInventoryAdjustmentURL items 200 "add items"
  

postStockAdjustment :: FAConnectInfo -> StockAdjustment -> IO (Either Text Int)
postStockAdjustment connectInfo stockAdj = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newAdjustmentURL method_GET 200 "Problem trying to create a new inventory adjustment"
    _ <- mapM addAdjustmentDetail (adjDetails (stockAdj :: StockAdjustment))
    let process = CurlPostFields [ "ref="<> (unpack $ adjReference (stockAdj :: StockAdjustment))
                                 , "Process=Process"
                                 , "AdjDate=" <>  toFADate (adjDate stockAdj)
                                 , "StockLocation=" <> unpack (adjLocation stockAdj) 
                                 , "type=1" -- Adjustment @TODO config file
                                 , "Increase=" <> if adjAdjType stockAdj == PositiveAdjustment 
                                                     then "1" else "0"
                                 ] : method_POST
    tags <- curlSoup ajaxInventoryAdjustmentURL process 200 "process inventory adjustment"
    case extractAddedId tags  of
            Left e -> throwError $ "Inventory Adjustment creation failed:" <> e
            Right faId -> return faId

-- *** Location Transfer

postLocationTransfer :: FAConnectInfo -> LocationTransfer -> IO (Either Text Int)
postLocationTransfer connectInfo locTrans = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newLocationTransferURL method_GET 200 "Problem trying to create a new location transfer"
    _ <- mapM addLocationTransferDetail (ltrDetails (locTrans :: LocationTransfer))
    let process = CurlPostFields [ "ref="<> (unpack $ ltrReference (locTrans :: LocationTransfer))
                                 , "Process=Process"
                                 , "AdjDate=" <>  toFADate (ltrDate locTrans)
                                 , "FromStockLocation=" <> unpack (ltrLocationFrom locTrans) 
                                 , "ToStockLocation=" <> unpack (ltrLocationTo locTrans) 
                                 ] : method_POST
    tags <- curlSoup (toAjax locationTransferURL) process 200 "process location transfer"
    case extractAddedId tags  of
            Left e -> throwError $ "Location Transfer creation failed:" <> e
            Right faId -> return faId

locationTransferURL :: (?baseURL :: URLString) => URLString
locationTransferURL = ?baseURL <> "/inventory/transfers.php"
newLocationTransferURL :: (?baseURL :: URLString) => URLString
newLocationTransferURL = locationTransferURL <> "?NewTransfer=1"


addLocationTransferDetail  :: (?baseURL :: URLString, ?curl :: Curl)
                           => LocationTransferDetail -> ExceptT Text IO [Tag String]
addLocationTransferDetail LocationTransferDetail{..} = do
  let items = CurlPostFields [ "AddItem=Add%20Item"
                             , "stock_id="<> unpack ltrSku
                             , "std_cost=0"
                             , "qty=" <> show ltrQuantity
                             ] : method_POST
  curlSoup (toAjax locationTransferURL) items 200 "add items"


-- ** Purchase
--- *** GRN
postGRN :: FAConnectInfo -> GRN -> IO (Either Text Int)
postGRN connectInfo grn = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    _ <- curlSoup newGRNURL method_GET 200 "Problem trying to create a new GRN"
    _ <- mapM addGRNDetail (grnDetails grn)
    let process = CurlPostFields [ "supplier_id=" <> show (grnSupplier grn)
                                 , "due_date=" <> toFADate (grnDeliveryDate grn)
                                 , maybe "" (("ref=" <>) . unpack) (grnReference grn)
                                 , maybe "" (("supp_ref=" <>) . unpack) (grnSupplierReference grn)
                                 , maybe "" (("delivery_address=" <>) . unpack) (grnDeliveryInformation grn)
                                 , "StkLocation=" <> unpack (grnLocation grn)
                                 , "Comments=" <> unpack (grnMemo grn)
                                 ] : method_POST
    tags <- curlSoup (ajaxGRNURL) process 200 "Create GRN"
    case extractAddedId tags of
      Left e -> throwError $ "GRN creation failed:" <> e
      Right faId -> return faId

addGRNDetail :: (?baseURL :: URLString, ?curl :: Curl)
             => GRNDetail -> ExceptT Text IO [Tag String]
addGRNDetail GRNDetail{..} = do
  let fields = CurlPostFields [ "EnterLine=Ad%20Item"
                             , "stock_id=" <> unpack grnSku
                             , "qty=" <> show grnQuantity
                             , "price=" <> show grnPrice
                             ] : method_POST
  curlSoup ajaxGRNURL fields 200 "add items"
