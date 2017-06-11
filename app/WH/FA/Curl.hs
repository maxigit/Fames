{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
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

faDateFormat = "%Y/%m/%d" :: String
-- faURL = "http://172.18.0.1"
inventoryAdjustmentURL = ?baseURL <> "/inventory/adjustments.php"
newAdjustmentURL = inventoryAdjustmentURL <> "?NewAdjustment=1"
toAjax url = url <> "?jsHttpRequest=0-xml"
ajaxInventoryAdjustmentURL = toAjax inventoryAdjustmentURL


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
    Just error -> throwError error

withCurl = mapExceptT withCurlDo
  
addAdjustmentDetail :: (?curl :: Curl, ?baseURL:: String)
                    => StockAdjustmentDetail -> ExceptT Text IO [Tag String]
addAdjustmentDetail StockAdjustmentDetail{..} = do
  let items = CurlPostFields [ "AddItem=Add%20Item"
                             , "stock_id="<> unpack adjSku
                             , "std_cost=" <> show adjCost
                             , "qty=" <> show adjQuantity
                             ] : method_POST
  curlSoup ajaxInventoryAdjustmentURL items 200 "add items"
  

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
    curlSoup ?baseURL (loginOptions <> [CurlCookieJar "cookies", CurlCookieSession True])
             200 "log in FrontAccounting"
    lift $ setopts curl [CurlCookieFile "cookies"]
    m

postStockAdjustment :: FAConnectInfo -> StockAdjustment -> IO (Either Text Int)
postStockAdjustment connectInfo stockAdj = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    curlSoup newAdjustmentURL method_GET 200 "Problem trying to create a new inventory adjustment"
    mapM addAdjustmentDetail (adjDetails (stockAdj :: StockAdjustment))
    let process = CurlPostFields [ "ref="<> (unpack $ adjReference (stockAdj :: StockAdjustment))
                                 , "Process=Process"
                                 , "AdjDate=" <>  formatTime defaultTimeLocale faDateFormat (adjDate stockAdj)
                                 , "StockLocation=" <> unpack (adjLocation stockAdj) 
                                 , "type=1" -- Adjustment @TODO config file
                                 , "Increase=" <> if adjAdjType stockAdj == PositiveAdjustment 
                                                     then "1" else "0"
                                 ] : method_POST
    tags <- curlSoup ajaxInventoryAdjustmentURL process 200 "process inventory adjustment"
    case extractAddedId tags  of
            Left e -> throwError $ "Inventory Adjustment creation failed:" <> e
            Right id -> return id

-- | Extract the Id from the process adjustment response
extractAddedId :: [Tag String] -> Either Text Int
extractAddedId tags = let
  metas = sections (~== TagOpen ("meta" :: String) [("http-equiv","Refresh"), ("content","")]) tags
  in case (traceShowId metas) of
      [meta:_] -> let
        url = fromAttrib "content" meta
        in case mrSubList $ url =~ ("AddedID=([0-9]+)$" :: String) of
             [s] -> Right $ Prelude.read  (traceId s)
             _ -> Left (pack "Error, can't find adjustment Id.")
      _ -> Left (fromMaybe "" $ extractErrorMsgFromSoup tags)
        
  
extractErrorMsg :: CurlResponse -> Maybe Text
extractErrorMsg response = let
  tags = parseTags (respBody response)
  in extractErrorMsgFromSoup tags
  

extractErrorMsgFromSoup :: [Tag [Element String]] -> Maybe Text
extractErrorMsgFromSoup tags = let
  errors = sections (~== TagOpen ("div" :: String) [("class", "err_msg")]) tags
  msgs = map (headEx .drop 1) errors -- get next tag
  in case msgs of
    [] -> Nothing
    _ -> Just . pack $ unlines (mapMaybe maybeTagText msgs)


postLocationTransfer :: FAConnectInfo -> LocationTransfer -> IO (Either Text Int)
postLocationTransfer connectInfo locTrans = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    curlSoup newLocationTransferURL method_GET 200 "Problem trying to create a new location transfer"
    mapM addLocationTransferDetail (ltrDetails (locTrans :: LocationTransfer))
    let process = CurlPostFields [ "ref="<> (unpack $ ltrReference (locTrans :: LocationTransfer))
                                 , "Process=Process"
                                 , "AdjDate=" <>  formatTime defaultTimeLocale faDateFormat (ltrDate locTrans)
                                 , "FromStockLocation=" <> unpack (ltrLocationFrom locTrans) 
                                 , "ToStockLocation=" <> unpack (ltrLocationTo locTrans) 
                                 ] : method_POST
    tags <- curlSoup (toAjax locationTransferURL) process 200 "process location transfer"
    case extractAddedId tags  of
            Left e -> throwError $ "Location Transfer creation failed:" <> e
            Right id -> return id

locationTransferURL = ?baseURL <> "/inventory/transfers.php"
newLocationTransferURL = locationTransferURL <> "?NewTransfer=1"


addLocationTransferDetail LocationTransferDetail{..} = do
  let items = CurlPostFields [ "AddItem=Add%20Item"
                             , "stock_id="<> unpack ltrSku
                             , "std_cost=0"
                             , "qty=" <> show ltrQuantity
                             ] : method_POST
  curlSoup (toAjax locationTransferURL) items 200 "add items"

testFAConnection :: FAConnectInfo -> IO (Either Text ())
testFAConnection connectInfo = do
  let ?baseURL = faURL connectInfo
  runExceptT $ withFACurlDo (faUser connectInfo) (faPassword connectInfo) $ do
    return ()

  