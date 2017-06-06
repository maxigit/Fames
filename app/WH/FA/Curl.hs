{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DisambiguateRecordFields #-}
-- | Post event to FA using Curl
module WH.FA.Curl
( postStockAdjustment
-- , postLocationTransfer
) where

import ClassyPrelude
import WH.FA.Types
import Network.Curl
import Control.Monad.Except

import qualified Prelude as Prelude
import Text.HTML.TagSoup 
import Text.Regex.TDFA

faDateFormat = "%Y/%m/%d" :: String
faURL = "http://127.0.0.1"
inventoryAdjustmentURL = ?baseURL <> "/inventory/adjustments.php"
newAdjustmentURL = inventoryAdjustmentURL <> "?NewAdjustment=1"
toAjax url = url <> "?jsHttpRequest=0-xml"
ajaxInventoryAdjustmentURL = toAjax inventoryAdjustmentURL


docurl:: (?curl :: Curl) => URLString -> [CurlOption] -> ExceptT Text IO CurlResponse
docurl url opts = lift $ do_curl_ ?curl url opts

withCurl = mapExceptT withCurlDo
  
addAdjustmentDetail :: (?curl :: Curl, ?baseURL:: String) => StockAdjustmentDetail -> ExceptT Text IO ()
addAdjustmentDetail StockAdjustmentDetail{..} = do
  let items = CurlPostFields [ "AddItem=Add%20Item"
                             , "stock_id="<> unpack sku
                             , "std_cost=" <> show cost
                             , "qty=" <> show quantity
                             ] : method_POST
  r <- docurl ajaxInventoryAdjustmentURL items :: ExceptT Text IO CurlResponse

  maybe (return ()) throwError (extractErrorMsg r)
  

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
    r <- docurl ?baseURL (loginOptions <> [CurlCookieJar "cookies", CurlCookieSession True])
    lift $ setopts curl [CurlCookieFile "cookies"]
    if respCurlCode r /= CurlOK || respStatus r /= 200
       then throwError "Failed to log in to FrontAccounting"
       else m

postStockAdjustment :: StockAdjustment -> IO (Either Text Int)
postStockAdjustment stockAdj = do
  let ?baseURL = faURL
  runExceptT $ withFACurlDo "curl" "purl" $ do
    r <- docurl newAdjustmentURL method_GET
    when (respCurlCode r /= CurlOK || respStatus r /= 200) $ do
       throwError "Problem trying to create a new inventory adjustment"
    mapM addAdjustmentDetail (details (stockAdj :: StockAdjustment))
    let process = CurlPostFields [ "ref="<> (unpack $ reference stockAdj)
                                 , "Process=Process"
                                 , "AdjDate=" <>  formatTime defaultTimeLocale faDateFormat (date stockAdj)
                                 , "StockLocation=" <> unpack (location stockAdj) 
                                 , "type=1" -- Adjustment @TODO config file
                                 , "Increase=" <> if adjType stockAdj == PositiveAdjustment 
                                                     then "1" else "0"
                                 ] : method_POST
    r <- docurl ajaxInventoryAdjustmentURL process
    when (respCurlCode r /= CurlOK || respStatus r /= 200) $ do



      throwError $ "Failed to add process session : "
                   <> tshow (respCurlCode r)
                   <> tshow (respStatusLine r)
    lift $ Prelude.putStrLn (respBody r)
    case extractNewAdjustmentId (respBody r) of
            Left e -> throwError $ "Inventory Adjustment creation failed:" <> e
            Right id -> return id

-- | Extract the Id from the process adjustment response
extractNewAdjustmentId :: String -> Either Text Int
extractNewAdjustmentId response = let
  tags = parseTags response
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
