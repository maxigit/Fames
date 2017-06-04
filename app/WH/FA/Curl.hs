{-# LANGUAGE OverloadedStrings #-}
-- | Post event to FA using Curl
module WH.FA.Curl
( postStockAdjustment
-- , postLocationTransfer
) where

import ClassyPrelude
import WH.FA.Types
import Network.Curl

import qualified Prelude as Prelude


faURL = "http://127.0.0.1"
inventoryAdjustmentURL = faURL <> "/inventory/adjustments.php"
newAdjustmentURL = inventoryAdjustmentURL <> "?NewAdjustment=1"
toAjax url = url <> "?jsHttpRequest=0-xml"
ajaxInventoryAdjustmentURL = toAjax inventoryAdjustmentURL

addAdjustmentDetail :: Curl -> StockAdjustmentDetail -> IO (Either Text ())
addAdjustmentDetail curl StockAdjustmentDetail{..} = do
  let items = CurlPostFields [ "AddItem=Add%20Item"
                             , "stock_id="<> unpack sku
                             , "std_cost=" <> show cost
                             , "qty=" <> show quantity
                             ] : method_POST
  r <- do_curl_ curl ajaxInventoryAdjustmentURL items :: IO CurlResponse
  return (Right ())
  

-- | Open a Session  to FrontAccounting an execute curl statement
withFACurlDo :: String -> String -> String -> (Curl -> IO (Either Text a)) -> IO (Either Text a)
withFACurlDo faURL user password m = do
  let opts = [{-CurlCookieJar "cookies" ,-} CurlUserAgent "curl/7.47.0", CurlVerbose True ]
  let loginOptions = CurlPostFields [ "user_name_entry_field="<>user
                                    , "password=" <> password
                                    , "company_login_name=0"
                                    ] : method_POST
  withCurlDo $ do
    curl <- initialize
    setopts curl opts
    r <- do_curl_ curl faURL (loginOptions <> [CurlCookieJar "cookies", CurlCookieSession True]) :: IO CurlResponse
    setopts curl [CurlCookieFile "cookies"]
    if respCurlCode r /= CurlOK || respStatus r /= 200
       then return (Left "Failed to log in to FrontAccounting")
       else m curl 

postStockAdjustment ref stockAdj sku = do
  withFACurlDo faURL "curl" "purl" $ \curl -> do
    r <- do_curl_ curl newAdjustmentURL method_GET :: IO CurlResponse
    if respCurlCode r /= CurlOK || respStatus r /= 200
       then return (Left "Problem trying to create a new inventory adjustment")
       else do
          r <- addAdjustmentDetail curl (StockAdjustmentDetail sku 1 5)
          case r of
            Left _ -> return r
            Right _ ->  do
              -- finalize
                    let process = CurlPostFields [ "ref="++ref
                                                  , "Process=Process"
                                                  , "AdjDate=2017/06/03"
                                                  , "StockLocation=DEF"
                                                  ] : method_POST
                    r <- do_curl_ curl ajaxInventoryAdjustmentURL process :: IO CurlResponse 
                    if respCurlCode r /= CurlOK || respStatus r /= 200
                      then return $ Left ("Failed to add process session : " <> tshow (respCurlCode r) ++ tshow (respStatusLine r))
                      else do
                        Prelude.putStrLn (respBody r)
                        return (Right ())
                
          
test ref = postStockAdjustment ref undefined
