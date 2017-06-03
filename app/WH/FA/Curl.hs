-- | Post event to FA using Curl
module WH.FA.Curl
( postStockAdjustment
-- , postLocationTransfer
) where

import ClassyPrelude
import WH.FA.Types
import Network.Curl


postStockAdjustment = do
  -- Init session
  let opts = [CurlCookieJar "cookies" , CurlVerbose True ]
      loginOptions = CurlPostFields [ "user_name_entry=curl"
                                    , "password=purl"
                                    , "company_login_name=0"
                                    ] : method_POST
      fa = "http:/127.0.0.1"
      adjUrl = fa <> "/inventory/adjusment.php?jsHttpRequest=0-xml"

  withCurlDo $ do
    curl <- initialize
    setopts curl opts
    r <- do_curl_ curl fa loginOptions :: IO CurlResponse
    if respCurlCode r /= CurlOK || respStatus r /= 302
       then error $ "Failed to log in: " ++ show (respCurlCode r) ++ respStatusLine r
       else do
          let items = CurlPostFields [ "AddItem=Add%20Item"
                                     , "stock_id=ML13-BD1-CHK"
                                     , "std_cost=3.5"
                                     , "qty=3"
                                     ] : method_POST
          r <- do_curl_ curl adjUrl items :: IO CurlResponse
          if respCurlCode r /= CurlOK || respStatus r /= 200
             then error $ "Failed to add object to session : " ++ show (respCurlCode r) ++ respStatusLine r
             else do
               let process = CurlPostFields [ "ref=test"
                                            , "Process=Process"
                                            , "AdjDate=2017/06/03"
                                            , "StockLocation=DEF"
                                            ] : method_POST
               r <- do_curl_ curl adjUrl process :: IO CurlResponse
               if respCurlCode r /= CurlOK || respStatus r /= 200
                 then error $ "Failed to add process session : " ++ show (respCurlCode r) ++ respStatusLine r
                 else do
                     return ()
      
           
      


  
  
