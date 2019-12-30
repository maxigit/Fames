{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Handler.GL.TaxReport.HMRC where
import Import
import Network.Curl
import Curl
import GL.TaxReport.Settings 

import Util.Cache
import Data.Dynamic(fromDynamic)

import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Types
import Data.Aeson(encode)
import Data.Time(diffUTCTime)

import Data.Fixed
import Data.Yaml.Pretty(encodePretty, defConfig)

-- * Types
data VATObligation = VATObligation
   { start :: Day
   , end :: Day
   , due :: Day
   , periodKey :: Text
   , received :: Maybe Day
   } deriving (Eq, Show, Read)

data VATReturn = VATReturn
   { vr_periodKey :: Text
   , vr_vatDueSales :: Fixed E2
   , vr_vatDueAcquisitions :: Fixed E2
   , vr_totalVatDue :: Fixed E2
   , vr_vatReclaimedCurrPeriod :: Fixed E2
   , vr_netVatDue :: Fixed E2
   , vr_totalValueSalesExVAT :: Fixed E0
   , vr_totalValuePurchasesExVAT :: Fixed E0
   , vr_totalValueGoodsSuppliedExVAT :: Fixed E0
   , vr_totalAcquisitionsExVAT :: Fixed E0
   , vr_finalised :: Bool
   } deriving (Eq, Show, Read)

newtype AuthorizationCode = AuthorizationCode Text
data AuthorizationToken = AuthorizationToken
  { accessToken :: Text
  , tokenType :: Text
  , expiresIn :: Int
  , refreshToken :: Text
  , scope :: Text -- should be a list
   } deriving (Eq, Show, Read)

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_'  } ''AuthorizationToken)
$(deriveJSON defaultOptions ''VATObligation)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''VATReturn)

-- | Weither corrections from the past can be submitted or
data CorrectionStatus
  = CorrectionOK -- ^ No correction neeed
  | CorrectionDisplayNotice -- ^ ask the user to check that correctin are within threshold
  | CorrectionManual -- ^ Can't do it. Things in the past needs to be uncollected
  deriving Show

-- ** Connection to HMRC website

--  Key used to cache auth information
data AuthKey = HMRCAuthKey Text  -- report Type
             | HMRCTokenKey Text
               deriving Show

getHMRCAuthorizationCode :: Text -> HMRCProcessorParameters -> Handler AuthorizationCode
getHMRCAuthorizationCode reportType HMRCProcessorParameters{..} = do
  -- retrieve the authoration 
  let cacheKey = HMRCAuthKey reportType

  cvar <- getsYesod appCache
  cache <- toCacheMap <$> readMVar cvar
  refereeM <- getCurrentRoute
  -- traceShowM $ ("KEYS", keys cache, "KEY", cacheKey)
  case lookup (show cacheKey) cache of
    Just mvar -> do -- extract value
      (dyn, _) <- readMVar mvar 
      case fromDynamic dyn of
        Nothing -> error $ "Authorization code of the wrong type for key: " <> show cacheKey
        Just code -> return $ AuthorizationCode code
    Nothing -> do
      let 
        authUrl = intercalate "&"  $ [ baseUrl <> "/oauth/authorize?response_type=code"
                                     , "client_id=" <> clientId
                                     , "scope=read:vat+write:vat"
                                     , "redirect_uri=" <> fromMaybe "http://localhost:3000" redirectURI
                                                       <> "/gl/tax/oauth" 
                                     , "state=" <> tshow (reportType, refereeM) --  fromSqlKey <$> reportM)
                                     ] 
      redirect (authUrl :: Text)
  
setHMRCAuthorizationCode ::  Text -> HMRCProcessorParameters -> AuthorizationCode -> Handler ()
setHMRCAuthorizationCode reportType parameters (AuthorizationCode code) = do 
  -- cache0 True (cacheDay 1) HMRCAuthKey (return code)
  -- we don't cache the authorization anymore, we just convert it to a token straight away
  setInfo "HMRC authorization code processed"
  _ <- cache0 True (cacheMinute 10) (HMRCAuthKey reportType)  (return code)
  _ <- getHMRCToken reportType parameters
  return ()


getHMRCToken :: Text -> HMRCProcessorParameters -> Handler AuthorizationToken
getHMRCToken reportType params = do
  cvar <- getsYesod appCache
  cache <- toCacheMap <$> readMVar cvar
  let storeToken token = cache0 True (cacheHour 4) cacheKey (return token)
      cacheKey = HMRCTokenKey reportType
  case lookup (show cacheKey) cache of
    Just mvar -> do -- extract value
      (dyn, cacheTime) <- readMVar mvar 
      case fromDynamic dyn of
        Nothing -> error $ "Authorization code of the wrong type for key: " <> show cacheKey
        Just token -> do
          -- we check the expiration 
          now <- liftIO getCurrentTime 
          if diffUTCTime now cacheTime >= fromIntegral (expiresIn token)
            then do
              refreshed <- refreshHMRCToken reportType params token
              storeToken refreshed
            else
              return token
    Nothing -> do
      token <- curlHMRCToken reportType params
      storeToken token
                     

curlHMRCToken :: Text -> HMRCProcessorParameters -> Handler AuthorizationToken
curlHMRCToken reportType params@HMRCProcessorParameters{..} = do
  let endPoint = "/oauth/token" :: Text
      url = unpack $ baseUrl <> endPoint
  AuthorizationCode code <- getHMRCAuthorizationCode reportType params
  tokenE <- hxtoHe . ioxToHx $ withCurl $ do
      curl <- lift initialize
      let ?curl = curl
      let
         opts = curlPostFields [ "client_secret" <=> clientSecret
                             , "client_id" <=> clientId
                             , Just "grant_type=authorization_code"
                             , "redirect_uri" <=> (fromMaybe "http://localhost:3000" redirectURI <> "/gl/tax/oauth" )
                             , "code" <=> code
                             ] : CurlVerbose True : method_POST
      -- traceShowM ("OPT", opt)
      curlJson url  opts [200]  "Getting HMRC Token"
  return $ either (error . unpack) id tokenE
  
refreshHMRCToken :: Text -> HMRCProcessorParameters -> AuthorizationToken -> Handler AuthorizationToken
refreshHMRCToken __reportType HMRCProcessorParameters{..} token = do
  let endPoint = "/oauth/token" :: Text
      url = unpack $ baseUrl <> endPoint
  tokenE <- hxtoHe . ioxToHx $ withCurl $ do
      curl <- lift initialize
      let ?curl = curl
      let
        opts = curlPostFields [ "client_secret" <=> clientSecret
                                       , "client_id" <=> clientId
                                       , Just "grant_type=authorization_code"
                                       , "redirect_uri=" <=> (fromMaybe "http://localhost:3000" redirectURI <> "/gl/tax/oauth" )
                                       , "refresh_token" <=> (refreshToken token)
                                       ] : CurlVerbose True : method_POST
      -- traceShowM ("OPT", opt)
      curlJson url  opts [200]  "Refreshing HMRC Token"
  return $ either (error . unpack) id tokenE

retrieveVATObligations :: Text -> Maybe TaxReport -> HMRCProcessorParameters -> Handler [VATObligation]
retrieveVATObligations reportType reportM params@HMRCProcessorParameters{..} = do
  token <- getHMRCToken reportType params
  forM_ obligationTestScenario $ \testScenario -> do
    setWarning [shamlet|Using Gov Test Scenario: #{tshow testScenario}|]
  let endPoint = "/organisations/vat/"<>vatNumber<>"/obligations?":: Text
  -- let endPoint = "/organisations/vat/"<>vatNumber<>"/obligations" :: Text
      url = unpack $ baseUrl <> endPoint <> intercalate "&" l_params
      l_params = case reportM of
        Nothing -> ["status=O" ]-- only open return. Otherwise wee need to give date
        Just TaxReport{..} -> [ formatTime0 "from=%Y-%m-%d" taxReportStart
                              , formatTime0 "to=%Y-%m-%d" taxReportEnd
                              ]
-- #if DEVELOPMENT
  -- traceShowM token
-- #endif
  -- only keep the obligation corresponding to given report if any
  -- it's not clear how HMRc filters obligations, so we might get more than needed
  let filterGood = case reportM of
                    Nothing -> id
                    Just TaxReport{..} -> let good VATObligation{..} = start == taxReportStart && end == taxReportEnd
                                          in filter good
      -- go err json = case err of
      --               200 -> case fromJSON json of
      --                         Success obs -> Right obs
      --                         Error e -> Left (pack e)
      --               _ -> Left . decodeUtf8 $ encodePretty defConfig json 
  obligationsE <- hxtoHe . ioxToHx $ withCurl $ do
    curl <- lift initialize
    let
      ?curl = curl
    r <- curlJson url (-- curlPostFields [ -- Just "from=2018/01/11"
                                     --, Just "to=2019/02/01"
                                     -- ]
                    -- :
                        (CurlHttpHeaders $ catMaybes [ Just "Accept: application/vnd.hmrc.1.0+json"
                                                   , "Authorization: " <?> ("Bearer " <> accessToken token)
-- #if DEVELOPMENT
                                                   , "Gov-Test-Scenario: " <?> obligationTestScenario
 -- #endif
                                                   ]
                      )
      
                      : CurlVerbose True
                    : [] -- method_POST
                      ) [200] "Fetching VAT obligations"
    return r
  return $ either (error . unpack) (filterGood . findWithDefault [] "obligations" ) (obligationsE :: Either Text (Map Text [VATObligation]))


submitHMRCReturn :: TaxReport -> Text -> [TaxReportBox] -> HMRCProcessorParameters -> Handler TaxReport
submitHMRCReturn report@TaxReport{..} periodKey boxes params@HMRCProcessorParameters{..} = do
  token <- getHMRCToken taxReportType params
  now <- liftIO getCurrentTime 
  let endPoint = "/organisations/vat/"<>vatNumber<>"/returns" :: Text
  -- let endPoint = "/organisations/vat/"<>vatNumber<>"/obligations" :: Text
      url = unpack $ baseUrl <> endPoint 
      vatReturn = either (error . unpack) id $ mkVatReturn periodKey True boxes
  -- traceShowM ("VATRe", vatReturn)
  r <- hxtoHe . ioxToHx $ withCurl $ do
    curl <- lift initialize
    let ?curl = curl
    let
      opts = curlPostFields [Just $ unpack . decodeUtf8 $  encode vatReturn] 
             :          (CurlHttpHeaders $ catMaybes [ Just "Accept: application/vnd.hmrc.1.0+json"
                                                     , Just "Content-Type: application/json"
                                                     , "Authorization: " <?> ("Bearer " <> accessToken token)
-- #if DEVELOPMENT
                                                     , "Gov-Test-Scenario: " <?> submitTestScenario
-- #endif
                                                     ]
                        )
             : CurlVerbose True
             : method_POST 
    curlJson url opts [201] "submitting VAT return"
                 
  case r of
      Left e -> error $ unpack e
      Right json -> let
        submittedM = parseMaybe (withObject "" (.: "processingDate")) json
        in return $ report { taxReportSubmittedAt = submittedM <|> Just now
                                    , taxReportExternalReference = Just periodKey
                                    , taxReportExternalData = Just json
                                    }


    
mkVatReturn :: Text -> Bool -> [TaxReportBox] -> Either Text VATReturn
mkVatReturn vr_periodKey vr_finalised boxes = do -- Either 
  let
    boxMap = mapFromList $ map (fanl taxReportBoxName) boxes :: Map Text TaxReportBox
    getValue boxname = maybe  (Left $ "Box " <> boxname <> " doesn't exist") (Right . toFixed . taxReportBoxValue) (lookup boxname boxMap)
    getValue0 boxname = maybe  (Left $ "Box " <> boxname <> " doesn't exist") (Right . toFixed . taxReportBoxValue) (lookup boxname boxMap)
    toFixed f = fromRational (toRational f)
  vr_vatDueSales <- getValue "B1"
  vr_vatDueAcquisitions <- getValue "B2"
  vr_totalVatDue <- getValue "B3"
  vr_vatReclaimedCurrPeriod <- getValue "B4"
  vr_netVatDue <- abs <$> getValue "B5"
  vr_totalValueSalesExVAT <- getValue0 "B6"
  vr_totalValuePurchasesExVAT <- getValue0 "B7"
  vr_totalValueGoodsSuppliedExVAT <- getValue0 "B8"
  vr_totalAcquisitionsExVAT <- getValue0 "B9"

  return VATReturn{..}

-- * Fraud Validation headers
-- | Validates the fraud preventions using HMRC test website
validateHMRCFraudPreventionHeaders :: Text -> HMRCProcessorParameters -> Handler Html
validateHMRCFraudPreventionHeaders taxReportType params = do
  token <- getHMRCToken taxReportType params
  let endPoint = "/test/fraud-prevention-headers/validate" 
      url = unpack $ baseUrl params <> endPoint
      pprint :: Value -> Text
      pprint = decodeUtf8 . encodePretty defConfig
  result <- hxtoHe . ioxToHx $ withCurl $ do
    curl <- lift initialize
    let ?curl = curl
    json <- curlJson url (
               (CurlHttpHeaders $ catMaybes [ Just "Accept: application/vnd.hmrc.1.0+json"
                                            , Just "Content-Type: application/json"
                                            , "Authorization: " <?> ("Bearer " <> accessToken token)
                                            ]
               )
               : CurlVerbose True
               : [] -- method_POST
         ) [] "Fetching VAT obligations"
    return (json :: Value)
  defaultLayout [whamlet|
     <textarea style='width=100%;height=20em' readonly>#{either tshow pprint result}|]



  

  
  
