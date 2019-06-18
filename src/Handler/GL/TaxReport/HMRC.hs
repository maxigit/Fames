{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Handler.GL.TaxReport.HMRC where
import Import
import Network.Curl
import Curl
import GL.TaxReport.Settings 
import Control.Monad.Except
import Util.Cache
import Data.Dynamic(fromDynamic)
import Database.Persist.Sql (fromSqlKey)
import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(..))
import Data.Aeson.Types
import Data.Time(diffUTCTime)

-- * Types
data VATObligation = VATObligation
   { start :: Day
   , end :: Day
   , due :: Day
   , periodKey :: Text
   , received :: Maybe Day
   } deriving (Eq, Show, Read)

data VATReturn = VATReturn
   { 
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

-- ** Connection to HMRC website

--  Key used to cache auth information
data AuthKey = HMRCAuthKey Text  -- report Type
             | HMRCTokenKey Text
               deriving Show

getHMRCAuthorizationCode :: Text -> HMRCProcessorParameters -> Maybe TaxReportId -> Handler AuthorizationCode
getHMRCAuthorizationCode reportType HMRCProcessorParameters{..} reportM = do
  -- retrieve the authoration 
  let cacheKey = HMRCAuthKey reportType

  cvar <- getsYesod appCache
  cache <- readMVar cvar
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
                                     , "redirect_uri=http://localhost:3000/gl/tax/oauth" 
                                     , "state=" <> tshow (reportType, fromSqlKey <$> reportM)
                                     ] 
      redirect (authUrl :: Text)
  
setHMRCAuthorizationCode ::  Text -> HMRCProcessorParameters -> AuthorizationCode -> Handler ()
setHMRCAuthorizationCode reportType parameters (AuthorizationCode code) = do 
  -- cache0 True (cacheDay 1) HMRCAuthKey (return code)
  -- we don't cache the authorization anymore, we just convert it to a token straight away
  setWarning "HMRC authorization code processed"
  cache0 True (cacheMinute 10) (HMRCAuthKey reportType)  (return code)
  getHMRCToken reportType parameters Nothing
  return ()


getHMRCToken :: Text -> HMRCProcessorParameters -> (Maybe TaxReportId) -> Handler AuthorizationToken
getHMRCToken reportType params reportM = do
  cvar <- getsYesod appCache
  cache <- readMVar cvar
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
      token <- curlHMRCToken reportType params reportM 
      storeToken token
                     

curlHMRCToken :: Text -> HMRCProcessorParameters -> (Maybe TaxReportId) -> Handler AuthorizationToken
curlHMRCToken reportType params@HMRCProcessorParameters{..} reportM = do
  let endPoint = "/oauth/token" :: Text
      url = unpack $ baseUrl <> endPoint
  AuthorizationCode code <- getHMRCAuthorizationCode reportType params reportM
  tokenE <- hxtoHe . ioxToHx $ withCurl $ do
      curl <- lift initialize
      let ?curl = curl
      let
         opts = curlPostFields [ "client_secret" <=> clientSecret
                             , "client_id" <=> clientId
                             , Just "grant_type=authorization_code"
                             , Just "redirect_uri=http://localhost:3000/gl/tax/oauth"
                             , "code" <=> code
                             ] : CurlVerbose True : method_POST
      -- traceShowM ("OPT", opt)
      curlJson url  opts 200  "Getting HMRC Token"
  return $ either (error . unpack) id tokenE
  
refreshHMRCToken :: Text -> HMRCProcessorParameters -> AuthorizationToken -> Handler AuthorizationToken
refreshHMRCToken reportType params@HMRCProcessorParameters{..} token = do
  let endPoint = "/oauth/token" :: Text
      url = unpack $ baseUrl <> endPoint
  tokenE <- hxtoHe . ioxToHx $ withCurl $ do
      curl <- lift initialize
      let ?curl = curl
      let
        opts = curlPostFields [ "client_secret" <=> clientSecret
                                       , "client_id" <=> clientId
                                       , Just "grant_type=authorization_code"
                                       , Just "redirect_uri=http://localhost:3000/gl/tax/oauth"
                                       , "refresh_token" <=> (refreshToken token)
                                       ] : CurlVerbose True : method_POST
      -- traceShowM ("OPT", opt)
      curlJson url  opts 200  "Refreshing HMRC Token"
  return $ either (error . unpack) id tokenE

retrieveVATObligations :: Text -> Maybe TaxReport -> HMRCProcessorParameters -> Handler [VATObligation]
retrieveVATObligations reportType reportM params@HMRCProcessorParameters{..} = do
  token <- getHMRCToken reportType params Nothing
  let endPoint = "/organisations/vat/"<>vatNumber<>"/obligations?":: Text
  -- let endPoint = "/organisations/vat/"<>vatNumber<>"/obligations" :: Text
      url = unpack $ baseUrl <> endPoint <> intercalate "&" params
      params = case reportM of
        Nothing -> ["status=O" ]-- only open return. Otherwise wee need to give date
        Just TaxReport{..} -> [ formatTime0 "from=%Y-%m-%d" taxReportStart
                              , formatTime0 "to=%Y-%m-%d" taxReportEnd
                              ]
        

#if DEVELOPMENT
  traceShowM token
#endif
  -- only keep the obligation corresponding to given report if any
  -- it's not clear how HMRc filters obligations, so we might get more than needed
  let filterGood = case reportM of
                    Nothing -> id
                    Just TaxReport{..} -> let good VATObligation{..} = start == taxReportStart && end == taxReportEnd
                                          in filter good
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
#if DEVELOPMENT
                                                   , "Gov-Test-Scenario: " <?> obligationTestScenario
#endif
                                                   ]
                      )
      
                      : CurlVerbose True
                    : [] -- method_POST
                      ) 200 "Fetching VAT obligations"
    return r
  return $ either (error . unpack) (filterGood . findWithDefault [] "obligations" ) (obligationsE :: Either Text (Map Text [VATObligation]))


box'fields =  [ ("B1", "vatDueSales")
              , ("B2", "vatDueAcquisitions")
              , ("B3", "totalVatDue")
              , ("B4", "vatReclaimedCurrPeriod")
              , ("B5", "netVatDue")
              , ("B6", "totalValueSalesExVAT")
              , ("B7", "totalValuePurchasesExVAT")
              , ("B8", "totalValueGoodsSuppliedExVAT")
              , ("B9", "totalAcquisitionsExVAT")
              ]  :: [(Text, Text)]
submitHMRCReturn :: TaxReport -> Text -> [TaxReportBox] -> HMRCProcessorParameters -> Handler TaxReport
submitHMRCReturn report@TaxReport{..} periodKey boxes params@HMRCProcessorParameters{..} = do
  token <- getHMRCToken taxReportType params Nothing
  now <- liftIO getCurrentTime 
  let endPoint = "/organisations/vat/"<>vatNumber<>"/returns" :: Text
  -- let endPoint = "/organisations/vat/"<>vatNumber<>"/obligations" :: Text
      url = unpack $ baseUrl <> endPoint 
      boxMap = mapFromList $ map (fanl taxReportBoxName) boxes :: Map Text TaxReportBox
      mkBoxField  boxName fieldname =
        case lookup boxName boxMap  <|> lookup fieldname boxMap of
            Nothing -> Left $ "Can't find box " <> boxName <> " in current report"
            Just box -> Right (unpack fieldname <=> realFracToDecimal 2 $ taxReportBoxValue box)

      boxFieldsE = map (uncurry mkBoxField) box'fields
      boxFields = case sequence  boxFieldsE of
                    Left _ -> error $ show (lefts boxFieldsE)
                    Right bf -> bf
  r <- hxtoHe . ioxToHx $ withCurl $ do
    curl <- lift initialize
    let ?curl = curl
    let
      opts = curlPostFields ( "periodKey" <=> periodKey
                            : "finalised" <=> ("true" :: Text)
                            : boxFields
                            )
             :          (CurlHttpHeaders $ catMaybes [ Just "Accept: application/vnd.hmrc.1.0+json"
                                                   , "Authorization: " <?> ("Bearer " <> accessToken token)
#if DEVELOPMENT
                                                   , "Gov-Test-Scenario: " <?> submitTestScenario
#endif
                                                   ]
                        )
             : CurlVerbose True
             : method_POST 
    curlJson url opts 201 "submitting VAT return"
                 
  case r of
      Left e -> error $ unpack e
      Right json -> let
        submittedM = parseMaybe (withObject "" (.: "processingDate")) json
        in return $ report { taxReportSubmittedAt = submittedM <|> Just now
                                    , taxReportExternalReference = Just periodKey
                                    , taxReportExternalData = Just json
                                    }


    

  


  

  
