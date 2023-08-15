{-# LANGUAGE ImplicitParams #-}
module Handler.Customers.Map
( getCustMapR
, getCustRefreshGeodataR 
)
where

import Import
import Database.Persist.MySQL(rawSql, Single(..))
import Text.Printf
import Text.Julius(RawJS(..))
import Yesod.Form.Bootstrap3 (renderBootstrap3, BootstrapFormLayout(..))
import GL.Utils(calculateDate, DateCalculator(..))
import Data.Text (splitOn)
import Network.Curl
import Curl
import Control.Monad.Except hiding (mapM_, forM_)
import Data.Aeson

data Marker = Marker
     { name :: Text
     , latitude :: Double
     , longitude :: Double
     , description :: Text
     , lastOrder :: Day
     , radius :: Double
     , colour :: Text
     , opacity :: Double
     }
     deriving Show

omsWidget :: Text -> [Marker] -> Widget
omsWidget key markers = do
  addStylesheetRemote "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
  addStylesheetRemote "https://unpkg.com/leaflet-geosearch@3.8.0/dist/geosearch.css"
  let markerText = intercalate @[String] ", " $ map (\Marker{..} -> printf "{lat: %f, lng: %f,name: \"%s\",description: \"%s\", last:\"%s\", radius: %f, color:\"%s\", opacity: %f}"   latitude longitude name description (showDate lastOrder) radius colour opacity) markers
      showDate d =  formatTime defaultTimeLocale "%a %d %h %Y" d
  [whamlet|
    <div#map>
    <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
    <script src="https://unpkg.com/leaflet-geosearch@3.8.0/dist/geosearch.umd.js">

  |]
  toWidget [cassius|
    #map 
       height: 800px
       width: 100%
  |]
  toWidget [julius|
           document.addEventListener("DOMContentLoaded", function () {
             const markersData = [
             #{rawJS markerText}
             ];

             const map = L.map("map").setView([54.9701, -2.4636], 6);

             L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
               attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
             }).addTo(map);

             // Add custom markers to the map
             markersData.forEach(marker => {
               L.circleMarker([marker.lat, marker.lng],
                              {radius: marker.radius, opacity: .8*marker.opacity + 0.5, fillOpacity: 0.5*marker.opacity, color: marker.color, fillColor: marker.color})
                 .addTo(map)
                 .bindPopup(`<b>${marker.name}</b><br>${marker.description}<hr>last order: ${marker.last}`);
             });
          // Initialize Nominatim search control
          const provider = new GeoSearch.OpenStreetMapProvider({param: {
              email: '#{key}'}});
           const search = new GeoSearch.GeoSearchControl({
            provider: new GeoSearch.OpenStreetMapProvider(),
           style: 'bar',
           retainZoomLevel: true,
           autoClose: true,
           keepResult: true,
          });
          map.addControl(search);
  });
  |]


custForm from = renderBootstrap3 BootstrapInlineForm form where
  form = (,) <$> areq dayField "From" (Just from)
             <*> aopt filterEField "SKU" Nothing

getCustMapR :: Handler Html
getCustMapR = do
  keym <- appOpenStreetMapKey <$> getsYesod appSettings
  today <- todayH
  let defaultFrom = calculateDate (AddYears (-1)) today
  ((resp, form), encType) <- runFormGet $ custForm defaultFrom
  let (from, skuFilter ) = case resp of
                    FormSuccess p -> p
                    _ -> (defaultFrom, Nothing)
  case keym of
    Nothing -> error "Open Street Map key not provided"
    Just key -> do
      markers <- loadCustomerMarkers from skuFilter
      defaultLayout do
          [whamlet|
          <div.well>
            <form.form.form-inline role=form method=GET enctype=#{encType}>
              ^{form}
              <button.btn.btn-default type=submit> Submit
          ^{omsWidget key markers}
          |]
  
loadCustomerMarkers :: Day -> Maybe FilterExpression -> Handler [Marker]
loadCustomerMarkers from skuFilterM = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let sql :: Text
      sql = intercalate " " $ "SELECT name, SUM(unit_price*quantity)," :
          " 0_debtor_trans.debtor_no, 0_debtor_trans.branch_code, " :
          " br_post_address, geodata.latitude, geodata.longitude, formatted," :
          " datediff(now(), max(tran_date)), max(tran_date) " :
          " FROM 0_debtor_trans_details " :
          " JOIN 0_debtor_trans ON (0_debtor_trans_details.debtor_trans_no = 0_debtor_trans.trans_no " :
          " AND 0_debtor_trans_details.debtor_trans_type = 0_debtor_trans.type)  " :
          -- debtor master
          " JOIN 0_debtors_master USING(debtor_no)" :
          -- geodata 
          " JOIN 0_cust_branch using(debtor_no, branch_code) " :
          " LEFT JOIN fames_geodata as geodata on (geodata.address = 0_cust_branch.br_post_address) " :
          "WHERE type IN ("  :
          (tshow $ fromEnum ST_SALESINVOICE) :
          ",":
          (tshow $ fromEnum ST_CUSTCREDIT) :
          ") " :
          "AND quantity != 0" :
          "AND stock_id LIKE ? " : -- we don't want space between ' and stockLike
          "AND tran_date > ?" :
          skuFilter :
          "GROUP BY  0_debtor_trans.debtor_no, 0_debtor_trans.branch_code " :
          []
      (skuFilter, skup) = case skuFilterM of
                            Nothing -> ("", [])
                            Just f -> let (e, p) = filterEKeyword  f
                                      in ("AND stock_id " <> e, p)
      params = toPersistValue stockLike: toPersistValue from: skup
  sales <- runDB $ rawSql sql params 
  case sales of
    [] -> return []
    _ -> let maxAmount = maximumEx $ fmap (\(_, (Single amount, _, _), _ ,_) -> amount) sales
             maxAgo = maximumEx $ fmap (\(_, _, _ , (Single ago, _)) ->  ago) sales
                   in return $ map (mkMarker maxAmount maxAgo) sales
  where mkMarker :: Double -> Int -> (Single Text, (Single Double, Single Int64, Single Int64) , (Single Text, Single (Maybe Double), Single (Maybe Double), Single (Maybe Text)), (Single Int, Single Day)) -> Marker
        mkMarker maxAmount maxAgo (Single name, (Single amount, Single no, Single branch)
                             , (Single address, Single latm, Single longm, Single formattedm)
                             , (Single ago, Single lastOrder)
                 ) = let 
          latitude = fromMaybe (54 + fromIntegral no / 500) latm
          longitude = fromMaybe (2 + fromIntegral branch / 500) longm
          description = formatAddress case formattedm of
                                           Just f | not (null f) -> f
                                           _ -> address 
          radius = max 5 $ sqrt (amount / maxAmount) * 50
          opacity = (1 - fromIntegral ago / fromIntegral maxAgo) -- n of days till today
          colour = case (formattedm, latm) of
                    (Nothing, _) -> "orange" -- not done yet
                    (Just _, Nothing) -> "red" -- done but with an error
                    (Just _, Just _) -> if opacity > 0.8
                                        then "#3388ff" -- leaflet default
                                        else if opacity > 0.5
                                        then "#717afc"
                                        else if opacity > 0.3
                                        then "#8432ff"
                                        else "#a332ff"
          in Marker {..}
        formatAddress :: Text -> Text
        formatAddress ad = intercalate "<br>" $ lines ad
  
--------------------------------------------------
-- * Geodata
getCustRefreshGeodataR :: Handler Html
getCustRefreshGeodataR = do
  updateMissingGeodata
  setSuccess "Geodata updated"
  defaultLayout ""
  
  
data GeoGoogle
     = GeoGoogle { lat :: Double
                 , lng :: Double
                 , formattedAddress :: Text
                 }
      | ZeroResult
      | OtherStatus Text
     deriving Show
     
instance FromJSON GeoGoogle where
  parseJSON = withObject "GeoGoogle"  \o -> do
    status <- o .: "status"
    case status of
      "OK" -> do
        results <- o .: "results"
        case results of
          []           -> return ZeroResult
          or:_ -> withObject "results" (\r -> do
            formattedAddress <- r .:  "formatted_address"
            r .: "geometry" >>= withObject "geometry"  \geom -> do
                geom .: "location" >>= withObject "location" \loc -> do
                     lat <- loc .: "lat"
                     lng <- loc .: "lng"
                     return GeoGoogle{..}
                     ) or
      "ZERO_RESULTS" -> return ZeroResult
      _ -> do
        err <- o .:? "error_message"
        return $ OtherStatus $ status <> maybe "" (": " <>) err
       

googleGeocode :: Text -> Text -> ExceptT Text IO  GeoGoogle
googleGeocode apiKey address = do
  let url = printf "https://maps.googleapis.com/maps/api/geocode/json?key=%s&region=uk&address=%s,&sensor=false" apiKey (encodeAddress address)
      -- region=uk is a "region biais" in case there is more than one results
      -- https://developers.google.com/maps/documentation/geocoding/requests-geocoding#RegionCodes
      _t = url :: String
  withCurl do
         curl <- lift initialize
         let ?curl = curl
         curlJson url (
               (CurlHttpHeaders $ catMaybes [ Just "Accept: application/json"
                                            , Just "Content-Type: application/json"
                                            ]
               )
               : CurlVerbose True
               : CurlFollowLocation True
               : []
           ) [200] "???"
  
  
geocode :: Text -> Text -> ExceptT Text IO Geodata
geocode apiKey address = do
  googleGeo <- googleGeocode apiKey address
  case googleGeo of
    ZeroResult -> return $ Geodata address Nothing Nothing address
    GeoGoogle{..} -> return $ Geodata address (Just lat) (Just lng) formattedAddress
    (OtherStatus status) -> throwError status
                 


-- | Replace blank spaces by '%20'
encodeAddress :: Text -> Text
encodeAddress = intercalate "%20"
              . filter (not . null) -- remove consecutive spaces
              . splitOn " " 
              . intercalate " "
              . lines 
  
  
updateMissingGeodata :: Handler ()
updateMissingGeodata = do
  apiKeyM <- appGoogleApiKey <$> getsYesod appSettings
  case apiKeyM of
    Nothing -> error "Google API Key not provided"
    Just apiKey -> do
         let geocode' = ioxToHx . geocode apiKey
         addresses <- loadBranchAddressesWithoutGeodata
         -- ExceptT . stops if we exceed quotas
         e <- hxtoHe $ forM_ addresses \address -> do
           geodata <- geocode' address
           hToHx $ runDB $ insert_ geodata
         case e of
           Left err -> error $ unpack err
           _ -> return ()


loadBranchAddressesWithoutGeodata :: Handler [Text]
loadBranchAddressesWithoutGeodata = do
  let sql = "select br_post_address from 0_cust_branch "
         <> "left join fames_geodata as geo on (br_post_address = geo.address) "
         <> "where geo.address is null " 
         <> "limit 50" 
  rows <- runDB $ rawSql sql []
  return $ map unSingle rows

    
    
