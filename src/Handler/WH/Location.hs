{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Location
( getWHStocktakeLocationR
, getWHLocationListR
, getWHLocationStickersR
) where

import Import
import qualified Data.Map as Map


-- * Requests
-- | Displays the list of locations
getWHStocktakeLocationR :: Handler Html
getWHStocktakeLocationR = do
  locations <- appStockLocationsInverse . appSettings <$> getYesod
  defaultLayout [whamlet|
<h1> Stock locations
  <form action="@{WarehouseR WHLocationListR}" method="GET">
    <button.btn.btn-default type="submit">Csv
  <form action="@{WarehouseR WHLocationStickersR}" method="GET">
    <button.btn.btn-default type="submit">Stickers
  <table.table.table-striped> 
    <tr>
      <th> Name
    $forall (shelf, location) <- Map.toList locations
      <tr>
        <td> #{location}: #{shelf}
|]

getWHLocationListR :: HandlerT App IO TypedContent
getWHLocationListR = do
  source <- csvSource
  respondSource "text/csv" (source =$= mapC toFlushBuilder)
  
  
getWHLocationStickersR :: HandlerT App IO TypedContent
getWHLocationStickersR = do
  source <- csvSource
  generateLabelsResponse "location.csv" "config/locations-qr.glabels" source
  

-- * Render
-- | Generates a csv 
csvSource :: Monad m => Handler (ConduitM () (Text) m ())
csvSource = do
  locations <- appStockLocationsInverse . appSettings <$> getYesod
  return $ do
    yieldMany [ loc <> "," <> sub <> "\n"
              |  (loc, sub) <- ("Location", "Sublocation") : Map.toList locations
              ]
  
