{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Barcode where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
-- toto
  -- select prefix
  -- inactive date
  -- make checksum work
  -- use conduit ?
  -- proper csv (with colnames and more columns) 
  -- refactor , clean

import Formatting
import Formatting.Time
import Data.Text.Lazy.Builder (fromLazyText)
-- | Allowed prefixes. Read from configuration file.
barcodeTypes :: Handler [(Text, Text)]
barcodeTypes = return $ [("ST", "Stock take")]
  
barcodeForm :: [(Text, Text)] -> Maybe Day -> Form (Text, Int, Day)
barcodeForm prefixes date = renderBootstrap3 BootstrapBasicForm $ (,,)
  <$> areq textField "Prefix" Nothing
  <*> areq intField "Number" Nothing
  <*> areq dayField "Date" date

getWHBarcodeR :: Handler Html
getWHBarcodeR = do
  prefixes <- barcodeTypes
  now <- utctDay <$> liftIO getCurrentTime
  (form, encType) <- generateFormPost (barcodeForm prefixes (Just now))
  defaultLayout [whamlet|
<h1> Barcode Generator
  <form #barcode-form role=form method=post action=@{WarehouseR WHBarcodeR} enctype=#{encType}>
    ^{form}
    <button type="submit" .btn .btn-default>Download
|]



postWHBarcodeR :: Handler Html 
postWHBarcodeR = do
  prefixes <- barcodeTypes
  ((resp, textW), encType) <- runFormPost $ barcodeForm prefixes Nothing
  case resp of
    FormMissing -> error "missing"
    FormSuccess (barcodeType, number, date) -> do
      addHeader "Content-Disposition" "attachment"
      addHeader "Filename" "barcodes.csv"

      let prefix = format
                   ((fitLeft 2 %. stext) % (yy `mappend` later month2) ) --  (yy <> later month2))
                   barcodeType
                   (date)
          prefix' = toStrict prefix
      
      -- find last available number
      (start, end) <- runDB $ do
        seedM <- getBy (UniqueBCSeed prefix')
        case seedM of
          Nothing -> do
            let start = 1
                end = start + number -1
            insert $ BarcodeSeed prefix' end
            return (start, end)
          (Just (Entity sId (BarcodeSeed _ start))) -> do
            let end = start + number -1
            update sId [BarcodeSeedLastUsed =. end]
            return (start, end)
      let bareBarcodes = [format (stext % (left 4 '0'))  prefix' n | n <- [start..end]]
          barcodes = map ((<>) <*>  checksum) bareBarcodes
      defaultLayout [whamlet|
<ul>
  $forall (barcode) <- barcodes
    <li>
        #{barcode}
|]
                       

-- | Month abbreviation on letter
-- month2 :: Day -> Text
month2 day = let (_, month, _) = toGregorian day
  in fromLazyText $ go month  where
  go 1 = "JA"
  go 2 = "FE"
  go 3 = "MR"
  go 4 = "AP"
  go 5 = "MY"
  go 6 = "JU"
  go 7 = "JL"
  go 8 = "AU"
  go 9 = "SE"
  go 10 = "OC"
  go 11 = "NV"
  go 12 = "DE"



-- | checksum
-- checksum :: Text -> Text
checksum s = "TODO"
