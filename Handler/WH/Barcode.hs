{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Barcode where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
-- toto
  -- add number in CSV and type
  -- refactor , clean
  -- stop if too many number

import Formatting
import Formatting.Time
import Data.Text.Lazy.Builder (fromLazyText)
import Data.Char (ord, chr)
import qualified Data.Text.Lazy as LT
 
-- | Allowed prefixes. Read from configuration file.
barcodeTypes :: Handler [(Text, Text)]
barcodeTypes = return $ [("ST", "Stock take")]
  
barcodeForm :: [(Text, Text)] -> Maybe Day -> Maybe Int ->  Form (Text, Maybe Int, Int,  Day)
barcodeForm prefixes date start = renderBootstrap3 BootstrapBasicForm $ (,,,)
  <$> areq (selectFieldList [(p <> " - " <> d, p) | (p,d) <- prefixes ]) "Prefix" Nothing
  <*> aopt intField "Start" (Just start)
  <*> areq intField "Number" (Just 42)
  -- <*> areq dayField (FieldSettings "Date" Nothing Nothing Nothing [("disabled", "disabled")]) date
  <*> areq dayField (FieldSettings "Date" Nothing Nothing Nothing [("readonly", "readonly")]) date

getWHBarcodeR :: Handler Html
getWHBarcodeR = do
  now <- utctDay <$> liftIO getCurrentTime
  start <- lookupGetParam "Start"
  renderGetWHBarcodeR now (readMay =<< start)


renderGetWHBarcodeR :: Day -> Maybe Int -> Handler Html
renderGetWHBarcodeR date  start = do
  prefixes <- barcodeTypes
  (form, encType) <- generateFormPost (barcodeForm prefixes (Just date) start)
  table <- entityTableHandler (WarehouseR WHBarcodeR) ([] :: [Filter BarcodeSeed])
  defaultLayout $ [whamlet|
<h1> Barcode Generator
  <form #barcode-form role=form method=post action=@{WarehouseR WHBarcodeR} enctype=#{encType}>
    ^{form}
    <button type="submit" .btn .btn-default>Download
    <div>
      ^{table}
|]

postWHBarcodeR :: Handler TypedContent 
postWHBarcodeR = do
  prefixes <- barcodeTypes
  ((resp, textW), encType) <- runFormPost $ barcodeForm prefixes Nothing Nothing
  case resp of
    FormMissing -> error "missing"
    FormFailure msg ->  error "Form Failure"
    FormSuccess (barcodeType, startM, number, date) -> do
      addHeader "Content-Disposition" "attachment"
      addHeader "Filename" "barcodes.csv"

      let prefix = format
                   ((fitLeft 2 %. stext) % (yy `mappend` later month2) ) --  (yy <> later month2))
                   barcodeType
                   (date)
          prefix' = toStrict prefix
      
      -- find last available number
      seE <- runDB $ do
        seedM <- getBy (UniqueBCSeed prefix')
        case seedM of
          Nothing -> do
            let start = fromMaybe 1 startM
                end = start + number -1
            insert $ BarcodeSeed prefix' end
            return $ Right (start, end)
          (Just (Entity sId (BarcodeSeed _ lastUsed))) -> do
            -- check if start set by user is valid
            let range s = (s, s +number-1)
            case startM of
                 Nothing -> return $ Right (range $ lastUsed +1)
                 Just start' | lastUsed > start' -> return $ Left lastUsed
                 Just start' -> do
                    let (start, end) = range (lastUsed + 1)
                    update sId [BarcodeSeedLastUsed =. end]
                    return $ Right (start, end)
      case seE of
          Left lastUsed -> do
                setMessage $ "Start parameter too low"
                -- toTypedContent <$> renderGetWHBarcodeR date (Just $ lastUsed+1)
                redirect (WarehouseR WHBarcodeR, [("Prefix", prefix'), ("Start", tshow lastUsed), ("Number", tshow number), ("Date", toStrict $ format dateDash date)])
          Right (start, end) -> do
                let bareBarcodes = [format (stext % (left 4 '0'))  prefix' n | n <- [start..end]]
                    barcodes = map ((<>) <*>  checksum) bareBarcodes

                respondSource typePlain $ do
                    sendChunkText $ "Barcode,Date\n"
                    forM_ barcodes (\b -> sendChunkText . toStrict $ format (text % "," % dateDash % "\n")  b date )

                       

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
checksum :: LT.Text -> LT.Text
checksum s = singleton . chr $ sum (map ((*23) . ord) (toList s)) `mod` 26 + ord 'A'

