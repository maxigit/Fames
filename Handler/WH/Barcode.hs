{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Barcode where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
-- toto
  -- change modulo checksum
  -- refactor , clean
  --  load from config

import Formatting
import Formatting.Time
import Data.Text.Lazy.Builder (fromLazyText)
import Data.Char (ord, chr)
import Data.List (iterate)
import qualified Data.Text.Lazy as LT
 
-- | Allowed prefixes. Read from configuration file.
data BarcodeParams = BarcodeParams
  { bpPrefix :: Text
  , bpDescription :: Text
  , bpTemplate :: Text
  , bpNbPerPage :: Int
  } deriving (Eq, Read, Show)

data OutputMode = Csv | GLabels deriving (Eq, Ord, Read, Show)
barcodeTypes :: Handler [BarcodeParams]
barcodeTypes = return $ [BarcodeParams "ST" "Stock take" "stock_take.gl3" 21]
  
barcodeForm :: [BarcodeParams]
            -> Maybe Day
            -> Maybe Int
            ->  Form (BarcodeParams, Maybe Int, Int,  Day, OutputMode)
barcodeForm bparams date start = renderBootstrap3 BootstrapBasicForm $ (,,,,)
  <$> areq (selectFieldList [(bpPrefix p <> " - " <> bpDescription p, p) | p <- bparams]) "Prefix" Nothing
  <*> aopt intField "Start" (Just start)
  <*> areq intField "Number" (Just 42)
  -- <*> areq dayField (FieldSettings "Date" Nothing Nothing Nothing [("disabled", "disabled")]) date
  <*> areq dayField (FieldSettings "Date" Nothing Nothing Nothing [("readonly", "readonly")]) date
  <*> ((\o -> if o then Csv else GLabels) <$> areq checkBoxField "Only data" (Just False))

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
  bparams <- barcodeTypes
  ((resp, textW), encType) <- runFormPost $ barcodeForm bparams Nothing Nothing
  case resp of
    FormMissing -> error "missing"
    FormFailure msg ->  error "Form Failure"
    FormSuccess (bparam, startM, number, date, outputMode) -> do
      addHeader "Content-Disposition" "attachment"
      addHeader "Filename" "barcodes.csv"

      let prefix = format
                   ((fitLeft 2 %. stext) % (yy `mappend` later month2) ) --  (yy <> later month2))
                   (bpPrefix bparam) -- barcodeType
                   (date)
          prefix' = toStrict prefix
          endFor start =
            let number' = case outputMode of
                          Csv -> start + number -1
                          GLabels -> -- fill a page
                                  let perPage = bpNbPerPage bparam
                                      missing = (- number) `mod` perPage
                                  in start + number + missing -1
            in min 99999 number'
            
      
      -- find last available number
      seE <- runDB $ do
        seedM <- getBy (UniqueBCSeed prefix')
        liftIO $ print (prefix', seedM)
        case seedM of
          Nothing -> do
            let start = fromMaybe 1 startM
                end = endFor start
            insert $ BarcodeSeed prefix' end
            return $ Right (start, end)
          (Just (Entity sId (BarcodeSeed _ lastUsed))) -> do
            -- check if start set by user is valid
            let range s = do
                  let end = endFor s
                  update sId [BarcodeSeedLastUsed =. end]
                  return (s, end)
            case startM of
                 Nothing -> Right <$> range (lastUsed +1)
                 Just start' | lastUsed >= start' -> return $ Left lastUsed
                 Just start' -> Right <$> range start'
      case seE of
          Left lastUsed -> do
                setMessage $ "Start parameter too low"
                -- toTypedContent <$> renderGetWHBarcodeR date (Just $ lastUsed+1)
                redirect (WarehouseR WHBarcodeR, [("Prefix", prefix'), ("Start", tshow (lastUsed+1)), ("Number", tshow number), ("Date", toStrict $ format dateDash date)])
          Right (start, end) -> do
                let numbers = [start..end]
                    bareBarcodes = [format (stext % (left 5 '0'))  prefix' n | n <- numbers]
                    barcodes = zip (map ((<>) <*>  checksum) bareBarcodes) numbers

                respondSource typePlain $ do
                    sendChunkText $ "Barcode,Number,Date\n"
                    forM_ barcodes (\(b,n) -> sendChunkText
                                           . toStrict
                                           $ format (text % "," % int %","% dateDash % "\n")  b n date )

                       

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
checksum text = let
 c = sum $ zipWith (*) (map ord (reverse $ toList text)) (iterate (*10) 7)
 in singleton . chr $ c `mod` 26 + ord 'A'

