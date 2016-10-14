{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Barcode where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
-- toto
  --  load from config
	-- add many template
         -- with description (in config)
	-- use glabels
  -- refactor , clean

import Formatting
import Formatting.Time
import WH.Barcode
import Data.List((!!))
 
barcodeForm :: [BarcodeParams]
            -> Maybe Day
            -> Maybe Int
            ->  Form (BarcodeParams, Maybe Int, Maybe Int, Int,  Day, OutputMode)
barcodeForm bparams date start = renderBootstrap3 BootstrapBasicForm $ (,,,,,)
  <$> areq (selectFieldList [(bpPrefix p <> " - " <> bpDescription p, p) | p <- bparams]) "Prefix" Nothing
  <*> aopt intField "Template" Nothing
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

getBarcodeParams :: Handler [BarcodeParams]
getBarcodeParams = appBarcodeParams <$> appSettings <$> getYesod

renderGetWHBarcodeR :: Day -> Maybe Int -> Handler Html
renderGetWHBarcodeR date  start = do
  barcodeParams <- getBarcodeParams
  (form, encType) <- generateFormPost (barcodeForm barcodeParams (Just date) start)
  table <- entityTableHandler (WarehouseR WHBarcodeR) ([] :: [Filter BarcodeSeed])
  defaultLayout $ [whamlet|
<h1> Barcode Generator
  #{tshow barcodeParams}
  <form #barcode-form role=form method=post action=@{WarehouseR WHBarcodeR} enctype=#{encType}>
    ^{form}
    <button type="submit" .btn .btn-default>Download
    <div>
      ^{table}
|]

postWHBarcodeR :: Handler TypedContent 
postWHBarcodeR = do
  bparams <- getBarcodeParams
  ((resp, textW), encType) <- runFormPost $ barcodeForm bparams Nothing Nothing
  case resp of
    FormMissing -> error "missing"
    FormFailure msg ->  error "Form Failure"
    FormSuccess (bparam, templateNb, startM, number, date, outputMode) -> do

      let prefix = format
                   ((fitLeft 2 %. stext) % (yy `mappend` later month2) ) --  (yy <> later month2))
                   (bpPrefix bparam) -- barcodeType
                   (date)
          prefix' = toStrict prefix
          template = do
             n <- templateNb
             if n < length (bpTemplates bparam)
                then return $ (bpTemplates bparam) !! n
                else Nothing
          
          endFor start =
            let number' = case outputMode of
                          Csv -> start + number -1
                          GLabels -> -- fill a page
                                  let perPage = fromMaybe 1 (btNbPerPage <$> template)
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

                addHeader "Content-Disposition"
                          (toStrict $ format ("attachment; filename=\"barcodes-"%text%"-"%int%"-"%int%".csv\"")
                                             prefix
                                             start
                                             end
                          )
                respondSource typePlain $ do
                    sendChunkText $ "Barcode,Number,Date\n"
                    forM_ barcodes (\(b,n) -> sendChunkText
                                           . toStrict
                                           $ format (text % "," % int %","% dateDash % "\n")  b n date )

                       




