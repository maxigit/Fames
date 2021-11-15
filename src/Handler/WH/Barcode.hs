{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
module Handler.WH.Barcode where

import Import
import Formatting hiding(now)
import Formatting.Time
import WH.Barcode
import qualified Data.Text.Lazy as L
import Handler.Util(setAttachment, generateLabelsResponse, renderField)
-- import Data.Streaming.Process (streamingProcess, proc, ClosedStream(..), waitForStreamingProcess)
-- import System.IO.Temp (openTempFile)
-- import System.Exit (ExitCode(..))
-- import qualified Data.Conduit.Binary as CB
-- import System.Directory (removeFile)
import Data.Time
 
withAngularApp :: Maybe Text -> Widget -> Widget
withAngularApp modM widget = do
  case modM of
     Nothing -> [whamlet|<div ng-app>^{widget}|]
     Just module_ -> [whamlet|<div ng-app=#{module_}>^{widget}|]
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min.js"

barcodeForm :: [BarcodeParams]
            -> Maybe Day
            -> Maybe Int
            -> Html
            ->  MForm Handler ( FormResult (BarcodeParams, Maybe BarcodeTemplate, Maybe Int, Int,  Day, OutputMode)
                              , Widget
                              )

-- | Displays a Form with a dropdown menu for the prefix
--  and a list of availabl /e templates for the prefix
-- we use JS to modify the the list of templates to display
barcodeForm bparams date start extra = do
  let prefixName = "fprefix" :: Text
      templateName = "ftemplate" :: Text
      dateName = "fdate" :: Text
  (prefixRes, prefixView) <- mreq (selectFieldList [(bpPrefix p <> " - " <> bpDescription p, p) | p <- bparams])
           ("Prefix" {fsName = Just prefixName})
           Nothing
  (templateRes, templateView) <- mopt (radioFieldList [(btPath t, t) | (_, t) <- allTemplates])
           ("Template" {fsName = Just templateName})
           Nothing
  (startRes, startView) <- mopt intField "Start" (Just start)
  (numberRes, numberView) <- mreq intField "Number" (Just 42)
  (dateRes, dateView) <- mreq dayField ("Date" {fsName = Just dateName, fsAttrs = [("readonly", "readonly")]}) date
  (onlyRes, onlyView) <- mreq checkBoxField "Only data" (Just False)
  let widget = do
        withAngularApp Nothing [whamlet|
#{extra}
<div .form-grouprequired>
  <label for=#{fvLabel prefixView }> Type
  <select ##{fvId prefixView} name=#{prefixName} ng-model="prefix"
      onchange="$('input.template-'+$('##{fvId prefixView}').val())[0].checked=true;"
  >
    $forall (bparam, bi) <- zip bparams ns
      <option value=#{bi}
      > #{bpPrefix bparam}-#{bpDescription bparam}

<div ##{fvId templateView} .form-groupoptional>
  <label for=#{fvLabel templateView }> Template
    $forall ((bi, template), i) <- zip allTemplates ns
      <div ng-show="prefix==#{bi}">
         <input class="template template-#{bi}" name="#{templateName}" type="radio" value="#{i}">
            #{btPath template} - #{btNbPerPage template}

^{renderField startView}
^{renderField numberView}
<div .form-grouprequired<div .form-grouprequired>
  <label for=#{fvLabel dateView }> Date
  <input ##{fvId dateView} readonly="readonly" name="#{dateName}" type="date" value="#{maybe "" tshow date}">
^{renderField onlyView}
|]

      result = (,,,,,) <$> prefixRes
                       <*> templateRes
                       <*> startRes
                       <*> numberRes
                       <*> dateRes
                       <*> ((\o -> if o then Csv else GLabels) <$> onlyRes)
  return (result, widget)
  where allTemplates = [(bi, template)
                       | (bparam,bi) <- zip bparams ns
                       , template <- bpTemplates bparam
                       ]
        ns = [1..] :: [Int]

{-# NOINLINE getWHBarcodeR #-}
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
  ((_,form), encType) <- runFormPost (barcodeForm barcodeParams (Just date) start)
  entities <- runDB $ selectList ([] :: [Filter BarcodeSeed]) []
  let table = entitiesToTable getDBName entities
  defaultLayout $ [whamlet|
<h1> Barcode Generator
  <form #barcode-form role=form method=POST action=@{WarehouseR WHBarcodeR} enctype=#{encType}>
    ^{form}
    <button type="submit" .btn .btn-default>Download
    <div>
      ^{table}
|]

{-# NOINLINE postWHBarcodeR #-}
postWHBarcodeR :: Handler TypedContent 
postWHBarcodeR = do
  bparams <- getBarcodeParams
  ((resp, __textW), __encType) <- runFormPost $ barcodeForm bparams Nothing Nothing
  case resp of
    FormMissing -> error "missing"
    FormFailure msg ->  error $ "Form Failure:" ++ show msg
    FormSuccess (bparam, template, startM, number, date, outputMode) -> do

      let prefix = format
                   ((fitLeft 2 %. stext) % (yy `mappend` later month2) ) --  (yy <> later month2))
                   (bpPrefix bparam) -- barcodeType
                   (date)
          prefix' = toStrict prefix
          
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
        -- liftIO $ print (prefix', seedM)
        case seedM of
          Nothing -> do
            let start = fromMaybe 1 startM
                end = endFor start
            _ <- insert $ BarcodeSeed prefix' end
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
                    bareBarcodes = map (formatBarcode prefix) numbers
                    barcodes = zip (bareBarcodes) numbers

                let barcodeSource =  do
                            yield "Barcode,Number,Date\n"
                            yieldMany [ toStrict $ format (text % "," % int %","% dateDash % "\n")  b n date
                                      | (b,n) <- barcodes
                                      ]
                case (outputMode, template) of
                  (GLabels, Just template') -> do
                            generateLabelsResponse  (toStrict $ outputFile prefix start end "pdf")
                                                    (btPath template')
                                                    barcodeSource

                  (Csv, _) -> do
                        setAttachment (outputFile prefix start end "csv")
                        respondSource "text/csv" $ do
                            barcodeSource .| mapC toFlushBuilder
                  (GLabels, Nothing) -> error "SHOULD NOT HAPPEN!. Call to glabels without template." 


outputFile :: L.Text -> Int -> Int -> L.Text -> L.Text
outputFile prefix start end ext =
  format ("barcodes-"%text%"-"%int%"-"%int%"."%text) prefix start end ext


-- * Utils

-- | generates a sequence of barcodes given a prefix
-- roll to the next prefix if needed
generateBarcodes :: Text -> (Maybe Day) -> Int -> SqlHandler [Text]
generateBarcodes _ _ 0 = return []
generateBarcodes code dayM nb = do
  today <- todayH
  let date = fromMaybe today dayM
      prefix = format ((fitLeft 2 %. stext) % (yy `mappend` later month2))
                      code
                      date
      prefix' = toStrict prefix
      endFor start = start + nb -1
  
  -- find last available number
  seedM <- getBy (UniqueBCSeed prefix')
  (start, end) <- case seedM of
    Nothing -> do
      let start = 1
          end = endFor start

      _ <- insert $ BarcodeSeed prefix' end
      return (start,end)

    Just (Entity sId (BarcodeSeed _ lastUsed)) -> do
      let start = lastUsed + 1
          end = endFor start
      update sId [ BarcodeSeedLastUsed =. end ]
      return (start,end)

  let maxIndex = 1e5-1 :: Int
      (range, done) = if start > maxIndex
                 then  ([], 0)
                 else  let end' = min maxIndex end
                       in   ([start..end'], end'-start+1)

  -- in case we need to use the next date
  nexts <- generateBarcodes code (Just $ addGregorianMonthsClip 1 date) (nb -done)
      -- nexts = []

  -- is there any left
  return $   map (toStrict . formatBarcode prefix) range ++ nexts
      
  
 
