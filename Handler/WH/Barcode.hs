{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Barcode where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
-- TODO
  -- TODO clean code (Conduit)
  -- TODO clean import (conduit, cabal)
  -- DONE manage error in calling gllabels
  -- TODO fix template retrieving from form
  --  DONE load from config
  -- DONE add many template
     -- TODO with description (in config)
  -- DONE use glabels
  -- refactor , clean
     -- field creation DONE
     -- angular -> fay ?
     -- remove [Template] param, just return a Barcode with a list
     -- use angular Yesod ?
  -- split into function DONE
  -- bracket tmp file DONE

import Formatting
import Formatting.Time
import WH.Barcode
import Data.List((!!))
import Data.Streaming.Process (streamingProcess, proc, ClosedStream(..), waitForStreamingProcess)
import System.IO.Temp (openTempFile)
import System.Exit (ExitCode(..))
import qualified Data.Conduit.Binary as CB
import System.Directory (removeFile)
 
barcodeFayWidget = return () --  $(fayFile "WH/Barcode")

data ReqOpt = Optional | Required deriving (Eq, Read, Show)
renderField ::
  (MonadIO m, MonadBaseControl IO m, MonadThrow m)
  => Text-> FieldView site -> ReqOpt -> WidgetT site m ()
renderField label view fieldType = let
  class_ = case fieldType of
    Optional -> "optional" :: Text
    Required -> "required"
  in [whamlet|
<div .form-group#{class_}>
  <label for=#{fvLabel view}> #{label}
  ^{fvInput view}
|]
withAngularApp :: Maybe Text -> Widget -> Widget
withAngularApp modM widget = do
  case modM of
     Nothing -> [whamlet|<div ng-app>^{widget}|]
     Just mod -> [whamlet|<div ng-app=#{mod}>^{widget}|]
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
  <select ##{fvId prefixView} name=#{prefixName} ng-model="prefix">
    $forall (bparam, bi) <- zip bparams ns
      <option value=#{bi}> #{bpPrefix bparam}-#{bpDescription bparam}

<div ##{fvId templateView} .form-groupoptional>
  <label for=#{fvLabel templateView }> Template
    $forall ((bi, template), i) <- zip allTemplates ns
      <div ng-show="prefix==#{bi}">
         <input name="#{templateName}" type="radio" value="#{i}">
            #{btPath template} - #{btNbPerPage template}

^{renderField "Start" startView Optional}
^{renderField "Number" numberView Required}
<div .form-grouprequired<div .form-grouprequired>
  <label for=#{fvLabel dateView }> Date
  <input ##{fvId dateView} readonly="readonly" name="#{dateName}" type="date" value="#{maybe "" tshow date}">
^{renderField "Only Data" onlyView Required}
|]

      result = (,,,,,) <$> prefixRes
                       <*> templateRes
                       <*> startRes
                       <*> numberRes
                       <*> dateRes
                       <*> ((\o -> if o then Csv else GLabels) <$> onlyRes)
  return (result, widget >> barcodeFayWidget)
  where allTemplates = [(bi, template)
                       | (bparam,bi) <- zip bparams ns
                       , template <- bpTemplates bparam
                       ]
        ns = [1..] :: [Int]

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
  bparams <- getBarcodeParams
  ((resp, textW), encType) <- runFormPost $ barcodeForm bparams Nothing Nothing
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
                    bareBarcodes = map (formatBarcode prefix) numbers
                    barcodes = zip (bareBarcodes) numbers

                let barcodeSource =  do
                            yield "Barcode,Number,Date\n"
                            yieldMany [ format (text % "," % int %","% dateDash % "\n")  b n date
                                      | (b,n) <- barcodes
                                      ]
                case (outputMode, template) of
                  (GLabels, Just template') -> do
                            generateLabels barcodeSource template' (outputFile prefix start end "pdf")

                  (Csv, _) -> do
                        setAttachment (outputFile prefix start end "csv")
                        respondSource "text/csv" $ do
                            barcodeSource =$= mapC (toFlushBuilder . toStrict)

generateLabels barcodeSource template attachmentPath = do
  (tmp, thandle) <- liftIO $ openTempFile "/tmp" "barcode.pdf" 
  (pin, pout, perr, phandle ) <- streamingProcess (proc "glabels-3-batch"
                                                        ["--input=-"
                                                        , "--output"
                                                        , tmp
                                                        , unpack $ btPath template
                                                        ]
                                                  )
  runConduit $ barcodeSource =$= sinkHandle pin
  exitCode <- waitForStreamingProcess phandle
  -- we would like to check the exitCode, unfortunately
  -- glabels doesn't set the exit code.
  -- we need to stderr instead 
  errorMessage <- sourceToList  $ sourceHandle perr 
  let cleanUp = liftIO $  do
      hClose pout
      hClose perr

      removeFile tmp
      hClose thandle

  case errorMessage of
    [] ->  do
      setAttachment attachmentPath
      respondSource "application/pdf"
                    (const cleanUp `addCleanup` CB.sourceHandle thandle =$= mapC (toFlushBuilder))

    _ -> do
        runConduit $ sourceHandle pout =$= mapC (\t -> t :: Text) =$= sinkHandle stdout
        cleanUp
        sendResponseStatus (toEnum 422) (mconcat (errorMessage :: [Text]))

-- outputFile :: Text -> Int -> Int -> Text -> Text
outputFile prefix start end ext =
  format ("barcodes-"%text%"-"%int%"-"%int%"."%text) prefix start end ext


setAttachment path = do
  addHeader "Content-Disposition" (toStrict $ format ("attachment; filename=\"barcodes-"%text%"\"") path)
