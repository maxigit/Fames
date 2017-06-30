{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- | Miscellaneous functions to help rendering
-- | and/or accessing the database
module Handler.Util
( entitiesToTable
, getDBName
, getHaskellName
, entityTableHandler
, uploadFileForm
, uploadFileFormWithComment
, Encoding(..)
, readUploadUTF8
, computeDocumentKey
, setAttachment
, generateLabelsResponse
, firstOperator
, badgeSpan
) where

import Foundation
import Import.NoFoundation
import Data.Conduit.List (consume)
import Data.Text.Encoding(decodeLatin1)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              ) -- withSmallInput)
import qualified Crypto.Hash as Crypto
import qualified Data.Conduit.Binary as CB
-- import Data.Conduit.List (consume)
import System.Directory (removeFile)
import System.IO.Temp (openTempFile)
-- import System.Exit (ExitCode(..))
import Data.Streaming.Process (streamingProcess, proc, Inherited(..), waitForStreamingProcess, env)
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html (Markup)
-- import Data.IOData (IOData)

-- * Display entities
-- | Display Persist entities as paginated table
-- the filter is mainly there as a proxy to indicate
-- the entity type to display
entityTableHandler :: (PersistEntityBackend a ~ BaseBackend (YesodPersistBackend site), Yesod site, YesodPersist site, PersistQueryRead (YesodPersistBackend site), PersistEntity a)
  => Route site -> [Filter a] -> HandlerT site IO Html
entityTableHandler route filter_ = do
  let page_ = "page"
      pageSize_ = "pageSize_"
  pageSizeM <- lookupGetParam pageSize_
  pageM <- lookupGetParam page_
  count_ <- runDB $ count filter_
  let pageSize = fromMaybe 200 (readMay =<< unpack <$> pageSizeM)
      lastPage = (count_ + pageSize - 1) `div` pageSize
      page = fromMaybe 1 (readMay =<< unpack <$> pageM)
      offset = pageSize * (page-1)
      previous = page -1
      next = page +1
      one = 1 :: Int
  entities <- runDB $ selectList filter_ [LimitTo pageSize, OffsetBy offset]
  -- let typed = entities :: [Entity FA.ItemRequest]
  let navBar = [whamlet|
<nav.navbar.navbar-default>
  <ul.nav.navbar-nav>
    $if page > 1
      <li>
          <a href="@?{(route, [(page_,tshow one), (pageSize_,tshow pageSize)])}">First
      <li>
        <a href="@?{(route, [(page_,tshow previous), (pageSize_,tshow pageSize)])}">Previous
    $if page < lastPage
      <li>
        <a href="@?{(route, [(page_,tshow next), (pageSize_,tshow pageSize)])}">Next
      <li>
          <a href="@?{(route, [(page_,tshow lastPage), (pageSize_,tshow pageSize)])}">Last
|]
  defaultLayout $ do
     navBar
     toWidget (entitiesToTable getDBName entities )
     navBar

entitiesToTable :: PersistEntity a => (FieldDef -> Text) -> [Entity a] -> Html
entitiesToTable getColumn entities = do
  let eDef = entityDef (map entityVal entities)
  [shamlet|
<table.table.table-bordered.table-striped class="#{unHaskellName $ entityHaskell eDef}">
  <tr>
    <th> Id
    $forall field <- entityFields eDef
      <th> #{getColumn field}
  $forall Entity eit entity  <- entities
    <tr>
      <td.id> #{renderPersistValue $ toPersistValue eit}
      $forall (pfield, fieldDef) <- zip (toPersistFields entity) (entityFields eDef)
        <td class="#{getHaskellName fieldDef}" > #{renderPersistValue $ toPersistValue pfield}
|]

getDBName :: FieldDef -> Text
getDBName = unDBName . fieldDB

getHaskellName :: FieldDef -> Text
getHaskellName = unHaskellName . fieldHaskell


renderPersistValue :: PersistValue -> Text
renderPersistValue (PersistList _) = "<>"
renderPersistValue pvalue = case (fromPersistValueText pvalue) of
  Left _ -> tshow pvalue
  Right text -> text


-- * Forms
-- | Encoding of the file being uploaded.
data Encoding = UTF8 | Latin1 deriving (Show, Read, Eq, Enum, Bounded)

uploadFileForm :: _ a
                        -> Markup
                        -> _
                             (Maybe (Env, FileEnv), App, [Lang])
                             Enctype
                             Ints
                             Handler
                             (FormResult (FileInfo, Encoding, a), Widget) 
uploadFileForm fields = renderBootstrap3 BootstrapBasicForm
  ((,,)
   <$> areq fileField "upload" Nothing
   <*> areq (selectField optionsEnum ) "encoding" (Just UTF8)
   <*> fields
  )

uploadFileFormWithComment :: Markup
                          -> _
                          (Maybe (Env, FileEnv), App, [Lang])
                          Enctype
                          Ints
                          Handler
                          (FormResult (FileInfo, Encoding, Maybe Textarea), Widget)
uploadFileFormWithComment = uploadFileForm (aopt textareaField "comment" Nothing)
computeDocumentKey :: ByteString -> Text
computeDocumentKey bs = let
  digest = Crypto.hash bs :: Crypto.Digest Crypto.SHA256
  in tshow digest

-- | Retrieve the content of an uploaded file.
readUploadUTF8 :: MonadResource m => FileInfo -> Encoding -> m (ByteString, Text)
readUploadUTF8  fileInfo encoding = do
  c <- fileSource fileInfo $$ consume
  let bs = decode encoding (concat c)

  return $ (bs, computeDocumentKey bs)

decode :: Encoding -> ByteString -> ByteString
decode UTF8 bs = bs
decode Latin1 bs = encodeUtf8 . decodeLatin1 $ bs


setAttachment :: MonadHandler m => LT.Text -> m ()
setAttachment path = 
  addHeader "Content-Disposition" (toStrict ("attachment; filename=\"barcodes-"<>path<>"\"") )


-- * Labels
generateLabelsResponse ::
  Text
  -> Text
  -> Conduit () (HandlerT site IO) Text
  -> HandlerT site IO TypedContent
generateLabelsResponse outputName template labelSource = do
  -- let types = (outputName, template) :: (Text, Text)
  (tmp, thandle) <- liftIO $ openTempFile "/tmp" (unpack outputName)
  (pin, Inherited, perr, phandle ) <- streamingProcess (proc "glabels-3-batch"
                                                        ["--input=-"
                                                        , "--output"
                                                        , tmp
                                                        , (unpack template)
                                                        ]
                                                  ) {env = Just [("LANG", "C.UTF-8")]}
  runConduit $ labelSource =$= encodeUtf8C =$= sinkHandle pin
  _exitCode <- waitForStreamingProcess phandle
  -- we would like to check the exitCode, unfortunately
  -- glabels doesn't set the exit code.
  -- we need to stderr instead 
  errorMessage <- sourceToList  $ sourceHandle perr 
  let cleanUp = liftIO $  do
        hClose perr

        removeFile tmp
        hClose thandle

  case errorMessage of
    [] ->  do
      setAttachment (fromStrict outputName)
      respondSource "application/pdf"
                    (const cleanUp `addCleanup` CB.sourceHandle thandle =$= mapC (toFlushBuilder))

    _ -> do
        cleanUp
        sendResponseStatus (toEnum 422) (mconcat (map decodeUtf8 errorMessage :: [Text]))

-- * Operator
-- | Returns the first active operator.
-- This is the default operator which will be used in batch mode if  required.
--- We should have at least one operator, so we don't need the Maybe
firstOperator :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, YesodPersist site, PersistQueryRead (YesodPersistBackend site)) => HandlerT site IO (Entity Operator)
firstOperator = do
  operator <- runDB $ selectFirst [OperatorActive ==. True] [Asc OperatorId]
  maybe (error "No active operators found. Please contact your administrator") return operator


-- * Badges
badgeSpan :: (Num a,  Show a) => (a -> Maybe Int) -> a -> Maybe String -> String -> Html
badgeSpan badgeWidth qty bgM klass = do
  let style = case badgeWidth qty of
        Nothing -> "display:none"
        Just w ->  "width:" ++ show w  ++ "em"
      bg = case bgM of
             Nothing -> ""
             Just col ->  "background-color:"++col++";"
      qs = tshow qty
      q = fromMaybe qs $  stripSuffix ".0" qs
  [shamlet|<span.badge class=#{klass} style="#{style}; #{bg}">#{q}|]
