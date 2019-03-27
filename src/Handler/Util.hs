{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- * Overview
-- | Miscellaneous functions to help rendering
-- | and/or accessing the database
module Handler.Util
( entitiesToTable
, getDBName
, getHaskellName
, renderPersistValue
, entityTableHandler , entityTableHandler'
, uploadFileForm
, uploadFileFormInline
, uploadFileFormWithComment
, hiddenFileForm
, unsafeRunFormPost
, unsafeRunFormGet
, Encoding(..)
, readUploadUTF8
, setAttachment
, generateLabelsResponse
, firstOperator
, badgeSpan
, blueBadgeBg , grayBadgeBg , greenBadgeBg , amberBadgeBg , redBadgeBg , blackBadgeBg
, paleRed, paleGreen, paleBlue, paleAmber
, tshowM
, showDouble
, panel
, infoPanel
, dangerPanel
, primaryPanel
, splitSnake
, basePriceList
, timeProgress
, allOperators
, operatorFinder
, operatorFinderWithError
, FilterExpression(..)
, readFilterExpression
, filterE
, filterEField
, filterEKeyword
, filterEToSQL
, filterEAddWildcardRight
, (<-?.)
, readUploadOrCacheUTF8
, cacheText
, cacheByteString
, retrieveTextByKey
, locationSet
, toHtmlWithBreak
, hxtoHe
, ioeToIox
, heToHx
, ioxToHx
, ioeToHx
, eToHx
, ioToHx
, ioToH
, hToHx
, eToX
, categoriesH
, batchCategoriesH
, customerCategoriesH
, orderCategoriesH
, Identifiable(..)
, getIdentified
, renderField
, allCustomers
, allSuppliers
, todayH
, fiscalYearH
, currentFAUser
, getSubdirOptions
) where
-- ** Import
import Foundation
import Import.NoFoundation
import Data.Conduit.List (consume)
import Data.Text.Encoding(decodeLatin1)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              ) -- withSmallInput)
import qualified Data.Conduit.Binary as CB
-- import Data.Conduit.List (consume)
import System.Directory (removeFile, createDirectoryIfMissing)
import System.FilePath(takeDirectory)
import System.IO.Temp (openTempFile)
-- import System.Exit (ExitCode(..))
import Data.Streaming.Process (streamingProcess, proc, Inherited(..), waitForStreamingProcess, env)
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html (Markup)
import FA as FA hiding (unUserKey)
import Data.Time (diffDays, addGregorianMonthsClip)
import System.Directory (doesFileExist)
import qualified Data.Map.Strict as Map
import qualified Data.List as Data.List
import Model.DocumentKey
import Control.Monad.Except hiding(mapM_, filterM)
import Text.Printf(printf) 
import Data.Maybe(fromJust)
import Text.Read(Read,readPrec)
-- import Data.IOData (IOData)
import Database.Persist.MySQL(unSqlBackendKey)
import System.Directory(listDirectory, doesDirectoryExist)
import Data.Char(isUpper)
import qualified Data.List.Split as Split 

-- * Display entities
-- | Display Persist entities as paginated table
-- the filter is mainly there as a proxy to indicate
-- the entity type to display
entityTableHandler :: (PersistEntityBackend a ~ BaseBackend (YesodPersistBackend site), Yesod site, YesodPersist site, PersistQueryRead (YesodPersistBackend site), PersistEntity a)
  => Route site -> [Filter a] -> HandlerT site IO Html
entityTableHandler route filter_ = entityTableHandler' route filter_ []
entityTableHandler' :: (PersistEntityBackend a ~ BaseBackend (YesodPersistBackend site), Yesod site, YesodPersist site, PersistQueryRead (YesodPersistBackend site), PersistEntity a)
  => Route site -> [Filter a] -> [SelectOpt a] -> HandlerT site IO Html
entityTableHandler' route filter_ orders = do
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
  entities <- runDB $ selectList filter_ (orders ++ [LimitTo pageSize, OffsetBy offset])
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
  Left _ -> tshow $ toJSON pvalue
  Right text -> text


-- * Forms
uploadFileForm :: _ a
                        -> Markup
                        -> _
                             (Maybe (Env, FileEnv), App, [Lang])
                             Enctype
                             Ints
                             Handler
                             (FormResult (FileInfo, Encoding, a), Widget) 
uploadFileForm = renderBootstrap3 BootstrapBasicForm . uploadFileForm'
uploadFileForm' fields = 
  ((,,)
   <$> areq fileField "file" Nothing
   <*> areq (selectField optionsEnum ) "encoding" (Just UTF8)
   <*> fields
  )

uploadFileFormInline = renderBootstrap3 BootstrapInlineForm . uploadFileForm'

uploadFileFormWithComment :: Markup
                          -> _
                          (Maybe (Env, FileEnv), App, [Lang])
                          Enctype
                          Ints
                          Handler
                          (FormResult (FileInfo, Encoding, Maybe Textarea), Widget)
uploadFileFormWithComment = uploadFileForm (aopt textareaField "comment" Nothing)

--  | Generate a form meant to be used to reload cache using readUploadOrCache
-- hiddenFileForm
--   :: DocumentHash
--      -> Maybe (DocumentHash, FilePath)
--      -> _ (FormResult (DocumentHash, FilePath), Widget)
hiddenFileForm key'pathM = renderBootstrap3 BootstrapBasicForm form where
  form =
    (,) <$> areq hiddenField  "key" (fmap fst  key'pathM)
        <*> areq hiddenField "path" (fmap snd key'pathM)
   

unsafeRunFormPost form = do
  ((resp, view), encType) <- runFormPost form
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess x -> do
      return (x, (view, encType))

unsafeRunFormGet form = do
  ((resp, view), encType) <- runFormGet form
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess x -> do
      return (x, (view, encType))

-- | Retrieve the content of an uploaded file.
readUploadUTF8 :: MonadResource m => FileInfo -> Encoding -> m (ByteString, DocumentHash)
readUploadUTF8  fileInfo encoding = do
  c <- fileSource fileInfo $$ consume
  let bs = decode encoding (concat c)

  return $ (bs, computeDocumentKey bs)

decode :: Encoding -> ByteString -> ByteString
decode UTF8 bs = bs
decode Latin1 bs = encodeUtf8 . decodeLatin1 $ bs


setAttachment :: MonadHandler m => LT.Text -> m ()
setAttachment path = 
  addHeader "Content-Disposition" (toStrict ("attachment; filename=\""<>path<>"\"") )


-- | Pair when only the first part of the t-uple is used for all the common instance
-- Ideal to be used in a form a select options when the desired object doesn't have
-- an EQ instance or label objects which doesn't have a show instance
newtype Identifiable a = Identifiable {unIdentifiable :: (Text, a)} deriving Functor
getIdentified :: Identifiable a -> a
getIdentified (Identifiable (_, a)) = a

instance Show (Identifiable a)  where
  show (Identifiable(i,_)) = unpack i
instance Eq (Identifiable a) where
  (Identifiable(i,_)) == (Identifiable(i',_)) = i == i'
instance Ord (Identifiable a) where
  (Identifiable(i,_)) `compare`  (Identifiable(i',_)) = i `compare` i'

instance Read (Identifiable ()) where
  readPrec = do
          name <- readPrec
          return $ Identifiable (name, ())

 
-- ** Read and reload file
-- | When validating a file, the file needs to be uploaded once for validating.
-- In order to be sure that we are processing the file which has been validated
-- (and because it's not possible to preset the upload parameter on the new form)
-- we save the first time, the file to a temporary file and use the key to reload it
-- on the second time. The key and path will need to be set to the form (as hidden parameter)
-- | readUploadOrCache.
-- Returns the content and the tempory key and path or nothing if nothing is present.
-- The resulting file is converted in UTF8 
readUploadOrCacheUTF8
  :: (MonadIO m, MonadResource m)
  => Encoding
  -> Maybe FileInfo -- file to upload
  -> Maybe DocumentHash -- Key if already uploaded
  -> Maybe Text -- Path if already uploaded
  -> m (Maybe (ByteString, DocumentHash, Text))
readUploadOrCacheUTF8  encoding fileInfoM keyM pathM = do
      let tmp (DocumentHash file) = "/tmp/DocumentCache" </> (unpack file)
      case (fileInfoM, keyM, pathM) of
        -- key and path set. Reuse the temporary file
        (_, Just (key), Just path) -> do
          ss <- readFile (tmp key)
          return $ Just (ss, key, path)
        (Just fileInfo, _, _) -> do
        -- File to upload. upload and save it If needed.
        -- If it already exist, the key guarantie that
        -- it is the same content.
          (ss, key) <- readUploadUTF8 fileInfo encoding
          let path = tmp key
          exist <- liftIO $ doesFileExist path
          unless exist $ do
            liftIO $ createDirectoryIfMissing True (takeDirectory path)
            writeFile path ss
          return $ Just (ss, key, fileName fileInfo)
        (_,_,_) -> return Nothing
  
-- | Saves and compute the cache key of a text
-- allows a textarea to be reread after submitting
cacheText :: (MonadIO m, MonadResource m) => Maybe (DocumentHash -> FilePath) -> Text -> m (DocumentHash, FilePath)
cacheText base = cacheByteString base . encodeUtf8
cacheByteString :: (MonadIO m, MonadResource m) => Maybe (DocumentHash -> FilePath) -> ByteString -> m (DocumentHash, FilePath)
cacheByteString base bs = do
  let key = computeDocumentKey bs
      path = fromMaybe (defaultPathMaker) base $ key
  exist <- liftIO $ doesFileExist path
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  writeFile path  bs

  return (key, path)

defaultPathMaker :: DocumentHash -> FilePath
defaultPathMaker (DocumentHash key) = "/tmp/DocumentCache" </> (unpack key)

retrieveTextByKey :: (MonadIO m, MonadResource m)
                  => Maybe (DocumentHash -> FilePath) -> DocumentHash -> m (Maybe Text)
retrieveTextByKey base key = do
  let path = fromMaybe defaultPathMaker  base $ key
  exist <- liftIO $ doesFileExist  path
  if exist
    then (Just . decodeUtf8) <$> readFile path
    else return Nothing


-- ** Form builder
renderField :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
            => FieldView site
            -> WidgetT site m ()
renderField view = let
  class_ = case fvRequired view of
    False -> "optional" :: Text
    True -> "required"
  in [whamlet|
<div .form-group class="#{class_}">
  <label for=#{fvId view}> #{fvLabel view}
  ^{fvInput view}
  $maybe err <- fvErrors view
    <div .errors>#{err}
|]

-- ** Directories
-- | Return a list of options corresponding to the sub directories
-- within the given one (on the server side)
-- The easiest is to use Dropbox or equivalent to synchronize those files
-- with the outside world
getSubdirOptions :: (AppSettings -> FilePath) -> Handler [(Text, FilePath)]
getSubdirOptions appDir = do
  mainDir <- appDir <$> getsYesod appSettings
  exists <- lift $ doesDirectoryExist mainDir
  if exists
    then do
          entries <- lift $ listDirectory mainDir
          dirs <- lift $ filterM (doesDirectoryExist . (mainDir </>)) entries
          -- traceShowM (forecastDir, entries, dirs)
          return [(pack dir, mainDir </> dir) | dir <- dirs]
    else do
      setWarning (toHtml $ "Planner master directory '" <> mainDir <> "' doesn't exist.")
      return []

-- * Labels
generateLabelsResponse ::
  Text
  -> Text
  -> Conduit () (HandlerT site IO) Text
  -> HandlerT site IO TypedContent
generateLabelsResponse outputName template labelSource = do
  -- let types = (outputName, template) :: (Text, Text)
  (tmp, thandle) <- liftIO $ openTempFile "/tmp/DocumentCache" (unpack outputName)
  (pin, Inherited, perr, phandle ) <- streamingProcess (proc "glabels-3-batch"
                                                        ["--input=-"
                                                        , "--output"
                                                        , tmp
                                                        , (unpack template)
                                                        ]
                                                  ) {env = Just [("LANG", "C.UTF-8")]}
  runConduit $ labelSource =$= encodeUtf8C =$= sinkHandle pin
  liftIO $ hClose pin
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
    _ ->  do
      setAttachment (fromStrict outputName)
      respondSource "application/pdf"
                    (const cleanUp `addCleanup` CB.sourceHandle thandle =$= mapC (toFlushBuilder))

    -- _ -> do
    --     cleanUp
    --     sendResponseStatus (toEnum 422) (mconcat (map decodeUtf8 errorMessage :: [Text]))

-- * Operator
-- | Returns the first active operator.
-- This is the default operator which will be used in batch mode if  required.
--- We should have at least one operator, so we don't need the Maybe
firstOperator :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, YesodPersist site, PersistQueryRead (YesodPersistBackend site)) => HandlerT site IO (Entity Operator)
firstOperator = do
  operator <- runDB $ selectFirst [OperatorActive ==. True] [Asc OperatorId]
  maybe (error "No active operators found. Please contact your administrator") return operator


-- * SQL
_ <-?. []  =  []
a <-?. list  =   [a <-. list]
-- ** Filtering Expressions (Like or Regexp)
-- | Generate a like or rlike statement
data FilterExpression = LikeFilter Text  | RegexFilter Text deriving (Eq, Show, Read)
showFilterExpression :: FilterExpression -> Text
showFilterExpression (LikeFilter t) = t
showFilterExpression (RegexFilter t) = "/" <> t


readFilterExpression :: Text -> FilterExpression
readFilterExpression t = case stripPrefix "/" t of
  Nothing -> LikeFilter t
  Just regex -> RegexFilter regex


instance IsString FilterExpression where
  fromString = readFilterExpression . fromString


-- | Creates a form field for a FilterExpression
filterEField :: (RenderMessage (HandlerSite m) FormMessage,
                  Monad m) =>
                Field m FilterExpression
filterEField = convertField readFilterExpression showFilterExpression textField


-- | Create a persistent filter from a maybe filter expression
filterE :: PersistField a =>
           (Text -> a) -- ^ how to convert the expression to the value of the field.
        -> EntityField record a -- ^ Persistent field
        -> Maybe FilterExpression
        -> [Filter record]
filterE _ _ Nothing = []
filterE conv field (Just (LikeFilter like)) = 
  [ Filter field
         (Left $ conv like)
         (BackendSpecificFilter "LIKE")
  ]
filterE conv field (Just (RegexFilter regex)) =
  [ Filter field
         (Left $ conv regex)
         (BackendSpecificFilter "RLIKE")
  ]
  
-- | SQL keyword.
filterEKeyword ::  FilterExpression -> (Text, Text)
filterEKeyword (LikeFilter f) = ("LIKE", f)
filterEKeyword (RegexFilter f) = ("RLIKE", f)
filterEToSQL :: FilterExpression -> Text
filterEToSQL exp = let (key, v) = filterEKeyword exp in key <> " '" <> v <> "''"

filterEAddWildcardRight :: FilterExpression -> FilterExpression
filterEAddWildcardRight (LikeFilter f) = LikeFilter (f<>"%")
filterEAddWildcardRight (RegexFilter f) = RegexFilter (f<>"*")
-- * Badges
badgeSpan :: (Num a,  Show a) => (a -> Maybe Int) -> a -> Maybe Text -> Text -> Html
badgeSpan badgeWidth qty bgM klass = do
  let style = case badgeWidth qty of
        Nothing -> "display:none"
        Just w ->  "width:" <> tshow w  <> "em"
      bg = case bgM of
             Nothing -> ""
             Just col ->  "background-color:"<>col<>";"
      qs = tshow qty
      q = fromMaybe qs $  stripSuffix ".0" qs
  [shamlet|<span.badge class=#{klass} style="#{style}; #{bg}">#{q}|]

-- ** BadgeColour
blueBadgeBg , grayBadgeBg , greenBadgeBg , amberBadgeBg , redBadgeBg , blackBadgeBg :: Text
blueBadgeBg = "#29abe0"
grayBadgeBg = "#cccccc"
greenBadgeBg = "#93c54b"
amberBadgeBg = "#f47c3c"
redBadgeBg = "#d9534f"
blackBadgeBg = "#000000"

paleRed, paleGreen, paleBlue, paleAmber :: Text
paleRed = "#f2dede"
paleGreen = "#dff0d8"
paleBlue = "#d0edf7"
paleAmber = "#fcf8e3"

-- * Progress bars
-- Display a time range within a global time range
-- (so that, all time range within the same page matches)
-- The done parameter specifys how to display date in the past (success or failure)
timeProgress :: Maybe Day -> Maybe Day -> Day -> Maybe Day -> Maybe Day -> Bool -> Widget
timeProgress minDateM maxDateM today startm endm done = do
  let minDate = minimumEx . (\x -> x :: [Day]) $ catMaybes [Just (addGregorianMonthsClip (-1) today), minDateM , startm]
      maxDate = maximumEx . (\x -> x :: [Day]) $ catMaybes [Just (addGregorianMonthsClip 1 today), maxDateM, endm]
      [start, end] = map (fromMaybe today) [startm, endm]
      maxWidth = max 1 (diffDays maxDate minDate) :: Integer

      bars = [ (col, 100 * fromIntegral w / fromIntegral maxWidth) | (col,w) <-
                case () of
                 _ | today < start -> [ ("none" :: Text, diffDays today minDate)
                                          , ("primary" , 1)
                                          , ("none", (diffDays start today) -1)
                                          , ("info", diffDays end start)
                                          ]
                 _ | today >= start && today <= end -> [ ("none", diffDays start minDate)
                                                               , ("success", diffDays today start)
                                                               , ("info", diffDays end today)
                                                               ]
                 -- end < tody
                 _ | done -> [ ("none", diffDays start minDate)
                                      , ("success", diffDays end start )
                                      , ("none", diffDays today end)
                                      , ("primary", 1)
                                      ] 
                 _ | True -> [ ("none", diffDays start minDate)
                             , ("danger", diffDays end start )
                             , ("none", diffDays today end)
                             , ("primary", 1)
                             ] 
             ]

  [whamlet|
<div.progress>
  $forall (style, width) <- bars
    <div.progress-bar class="progress-bar-#{style}"
                      role="progressbar"
                      style="min-width:0%; width:#{tshow width}%">&nbsp;<span class="sr-only"> #{tshow width}%
|]
  toWidget [cassius|
.progress-bar-none
  opacity:0
                   |]

-- * Html
tshowM :: Show a => Maybe a -> Text
tshowM = maybe "" tshow

toHtmlWithBreak :: Text -> Html
toHtmlWithBreak t  = [shamlet|
<span>
  $forall l <- lines t
    <p>#{l}
|]


showDouble :: Double -> Html
showDouble x = toHtml $ ( (printf "%.4f" x) :: String )
  
panel :: Text -> Text -> Widget -> Widget
panel panelClass title body = [whamlet|
  <div.panel class=#{panelClass}>
    <div.panel-heading>
      <h3>#{title}
    <div.panel-body>
      ^{body}
|]
  
infoPanel = panel "panel-info"
dangerPanel = panel "panel-danger"
primaryPanel = panel "panel-primary"
-- | split snake
splitSnake ::  Text -> Text
splitSnake t = pack $ intercalate " " $ Split.split  (Split.keepDelimsL $ Split.whenElt isUpper) (unpack t)
-- * Cached Value accross session
-- cacheEntities :: PersistEntity e => Text -> Bool -> Handler (Map (Key e) e )
cacheEntities cacheKey force = cache0 force cacheForEver cacheKey $ do
  entities <- runDB $ selectList [] []
  return $ mapFromList [(key, entity) | (Entity key entity) <- entities ]

-- ** From Front Accounting
-- Price list used as base to calculate other.
-- Found it FA system preferecense
basePriceList :: Handler Int
basePriceList = cache0 False cacheForEver "base-price-list" $ do
  [Entity _ prefs  ] <- runDB $ selectList [FA.SysPrefId ==. FA.SysPrefKey "base_sales"] []
  let Just basePl = readMay =<< FA.sysPrefValue prefs
  return basePl
  
-- *** Customer and Supplier map
allCustomers :: Bool -> Handler (Map (Key  FA.DebtorsMaster) FA.DebtorsMaster)
allCustomers force = cacheEntities "all-customer-list" force

allSuppliers :: Bool -> Handler (Map (Key  FA.Supplier) FA.Supplier)
allSuppliers force = cacheEntities "all-supplier-list" force

-- *** Current User
currentFAUser :: Handler (Maybe FA.User)
currentFAUser = do
  userM <- maybeAuthId
  fmap join $ forM userM $ \user ->  
    runDB $ get (FA.UserKey . fromIntegral . unSqlBackendKey . unUserKey $  user)

-- ** Fames
-- *** Operators
allOperators :: Handler (Map (Key Operator) Operator)
allOperators = cacheEntities "all-operators" False
  
-- | Find an operator by string, can be a mix of nickname, firstname, surname ...
operatorFinder :: Handler (Text -> Maybe (Entity Operator))
operatorFinder = cache0 False cacheForEver "operator-finder" $ do
      operators <- runDB $ selectList [] []
      let operatorKeys' = Map.fromListWith (++) $ concat
                   [ [ (toLower $ operatorNickname op, [e] ) 
                     , (toLower $ operatorFirstname op <> operatorSurname op, [e] )
                     -- , (toLower $ operatorFirstname op <> " " <> operatorSurname op, [e] )
                     , (toLower $ operatorFirstname op <> take 1 (operatorSurname op), [e] )
                     , (toLower $ operatorNickname op <> take 1 (operatorSurname op), [e] )
                     ]
                   | e@(Entity _ op) <- operators
                   ]
           -- we need to filter operators key with more than one solution
           -- but only if they are different solutions
          operatorKeys = fmap (Data.List.nub . sort) operatorKeys'
          pks = Map.map (Data.List.head)  (Map.filter (\ops -> Data.List.length ops ==1) operatorKeys)
          -- findOps = Map.lookup (Map.map (Data.List.head)  (Map.filter (\ops -> Data.List.length ops /=1) operatorKeys)) . toLower :: Text -> Entity Operator
      return $ (flip Map.lookup) pks  . toLower . (filter (/= ' '))

-- | Similar to operatorFinder but return an error instead of Nothing
operatorFinderWithError :: Handler (Text -> Either Text (Entity Operator))
operatorFinderWithError = do
  opFinder <- operatorFinder
  let go name = maybe (Left $ "Can't find operator with name '" <> name <> "'")
                      Right
                      (opFinder name)
  return go


-- *** Location
locationSet :: Handler (Set Text)
locationSet = cache0 False cacheForEver "location-set" $ do
    locations <- appStockLocationsInverse . appSettings <$> getYesod
    return . setFromList $ keys locations

  

-- * ExceptT 

type HandlerX = ExceptT Text Handler

hToHx :: Handler a -> HandlerX a
hToHx = lift

-- | as an excersise
ioToH :: IO a -> Handler a
ioToH = lift

ioToHx :: IO a -> HandlerX a
ioToHx = liftIO

eToHx :: Text -> HandlerX a
eToHx = throwError --  heToHx . return . Left


ioeToHx :: IO (Either Text a) -> HandlerX a
ioeToHx = ioxToHx . ioeToIox 

ioxToHx :: ExceptT Text IO a -> HandlerX  a
-- ioXtoHX = ExceptT . lift  . runExceptT
ioxToHx = mapExceptT lift

heToHx :: Handler (Either Text a)  -> HandlerX a
heToHx = ExceptT

ioeToIox:: IO (Either Text a) -> ExceptT Text IO a
ioeToIox = ExceptT

hxtoHe :: HandlerX a -> Handler (Either Text a)
hxtoHe = runExceptT 


eToX :: Monad m => Either e a -> ExceptT e m a
eToX =  either throwError return


-- * Categories
-- ** Items

  -- return finder

categoriesH :: Handler [Text]
categoriesH = do 
  catRulesMap <- appCategoryRules <$> getsYesod appSettings
  return $ concatMap keys catRulesMap

batchCategoriesH :: Handler [Text]
batchCategoriesH = do
  categories <- categoriesH
  -- at the moment filter categroy starting with match
  return $ filter (isPrefixOf "batch") categories

_allSkus :: Handler [Key FA.StockMaster]
_allSkus = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  -- selectKeysList doesn't work with non Integer KEy : Bug reported
  entities <- runDB $ selectList (filterE FA.StockMasterKey FA.StockMasterId (Just . LikeFilter $ stockLike) ) []
  return $ map entityKey entities

-- ** Customers

  -- return finder

customerCategoriesH :: Handler [Text]
customerCategoriesH = do 
  catRulesMap <- appCustomerCategoryRules <$> getsYesod appSettings
  return $ concatMap keys catRulesMap
-- ** Orders
orderCategoriesH :: Handler [Text]
orderCategoriesH = do 
  catRulesMap <- appOrderCategoryRules <$> getsYesod appSettings
  return $ concatMap keys catRulesMap

-- * Misc
-- todayH :: Handler Day
-- todayH :: MonadIO io => io Day
todayH = utctDay <$> liftIO getCurrentTime

-- | Find beginning of fiscal year
fiscalYearH :: Handler Day
fiscalYearH = do
  today <- todayH
  fiscalm <- runDB $ selectFirst [FA.FiscalYearEnd >. Just today] [Desc FA.FiscalYearBegin]
  case fiscalm of
    Just (Entity _ fiscal) -> return $ fromJust $ FA.fiscalYearBegin fiscal
    Nothing -> do
      fiscal <- runDB $ selectFirst [] [Desc FA.FiscalYearBegin]
      return . fromMaybe (error "No fiscal year configured in the database") $  fiscal >>= (FA.fiscalYearBegin . entityVal)
