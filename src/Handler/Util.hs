{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- * Overview
-- | Miscellaneous functions to help rendering
-- | and/or accessing the database
module Handler.Util
( entitiesToTable
, getDBName
, getHaskellName
, entityTableHandler , entityTableHandler'
, uploadFileForm
, uploadFileFormWithComment
, hiddenFileForm
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
, splitSnake
, basePriceList
, timeProgress
, allOperators
, operatorFinder
, operatorFinderWithError
, FilterExpression(..)
, filterE
, filterEField
, filterEKeyword
, filterEToSQL
, filterEAddWildcardRight
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
, categoryFinderCached
, categoriesH
, categoriesFor
, refreshCategoryCache
, refreshCategoryFor
, customerCategoryFinderCached
, customerCategoriesH
, customerCategoriesFor
, refreshCustomerCategoryCache
, refreshCustomerCategoryFor
, refreshOrderCategoryCache
, refreshNewOrderCategoryCache
, orderCategoriesH
, Identifiable(..)
, getIdentified
, renderField
, allCustomers
, allSuppliers
, loadStockMasterRuleInfos
, loadDebtorsMasterRuleInfos
, applyCategoryRules
, todayH
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

import Text.Read(Read,readPrec)
import qualified Data.Map as LMap
-- import Data.IOData (IOData)
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))
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

--  | Generate a form meant to be used to reload cache using readUploadOrCache
-- hiddenFileForm
--   :: DocumentHash
--      -> Maybe (DocumentHash, FilePath)
--      -> _ (FormResult (DocumentHash, FilePath), Widget)
hiddenFileForm key'pathM = renderBootstrap3 BootstrapBasicForm form where
  form =
    (,) <$> areq hiddenField  "key" (fmap fst  key'pathM)
        <*> areq hiddenField "path" (fmap snd key'pathM)
   

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

-- | Return a function finding the category given a style
-- The current implementation is based on TDFA regex
-- which are pretty so we cache it into a big map
categoryFinderCached :: Handler (Text -> FA.StockMasterId -> Maybe Text)
categoryFinderCached = cache0 False cacheForEver "category-finder" $ do
  reverseKey <- getsYesod appSettings <&> appReverseCategoryKey
  refreshCategoryCache False
  -- we reverse the stock_id to speed up string comparasison
  -- as most items share a common prefix, it might be faster to compare them from right to left 
  if reverseKey  
    then do
    itemCategories <- runDB $ rawSql "SELECT ?? FROM fames_item_category_cache ORDER BY REVERSE(stock_id), category " [] -- selectList [] [Asc ItemCategoryStockId, Asc ItemCategoryCategory]
    let sku'catMapR = Map.fromAscList [((reverse itemCategoryStockId , itemCategoryCategory ), itemCategoryValue )
                                      | (Entity _ ItemCategory{..}) <- itemCategories
                                      ]
        finder category (FA.StockMasterKey sku) = Map.lookup (reverse sku, category) sku'catMapR
    return $ sku'catMapR `seq` finder
  else do
       itemCategories <- runDB $ selectList [] [Asc ItemCategoryStockId, Asc ItemCategoryCategory]
       let sku'catMap = Map.fromAscList [((itemCategoryStockId , itemCategoryCategory ), itemCategoryValue )
                                         | (Entity _ ItemCategory{..}) <- itemCategories
                                         ]
           finder category (FA.StockMasterKey sku) = Map.lookup (sku, category) sku'catMap
       return $ sku'catMap `seq` finder

_allSkus :: Handler [Key FA.StockMaster]
_allSkus = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  -- selectKeysList doesn't work with non Integer KEy : Bug reported
  entities <- runDB $ selectList (filterE FA.StockMasterKey FA.StockMasterId (Just . LikeFilter $ stockLike) ) []
  return $ map entityKey entities

-- *** Category caches
type StockMasterRuleInfo = (Key StockMaster
                           , (Single String, Single String, Single String, Single String)
                           , (Single (Maybe String), Single (Maybe String), Single (Maybe String), Single (Maybe String))
                           , (Single String, Single String, Single String, Single String)
                           , Single (Maybe Double))
refreshCategoryFor :: Maybe FilterExpression -> Handler ()
refreshCategoryFor stockFilterM = do
  stockFilter <- case stockFilterM of
    Just sf -> return sf
    Nothing -> LikeFilter <$> appFAStockLikeFilter . appSettings <$> getYesod
  rulesMaps <- appCategoryRules <$> getsYesod appSettings
  stockMasters <- loadStockMasterRuleInfos stockFilter 
  let categories = concatMap (categoriesFor rules) stockMasters
      rules = map (first unpack) $ concatMap mapToList rulesMaps
  runDB $ do
    deleteWhere (filterE id ItemCategoryStockId (Just stockFilter))
    -- insertMany_ categories
    mapM_ insert_ categories

-- Populates the item category cache table if needed
refreshCategoryCache :: Bool -> Handler ()
refreshCategoryCache force = do
  -- check if the datbase is empty
  -- clear it if needed
  c <- runDB $ do
    when force (deleteWhere ([] ::[Filter ItemCategory]))
    count ([] ::[Filter ItemCategory]) 
  when (c == 0) (refreshCategoryFor Nothing)
  
loadStockMasterRuleInfos :: FilterExpression -> Handler [StockMasterRuleInfo]
loadStockMasterRuleInfos stockFilter = do
  base <- basePriceList
  let sql = " "
            <> "select sm.stock_id "

            <> "     , sm.description "
            <> "     , sm.long_description "
            <> "     , sm.units "
            <> "     , sm.mb_flag "

            <> "     , tt.name "
            <> "     , cat.description as category "
            <> "     , dim1.name as dim1 "
            <> "     , dim2.name As dim2 "

            <> "     , sm.sales_account "
            <> "     , sm.cogs_account "
            <> "     , sm.inventory_account "
            <> "     , sm.adjustment_account "

            <> "     , sales.price "
            <> "from 0_stock_master as sm "
            <> "left join 0_tax_types  AS tt on (sm.tax_type_id = tt.id) "
            <> "left join 0_dimensions as dim1 on (sm.dimension_id = dim1.id) "
            <> "left join 0_dimensions as dim2 on (sm.dimension2_id = dim2.id) "
            <> "left join 0_stock_category as cat on (sm.category_id = cat.category_id) "
            <> "left join 0_prices as sales on (sm.stock_id = sales.stock_id AND sales.sales_type_id =?) "
            <> "where sm.stock_id "<> keyw <> "?"
      (keyw, p) = filterEKeyword stockFilter

  runDB $ rawSql sql [toPersistValue base, PersistText p]


-- We use string to be compatible with regex substitution
-- applyCategoryRules :: [(String, CategoryRule)]
--                    -> StockMasterRuleInfo -> Map String String
-- applyCategoryRules
--   :: [(String, CategoryRule)]
--      -> StockMasterRuleInfo
--      -> (Key StockMaster, Map String String)
applyCategoryRules rules =
  let inputKeys = ["sku"
              ,"description"
              ,"longDescription"
              ,"unit"
              ,"mbFlag"
              ,"taxType"
              ,"category"
              ,"dimension1"
              ,"dimension2"
              ,"salesAccount"
              ,"cogsAccount"
              ,"inventoryAccount"
              ,"adjustmentAccount"
              ] -- inputMap key, outside of the lambda so it can be optimised and calculated only once
      catRegexCache =  mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys  <> map fst rules))
  in \(stockId
                         , (Single description, Single longDescription, Single units, Single mbFlag)
                         , (Single taxType, Single category, Single dimension1, Single dimension2 )
                         , (Single salesAccount, Single cogsAccount, Single inventoryAccount, Single adjustmentAccount)
                         , Single salesPrice )
     -> let
              sku = unStockMasterKey stockId
              ruleInput = RuleInput ( mapFromList 
                                    ( ("sku", unpack sku) :
                                      ("description", description) :
                                      ("longDescription", longDescription) :
                                      ("unit", units) :
                                      ("mbFlag", mbFlag) :
                                      (("taxType",) <$> taxType) ?:
                                      (("category",) <$> category) ?:
                                      (("dimension1",) <$> dimension1) ?:
                                      (("dimension2",) <$> dimension2) ?:
                                      ("salesAccount", salesAccount) :
                                      ("cogsAccount", cogsAccount) :
                                      ("inventoryAccount", inventoryAccount) :
                                      ("adjustmentAccount", adjustmentAccount) :
                                    []
                                    )
                                  )
                                  salesPrice
        in (stockId, computeCategories catRegexCache rules ruleInput (unpack sku))


categoriesFor :: [(String, CategoryRule)] -> StockMasterRuleInfo   -> [ItemCategory]
categoriesFor rules = let
  applyCached =  applyCategoryRules rules
  in \info -> let
      (sku, categories ) = applyCached info

      in [ ItemCategory (FA.unStockMasterKey sku) (pack key) (pack value)
        | (key, value) <- mapToList categories
        ]






-- ** Customers

  -- return finder

customerCategoriesH :: Handler [Text]
customerCategoriesH = do 
  catRulesMap <- appCustomerCategoryRules <$> getsYesod appSettings
  return $ concatMap keys catRulesMap

-- | Return a function finding the customerCategory given a style
-- The current implementation is based on TDFA regex
-- which are pretty so we cache it into a big map
customerCategoryFinderCached :: Handler (Text -> FA.DebtorsMasterId -> Maybe Text)
customerCategoryFinderCached = cache0 False cacheForEver "customerCategory-finder" $ do
  refreshCustomerCategoryCache False
  customerCategories <- runDB $ selectList [] [Asc CustomerCategoryCustomerId, Asc CustomerCategoryCategory]
  let debtor'catMap = Map.fromDistinctAscList [((customerCategoryCustomerId , customerCategoryCategory ), customerCategoryValue )
                           | (Entity _ CustomerCategory{..}) <- customerCategories
                           ]
      finder customerCategory (FA.DebtorsMasterKey debtor) = Map.lookup (debtor, customerCategory) debtor'catMap

  debtor'catMap `seq` return finder


-- *** CustomerCategory caches
type DebtorsMasterRuleInfo = (Key DebtorsMaster
                           , (Single String, Single String, Single String, Single String) -- debtor infos
                           , (Single (Maybe String), Single (Maybe String)) -- dimensions
                           , (Single (Maybe Day), Single (Maybe String)) -- first order
                           , (Single (Maybe Day), Single (Maybe String)) -- last order
                           )
refreshCustomerCategoryFor :: Handler ()
refreshCustomerCategoryFor = do
  rulesMaps <- appCustomerCategoryRules <$> getsYesod appSettings
  debtorsmasters <- loadDebtorsMasterRuleInfos
  let customerCategories = concatMap (customerCategoriesFor rules) debtorsmasters
      rules = map (first unpack) $ concatMap mapToList rulesMaps
  runDB $ do
    deleteWhere ([] :: [Filter CustomerCategory])
    -- insertMany_ customerCategories
    mapM_ insert_ customerCategories

-- Populates the customer customerCategory cache table if needed
refreshCustomerCategoryCache :: Bool -> Handler ()
refreshCustomerCategoryCache force = do
  -- check if the datbase is empty
  -- clear it if needed
  c <- runDB $ do
    when force (deleteWhere ([] ::[Filter CustomerCategory]))
    count ([] ::[Filter CustomerCategory]) 
  when (c == 0) (refreshCustomerCategoryFor) 
  
loadDebtorsMasterRuleInfos :: Handler [DebtorsMasterRuleInfo]
loadDebtorsMasterRuleInfos = do
  let sql = " "
            <> "select dm.debtor_no "
            <> "     , dm.name"
            <> "     , dm.notes"
            <> "     , dm.tax_id"
            <> "     , dm.curr_code"
            <> "     , dim1.name as dim1 "
            <> "     , dim2.name As dim2 "
            <> "     , ord.first_ord_date "
            <> "     , ord.first_ord_ref "
            <> "     , ord.last_ord_date "
            <> "     , ord.last_ord_ref "
            <> " from 0_debtors_master as dm "
            <> " left join 0_dimensions as dim1 on (dm.dimension_id = dim1.id) "
            <> " left join 0_dimensions as dim2 on (dm.dimension2_id = dim2.id) "
            <> " left join 0_prices as sales on (sales.sales_type_id = dm.sales_type) "
            <> " left join (" <> orders <> ") ord on(dm.debtor_no = ord.debtor_no) "
      orders = " "
              <> " select debtor_no, MIN(ord_date) AS first_ord_date, MAX(ord_date) AS last_ord_date"
              <> " , SUBSTRING_INDEX(GROUP_CONCAT(reference order by ord_date), ',', 1) AS first_ord_ref"
              <> " , SUBSTRING_INDEX(GROUP_CONCAT(reference order by ord_date desc), ',', 1) AS last_ord_ref"
              <> " from 0_sales_orders"
              <> " group by debtor_no"
      decode (debtorId , (Single name, Single note, Single taxCode, Single currency)
              , dims, firstOrder, lastOrder
              ) = (debtorId , (Single . unpack . decodeHtmlEntities $ pack  name
                              , Single . unpack . decodeHtmlEntities $ pack note, Single taxCode, Single currency)
              , dims, firstOrder, lastOrder
              )

  infos <- runDB $ rawSql sql []
  return $ map decode infos


-- We use string to be compatible with regex substitution
-- applyCategoryRules :: [(String, CustomerCategoryRule)]
--                    -> DebtorsMasterRuleInfo -> Map String String
-- applyCategoryRules
--   :: [(String, CustomerCategoryRule)]
--      -> DebtorsMasterRuleInfo
--      -> (Key DebtorsMaster, Map String String)
applyCustomerCategoryRules rules =
  let inputKeys = ["debtor_id"
              ,"name"
              ,"note"
              ,"tax_code"
              ,"currency"
              ,"dimension1"
              ,"dimension2"
              ] -- inputMap key, outside of the lambda so it can be optimised and calculated only once
      catRegexCache =  mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys  <> map fst rules))
  in \(debtorId
                         , (Single name, Single note, Single taxCode, Single currency)
                         , (Single dimension1, Single dimension2 )
                         , (Single firstOrderDate, Single firstOrderRef)
                         , (Single lastOrderDate, Single lastOrderRef)
                         )
     -> let
              debtor = unDebtorsMasterKey debtorId
              ruleInput = RuleInput ( mapFromList 
                                    ( ("id", show debtor ) :
                                      ("name", unpack name) :
                                      ("note", note) :
                                      ("tax_code", taxCode) :
                                      ("currency", currency) :
                                      (("dimension1",) <$> dimension1) ?:
                                      (("dimension2",) <$> dimension2) ?:
                                      ((("first_order_date",) . show) <$> firstOrderDate) ?:
                                      (("first_order_ref",) <$> firstOrderRef) ?:
                                      ((("last_order_date",) . show) <$> lastOrderDate) ?:
                                      (("last_order_ref",) <$> lastOrderRef) ?:
                                    []
                                    )
                                  )
                                  (Just $ fromIntegral debtor)
        in (debtorId, computeCategories catRegexCache rules ruleInput (unpack name))


customerCategoriesFor :: [(String, CategoryRule)] -> DebtorsMasterRuleInfo   -> [CustomerCategory]
customerCategoriesFor rules = let
  applyCached =  applyCustomerCategoryRules rules
  in \info -> let
      (debtor, customerCategories ) = applyCached info 

      in [ CustomerCategory (FA.unDebtorsMasterKey debtor) (pack key) (pack value)
        | (key, value) <- mapToList customerCategories
        ]






--- *** Order Category caches
-- | Clears all order categories and computes the n first if given
refreshOrderCategoryCache :: Maybe Int -> Handler ()
refreshOrderCategoryCache nM = do
  runDB $ deleteWhere ([] :: [Filter OrderCategory])
  refreshNewOrderCategoryCache nM

-- | Computes Order category and cache them
refreshNewOrderCategoryCache :: Maybe Int -> Handler ()
refreshNewOrderCategoryCache nM = runDB $ do
  rulesMap <- appOrderCategoryRules <$> getsYesod appSettings
  -- find last order with a category
  [(Single lastOrderM)] <- rawSql "SELECT max(order_id) FROM fames_order_category_cache" []
  orders <- selectList ( (FA.SalesOrderTransType ==. fromEnum ST_SALESORDER)
                       : maybe [] (return . (FA.SalesOrderOrderNo >.)) lastOrderM
                       ) (Asc FA.SalesOrderOrderNo : (maybe [] (return . LimitTo) nM))
  let orderCategories = concatMap (orderCategoriesFor rules) orders
      rules = map (first unpack) $ concatMap mapToList rulesMap
  insertMany_ orderCategories
  
orderCategoriesFor :: [(String, CategoryRule)] -> Entity FA.SalesOrder -> [OrderCategory]
orderCategoriesFor rules = let
  -- trick to for applyCached to be computed only once, so that the regular expressions
  -- are only computed once
  applyCached = applyOrderCategoryRules rules
  in \orderE -> let
       (orderId, orderCategories) = applyCached orderE
       in [OrderCategory (orderId) (pack key) (pack value)
          | (key, value) <- mapToList orderCategories
          ]

applyOrderCategoryRules rules =
  let inputKeys = ["date"
                  , "delivery-date"
                  , "delivery-delay"
                  , "reference"
                  , "customer-ref"
                  , "price_list"
                  , "shipping-cost"
                  , "shipping-via"
                  -- , "deliveryAddress"
                  , "amount"
                  , "amount-PPD"
                  , "PPD-days"
                  , "comment"

                  ]
      catRegexCache = mapFromList (map (liftA2 (,) id mkCategoryRegex) (inputKeys <> map fst rules))
  in \(Entity orderId FA.SalesOrder{..}) -> let
        ruleInput = RuleInput (mapFromList riList) (Just $ salesOrderTotal)
        riList = ("date", show salesOrderOrdDate) :
                 ("delivery-date", show salesOrderDeliveryDate) :
                 ("delivery-delay", printf "%03d" $ diffDays salesOrderDeliveryDate salesOrderOrdDate) :
                 ("reference", unpack salesOrderReference) :
                 ("customer-ref", unpack salesOrderCustomerRef) :
                 ("price_list", show salesOrderOrderType ) :
                 ("shipping-cost", printf "%09.2f"  salesOrderFreightCost) : -- padding with 0 to 6 figures 
                 ("shipping-via", show salesOrderShipVia) :
                 ("amount", printf "%09.2f"  salesOrderTotal) : -- padding with 0 to 6 figures 
                 ((("amount-PPD",) . printf "%09.2f") <$>  salesOrderTotalPpd) ?: -- padding with 0 to 6 figures 
                 ("PPD-days",  show  salesOrderPpdDays) : -- padding with 0 to 6 figures 
                 ((("comment",) . unpack) <$> salesOrderComments) ?:
                 []
        in (salesOrderOrderNo, computeCategories catRegexCache rules ruleInput (unpack salesOrderReference))

orderCategoriesH :: Handler [Text]
orderCategoriesH = do 
  catRulesMap <- appOrderCategoryRules <$> getsYesod appSettings
  return $ concatMap keys catRulesMap
-- * Misc
-- todayH :: Handler Day
-- todayH :: MonadIO io => io Day
todayH = utctDay <$> liftIO getCurrentTime
