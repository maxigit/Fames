{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- * Overview
-- | Miscellaneous functions to help rendering
-- | and/or accessing the database
module Handler.Util
( entitiesToTable
, getDBName
, getHaskellName
, entityTableHandler
, entityTableHandler'
, uploadFileForm
, uploadFileFormWithComment
, Encoding(..)
, readUploadUTF8
, setAttachment
, generateLabelsResponse
, firstOperator
, badgeSpan
, tshowM
, showDouble
, basePriceList
, timeProgress
, allOperators
, operatorFinder
, operatorFinderWithError
, FilterExpression(..)
, filterE
, filterEField
, filterEKeyword
, readUploadOrCacheUTF8
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
import System.Directory (removeFile)
import System.IO.Temp (openTempFile)
-- import System.Exit (ExitCode(..))
import Data.Streaming.Process (streamingProcess, proc, Inherited(..), waitForStreamingProcess, env)
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html (Markup)
import FA as FA
import Data.Time (diffDays, addGregorianMonthsClip)
import System.Directory (doesFileExist)
import qualified Data.Map.Strict as Map
import qualified Data.List as Data.List
import Model.DocumentKey
import Control.Monad.Except
import Text.Printf(printf) 
import Text.Regex.TDFA ((=~))

-- import Data.IOData (IOData)

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


-- ** Read and reload file
-- | When validating a file, the file needs to be uploaded once for validating.
-- In order to be sure that we are processing the file which has been validated
-- (and because it's not possible to preset the upload parameter on the new form)
-- we save the first time, the file to a temporary file and use the key to reload it
-- on the second time. The key and path will need to be set to the form (as hidden parameter)
-- readUploadOrCache.
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
      let tmp (DocumentHash file) = "/tmp" </> (unpack file)
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
            writeFile (tmp key) ss
          return $ Just (ss, key, fileName fileInfo)
        (_,_,_) -> return Nothing
  
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


-- * SQl
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
  
-- * Cached Value accross session
-- ** From Front Accounting
-- Price list used as base to calculate other.
-- Found it FA system preferecense
basePriceList :: Handler Int
basePriceList = cache0 False cacheForEver "base-price-list" $ do
  [Entity _ prefs  ] <- runDB $ selectList [FA.SysPrefId ==. FA.SysPrefKey "base_sales"] []
  let Just basePl = readMay =<< FA.sysPrefValue prefs
  return basePl
  
-- ** Fames
-- *** Operators
allOperators :: Handler (Map (Key Operator) Operator)
allOperators = cache0 False cacheForEver "all-operators" $ do
  operators <- runDB $ selectList [] []
  return $ mapFromList [ (key, operator) | (Entity key operator) <- operators ]
  
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
-- | Return a function finding the category given a style
-- The current implementation is based on TDFA regex
-- which are pretty so we cache it into a big map
categoryFinder :: Handler (Text -> Text -> Maybe Text)
categoryFinder = do
  -- each entry in the correspond to a caterogy heading
  catRulesMap <- appCategoryRules <$> getsYesod appSettings
  let flattenRules rules = concatMap (Map.toList) rules
      rulesMap = fmap flattenRules catRulesMap
      finderFor rules s = asum [ Just result
                            | (regex, result) <- rules
                            , unpack s =~ unpack regex
                            ]
      finder category item = do -- Maybe
        rules <- lookup category rulesMap
        finderFor rules item 



  return finder

categoriesH :: Handler [Text]
categoriesH = do 
  catRulesMap <- appCategoryRules <$> getsYesod appSettings
  return $ keys catRulesMap

categoryFinderCached :: Handler (Text -> Text -> Maybe Text)
categoryFinderCached = do
  sku'cat <- cache0 False cacheForEver "category-map" $ do
    categories <- categoriesH
    catFinder <- categoryFinder
    stockKeys <- allSkus
    let allSkus = map FA.unStockMasterKey stockKeys
    return $ mapFromList [ (heading, submap)
                         | heading <- categories
                         , let submap = mapFromList [(sku, cat)
                                                    | sku <- allSkus
                                                    , Just cat <- return $ catFinder heading sku
                                                    ]
                         ]
    
  let finder category item = do -- Maybe
        m <- Map.lookup category sku'cat
        Map.lookup item m
  return finder

allSkus :: Handler [Key FA.StockMaster]
allSkus = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  -- selectKeysList doesn't work with non Integer KEy : Bug reported
  entities <- runDB $ selectList (filterE FA.StockMasterKey FA.StockMasterId (Just . LikeFilter $ stockLike) ) []
  return $ map entityKey entities
