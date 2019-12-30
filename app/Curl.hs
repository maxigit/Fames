{-# LANGUAGE ImplicitParams #-}
module Curl where
import Network.Curl
import Text.HTML.TagSoup 


import ClassyPrelude
import Control.Monad.Except
import Data.Decimal
import Data.Aeson(eitherDecodeStrict, FromJSON, Value)
import Data.Yaml.Pretty(encodePretty, defConfig) 

-- ** Curl
docurl:: (?curl :: Curl) => URLString -> [CurlOption] -> ExceptT Text IO CurlResponse
docurl url opts = lift $ do_curl_ ?curl url opts


withCurl :: ExceptT e' IO b -> ExceptT e' IO b
withCurl = mapExceptT withCurlDo
  
-- | Merge all CurlPostFields option into one
-- Otherewise, only the last one is kept, which can cause problem
mergePostFields :: [CurlOption] -> [CurlOption]
mergePostFields opts = let
  getPostFields (CurlPostFields p_fields) = Just p_fields
  getPostFields _ = Nothing

  (posts, normals) = partition (isJust . getPostFields) opts
  fields = mapMaybe getPostFields posts
  in case concat fields of
    [] -> normals
    fs  -> CurlPostFields fs : normals

doCurlWith :: (?curl :: Curl)
         => (Int -> String -> Either Text r)
         -> (Int -> String -> Maybe Text)
         -> URLString -- ^ Url
         -> [CurlOption] -- ^ Options
         -> [Int] -- ^ Expected status
         -> Text -- ^ To add to error message
         -> ExceptT Text IO r
doCurlWith onsuccess onfailure url opts status msg = do
  -- traceShowM ("POST to CURL", url, opts)
  r <- docurl url (mergePostFields opts)
  -- traceShowM ("RESP", respStatus r, respBody r)
  when (respCurlCode r /= CurlOK || (respStatus r `notElem` status && not (null status))) $ do
      throwError . {- traceShowId $ -} unlines $
        [ "Failed to : " <> msg
        , "CURL status: " <> tshow (respCurlCode r)
        , "HTTP status: " <> pack (respStatusLine r)
        ] <> maybeToList ( ("Body: " <>) <$> onfailure (respStatus r) (respBody r)) <>
        [ "when accessing URL: '" <> tshow url <> "'"
        , "If the problem persits, contact your administrator."
        ]
  ExceptT $ return $ onsuccess (respStatus r) (respBody r)

curlJson :: (?curl :: Curl, FromJSON json)
         => URLString -- ^ Url
         -> [CurlOption] -- ^ Options
         -> [Int] -- ^ Expected status
         -> Text -- ^ To add to error message
         -> ExceptT Text IO json
curlJson = doCurlWith (const success) (const failure) where
  success = first fromString . eitherDecodeStrict . fromString
  failure body = Just $ case eitherDecodeStrict (fromString body)  of
    Left _ -> fromString body
    Right json -> decodeUtf8 $ encodePretty defConfig (json :: Value)

-- *** Post Paramters
class CurlPostField a where
  toCurlPostField :: a -> Maybe String


instance CurlPostField String where
  toCurlPostField = Just

instance CurlPostField Text where
  toCurlPostField = Just . unpack

instance CurlPostField Double where
  toCurlPostField  = Just . show

instance CurlPostField Decimal where
  toCurlPostField  = Just . show

instance CurlPostField Int where
  toCurlPostField  = Just . show

instance CurlPostField a => CurlPostField (Maybe a) where
  toCurlPostField = (>>= toCurlPostField)
  
curlPostFields :: [Maybe String] -> CurlOption
curlPostFields = CurlPostFields . catMaybes

-- | Creates a 
(<=>) :: CurlPostField a => String -> a -> Maybe String
field <=> value = fmap ((field <> "=") <> ) (toCurlPostField value)
(<?>) :: CurlPostField a => String -> a -> Maybe String
field <?> value = fmap (field  <> ) (toCurlPostField value)

-- | Like TagSoup.partitions but uses open AND close tag as separator (even though their are removed)
-- This allow for example to only what's within a form
-- @
-- partitionWithClone (=="form") <form><div>A</div><div>B</div></form><form>C
-- >  <form><div>A</div><div>B</div>
--    <form>C
-- @
partitionWithClose :: (Tag String) -> [Tag String] -> [[Tag String]]
partitionWithClose tag tags = let
  parts = partitions isOk tags
  (isOk, filterClose) = case tag of
           TagOpen name __attrs -> let
               close = TagClose name
               ok t = t ~== tag || t ~== close
               -- create filter to remove close tags
               -- after partitioning, each partitions should start with either an open
               -- or a closed tag. we keep the one
               toKeep (t:_) = t ~/=  close
               toKeep _ = True
               in (ok, toKeep)
                   
           _ -> ((~== tag), const True)
  in filter filterClose parts
  
                         
-- | remove blank tags
cleanTags :: [Tag String] -> [Tag String]
cleanTags tags = filter (not . blank) tags where
  blank (TagText t) = case t of
    "" -> True
    "\n" -> True
    _ -> False
  blank _ = False
