{-# LANGUAGE ImplicitParams #-}
module Curl where
import Network.Curl
import Text.HTML.TagSoup 
import Text.HTML.TagSoup.Match
import Text.Regex.TDFA
import ClassyPrelude
import Control.Monad.Except
import Data.Decimal
import Data.Aeson(eitherDecodeStrict, FromJSON)

-- ** Curl
docurl:: (?curl :: Curl) => URLString -> [CurlOption] -> ExceptT Text IO CurlResponse
docurl url opts = lift $ do_curl_ ?curl url opts


withCurl :: ExceptT e' IO b -> ExceptT e' IO b
withCurl = mapExceptT withCurlDo
  
-- | Merge all CurlPostFields option into one
-- Otherewise, only the last one is kept, which can cause problem
mergePostFields :: [CurlOption] -> [CurlOption]
mergePostFields opts = let
  getPostFields (CurlPostFields fields) = Just fields
  getPostFields _ = Nothing

  (posts, normals) = partition (isJust . getPostFields) opts
  fields = mapMaybe getPostFields posts
  in CurlPostFields (concat fields) : normals

doCurlWith :: (?curl :: Curl)
         => (String -> Either Text r)
         -> URLString -- ^ Url
         -> [CurlOption] -- ^ Options
         -> Int -- ^ Expected status
         -> Text -- ^ To add to error message
         -> ExceptT Text IO r
doCurlWith cont url opts status msg = do
  -- traceShowM ("POST to CURL", url, opts)
  r <- docurl url (mergePostFields opts)
  -- traceShowM ("RESP", respStatus r, respBody r)
  when (respCurlCode r /= CurlOK || respStatus r /= status) $ do
      throwError $ traceShowId $ unlines [ "Failed to : " <> msg
                           , "CURL status: " <> tshow (respCurlCode r)
                           , "HTTP status :" <> pack (respStatusLine r)
                           , "when accessing URL: '" <> tshow url <> "'"
                           , "If the problem persits, contact your administrator."
                           ]
  ExceptT $ return $ cont (respBody r)

curlJson :: (?curl :: Curl, FromJSON json)
         => URLString -- ^ Url
         -> [CurlOption] -- ^ Options
         -> Int -- ^ Expected status
         -> Text -- ^ To add to error message
         -> ExceptT Text IO json
curlJson = doCurlWith go where
  go = first fromString . eitherDecodeStrict . fromString
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
           TagOpen name attrs -> let
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
