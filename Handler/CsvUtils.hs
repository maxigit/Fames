{-# LANGUAGE PatternSynonyms, LiberalTypeSynonyms, DeriveFunctor #-}
-- | Miscellaneous functions and types to parse and render CSV.
module Handler.CsvUtils where

import Import
import qualified Data.Csv as Csv

import Data.Either
import Data.Time(parseTimeM)
import qualified Data.Map as Map
import Data.Char (ord,toUpper)
-- * Types
-- | If we can't parse the csv at all (columns are not present),
-- we need a way to gracefully return a error
data InvalidSpreadsheet = InvalidSpreadsheet
  { errorDescription :: Text
  , missingColumns :: [Text]
  , columnIndexes :: [Int] -- ^ index of present columns
  , sheet :: [[ Either Csv.Field Text]]  -- ^ origin file
  } deriving Show

data InvalidField = ParsingError { invFieldType :: Text
                                 , invFieldValue :: Text
                                 }
                  | MissingValueError { invFieldType :: Text }
  deriving (Read, Show)

invalidFieldError :: InvalidField -> Text
invalidFieldError ParsingError{..} = "Can't parse '"
                                     <> invFieldValue
                                     <> "' as "
                                     <> invFieldType
                                     <> "."
invalidFieldError MissingValueError{..} = invFieldType <> " is missing."

data ValidField a = Provided { validValue :: a} | Guessed { validValue :: a }  deriving Functor
instance Applicative ValidField  where
  pure = Provided
  (Provided f) <*> (Provided v) = Provided (f v)
  f <*> v = Guessed $ (validValue f) (validValue v)

guess :: ValidField a -> ValidField a
guess v = Guessed (validValue v)

deriving instance Show a => Show (ValidField a)
deriving instance Read a => Read (ValidField a)
deriving instance Eq a => Eq (ValidField a)
-- * Patterns
pattern RJust x = Right (Just x)
pattern RNothing = Right Nothing
pattern RRight x = Right (Right x)
pattern RLeft x = Right (Left x)
pattern LRight x = Left (Right x)
pattern LLeft x = Left (Left x)
-- * Transformables
-- | Sort of convert but can loose information
-- Used to demote valid rows to invalid ones.
class Transformable a b where
  transform :: a -> b

instance Transformable a () where
  transform = const ()

instance  Transformable a a where
  transform x =  x

instance Applicative f => Transformable a (f a) where
  transform = pure

instance Alternative f => Transformable () (f a) where
  transform = const empty

instance Transformable (ValidField a) a where
  transform v = validValue v
 
instance Transformable a (ValidField a) where
  transform x = Provided x

-- * Functions

parseInvalidSpreadsheet columnMap bytes err =
  let columns = fromString <$> keys columnMap 
      decoded = toList <$> Csv.decode Csv.NoHeader bytes :: Either String [[Either Csv.Field Text]]
      onEmpty = InvalidSpreadsheet "The file is empty" columns [] []
  in case (null bytes, decoded) of
       (True, _ )  -> onEmpty
       (_, Right [])  -> onEmpty
       (False, Left err) -> InvalidSpreadsheet "Can't parse file. Please check the file is encoded in UTF8 or is well formatted." columns [] [[Left (toStrict bytes)]]
       (False, Right sheet@(headerE:_)) -> do
         let headerPos = Map.fromList (zip header [0..]) :: Map Text Int
             header = map (either decodeUtf8 id) headerE
             -- index of a given column in the current header. Starts a 0.
             indexes = [ (fromString col
                         , asum [ lookup (fromString col') headerPos
                                | col' <- cols
                                ]
                         )
                       | (col, cols)  <- Map.toList columnMap
                       ]
             missingColumns = [col | (col, Nothing) <- indexes]
             columnIndexes = catMaybes (map snd indexes)
             errorDescription = case traverse sequence sheet of
                                     Left _ -> "Encoding is wrong. Please make sure the file is in UTF8"
                                     Right _ -> tshow err
         InvalidSpreadsheet{..}

parseSpreadsheet :: Csv.FromNamedRecord a => Map String [String] ->  Maybe String -> ByteString -> Either InvalidSpreadsheet [a]
parseSpreadsheet columnMap seps bytes = do
  let lbytes = fromStrict bytes
      (sep:_) = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- fromMaybe ",;\t" seps ]
  case Csv.decodeByNameWith sep lbytes of
    Left err -> Left $ parseInvalidSpreadsheet columnMap lbytes err
    Right (header, vector) -> Right $ toList vector
  

validateNonEmpty :: Text -> Either InvalidField (Maybe a) -> Either InvalidField (Maybe a)
validateNonEmpty field RNothing = Left (MissingValueError field) 
validateNonEmpty field v = v

expandColumnName :: String -> [String]
expandColumnName colname = [id, capitalize, map Data.Char.toUpper] <*> [colname]

capitalize [] = []
capitalize (x:xs) = Data.Char.toUpper x : xs

-- ** Custom field parsers.
-- | temporary class to remove currency symbol
newtype Currency = Currency {unCurrency :: Double} deriving (Show, Eq, Num, Fractional)
instance Csv.FromField Currency where
  parseField bs = do
    case stripPrefix "-" bs of
      Just bs' -> negate <$> Csv.parseField bs'
      Nothing -> do
        let stripped = bs `fromMaybe` stripPrefix (encodeUtf8 "£") bs
            res = Currency <$> Csv.parseField stripped
        -- trace ("Currency: " ++ show (bs, stripped)) res
        res

-- | try different date format
newtype AllFormatsDay = AllFormatsDay {allFormatsDay :: Day} deriving Show
instance Csv.FromField AllFormatsDay where
  parseField bs = do
    str <- Csv.parseField bs
    case  concat [parseTimeM True defaultTimeLocale f str | f <- formats] of
      [day] -> return $ AllFormatsDay day
      (d:ds) -> error (show (d:ds))
      _ -> mzero
    where
        -- formats to try. Normally there shouldn't be any overlap bettween the different format.
        -- The 0 in %0Y is important. It guarantes that only 4 digits are accepted.
        -- without 11/01/01 will be parsed by %Y/%m/%d and %d/%m/%y
        formats = [ "%0Y/%m/%d"
                  , "%d/%m/%0Y"
                  , "%d/%m/%y"
                  , "%0Y-%m-%d"
                  , "%d %b %0Y"
                  , "%d-%b-%0Y"
                  , "%d %b %y"
                  , "%d-%b-%y"
                  , "%0Y %b %d"
                  , "%0Y-%b-%d"
                  , "%a %d %b %0Y"
                  ]

instance (Csv.FromField a) => Csv.FromField (ValidField a) where
  parseField bs = do
    val <- Csv.parseField bs
    return $ Provided val

-- | Generate a Either InvalidField from a cell value and parsed field.
toError :: Text -> Either Csv.Field a -> Either InvalidField a
toError t e = case e of
  Left err -> Left (ParsingError ("Invalid format") t)
  Right v -> Right v
-- * Renderable

class Renderable r where
  render :: r -> Widget

instance Renderable () where
  render () = return ()
instance Renderable Int where
  render i = [whamlet|#{tshow i}|]

instance Renderable Double where
  render d = [whamlet|#{formatDouble d}|]

instance Renderable Text where
  render t = [whamlet|#{t}|]

instance Renderable Day where
  render t = [whamlet|#{tshow t}|]

instance (Renderable r) => Renderable (Maybe r) where
  render (Just x) = render x
  render Nothing = [whamlet||]

instance (Renderable l, Renderable r) => Renderable (Either l r) where
  render (Left r) = [whamlet|<span.left>^{render r}|]
  render (Right r) = [whamlet|<span.right>^{render r}|]

instance Renderable InvalidField where
  render invField = do
    let (class_, value) = case invField of
          ParsingError _ v -> ("parsing-error" :: Text, v)
          MissingValueError _ -> ("missing-value" :: Text,"<Empty>")
    toWidget [cassius|
.parsing-error, .missing-value
  .description
     display:none
.missing-value
  .message
    font-style: italic
|]
    [whamlet|
<span class="#{class_}">
  <span.description>#{invalidFieldError invField}
  <span.message.text-danger data-toggle="tooltip" title="#{invalidFieldError invField}">#{value}
|]
    toWidget [julius|
$('[data-toggle="tooltip"]').tooltip();
|]
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
  
instance Renderable a => Renderable (ValidField a) where
  render (Provided x) = render x
  render (Guessed x) = do
    toWidget [cassius|

|]
    [whamlet|
<span.guessed-value>^{render x}
|]

instance Renderable InvalidSpreadsheet where
  render i@InvalidSpreadsheet{..} = let
    colClass = go 0 (sort columnIndexes)
    go :: Int -> [Int]-> [Text]
    go i [] = "" : go i []
    go i (ix:ixs) | ix == i = "bg-success" : go (i+1) ixs
                  | otherwise = "" : go (i+1) (ix:ixs)
    convertField :: Either Csv.Field Text -> (Text, Text)
    convertField (Left bs) = ("bg-danger text-danger", decodeUtf8 bs)
    convertField (Right t) = ("", t)
    in  {-trace ("toHtml" ++ show i ) -}[whamlet|
<div .invalid-receipt>
  <div .error-description> #{errorDescription}
  $if not (null missingColumns)
    <div .missing-columns .bg-danger .text-danger>
      The following columns are missing:
      <ul>
        $forall column <- missingColumns
          <li> #{column}
  <table.sheet.table.table-bordered>
    $forall line <- sheet
      <tr>
        $forall (class_, field) <- zip colClass line
          $with (fieldClass, fieldValue) <- convertField field
            <td class="#{class_} #{fieldClass}"> #{fieldValue}
          |]
           
             

