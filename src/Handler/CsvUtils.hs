{-# LANGUAGE PatternSynonyms, LiberalTypeSynonyms, DeriveFunctor, DataKinds, PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-} 
-- | Miscellaneous functions and types to parse and render CSV.
module Handler.CsvUtils where

import Import hiding(toLower, Null)
import qualified Data.Csv as Csv

import Data.Time(parseTimeM)
import qualified Data.Map as Map
import Data.Char (ord,toUpper,toLower)
import Data.List (nub)
  
import qualified Data.ByteString.Lazy as LazyBS

-- * Types
-- | If we can't parse the csv at all (columns are not present),
-- we need a way to gracefully return a error
data InvalidSpreadsheet = InvalidSpreadsheet
  { errorDescription :: Text
  , missingColumns :: [Text]
  , columnIndexes :: [Int] -- ^ index of present columns
  , sheet :: [[ Either Csv.Field Text]]  -- ^ origin file
  } deriving (Eq, Read, Show)

data InvalidField = ParsingError { invFieldType :: Text
                                 , invFieldValue :: Text
                                 }
                  | MissingValueError { invFieldType :: Text }
                  | InvalidValueError { invFieldError :: Text 
                                      , invFieldValue :: Text
                                      }
  deriving (Read, Show, Eq)
-- State of a row after parsing
data RowTypes = RawT -- raw row, everything is text
              | PartialT -- final type but blank allowed
              | ValidT -- valid. can be guessed (if blank) or provided (by the user itself)
              | FinalT -- final type
              deriving (Read, Show, Eq)

data ParsingResult row result
  = WrongHeader InvalidSpreadsheet
  | InvalidFormat [row]-- some cells can't be parsed
  | InvalidData [Text] [row]-- each row is well formatted but invalid as a whole
  | ParsingCorrect result -- Ok

deriving instance (Eq a, Eq b) => Eq (ParsingResult a b)
deriving instance (Show a, Show b) => Show (ParsingResult a b)

type family NotEq a b where
  NotEq a a = 'True
  NotEq a b = 'False

type family UnMaybe a where
  UnMaybe  (Maybe a) = a
  UnMaybe  a = a 

data Null a = Null
instance Functor Null where
  fmap _ Null =  Null

instance Applicative Null where
  pure _ = Null
  _ <*> _ = Null

instance Num (Null a) where
  fromInteger _ = Null
  Null + Null = Null
  Null - Null = Null
  Null * Null = Null
  negate Null = Null
  abs Null = Null
  signum Null = Null
  
type family UnIdentity a where
  UnIdentity (Identity a) = a
  UnIdentity (Null a) = ()
  UnIdentity a = a
  
type FieldForRaw a  = Either InvalidField (Maybe (ValidField (UnMaybe a)))
type FieldForPartial a = (Maybe (ValidField (UnMaybe a)))
type FieldForFinal a = a

type family FieldTF (s :: RowTypes) a where
  FieldTF 'RawT (Maybe a) = Either InvalidField (Maybe (ValidField a))
  FieldTF 'RawT a = Either InvalidField (Maybe (ValidField a))
  FieldTF 'PartialT (Maybe a) = (Maybe (ValidField a))
  FieldTF 'PartialT a = (Maybe (ValidField a))
  FieldTF 'ValidT () = ()
  FieldTF 'ValidT (Maybe a) = Maybe (ValidField a)
  FieldTF 'ValidT a = ValidField a
  FieldTF 'FinalT a = a
 
invalidFieldError :: InvalidField -> Text
invalidFieldError ParsingError{..} = "Can't parse '"
                                     <> invFieldValue
                                     <> "' as "
                                     <> invFieldType
                                     <> "."
invalidFieldError MissingValueError{..} = invFieldType <> " is missing."
invalidFieldError InvalidValueError{..} = invFieldError

type FieldForValid a = FieldTF 'ValidT a

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
pattern RJust :: a -> Either e (Maybe a)
pattern RJust x = Right (Just x)

pattern RNothing :: Either e (Maybe a)
pattern RNothing = Right Nothing

pattern RRight :: a -> Either e (Either e' a)
pattern RRight x = Right (Right x)

pattern RLeft :: e' -> Either e (Either e' a)
pattern RLeft x = Right (Left x)

pattern LRight :: a -> Either (Either e' a) b
pattern LRight x = Left (Right x)

pattern LLeft :: e' -> Either (Either e' a) b
pattern LLeft x = Left (Left x)

-- * Transformables
-- | Sort of convert but can loose information
-- Used to demote valid rows to invalid ones.
class Transformable a b where
  transform :: a -> b

instance Transformable a () where
  transform = const ()

instance  {-# OVERLAPPABLE #-} (a~b) => Transformable a b where
  transform x =  x

instance Applicative f => Transformable a (f a) where
  transform = pure

instance Alternative f => Transformable () (f a) where
  transform = const empty

instance Transformable (ValidField a) a where
  transform v = validValue v
 
instance Transformable a (ValidField a) where
  transform x = Provided x


instance Transformable a b => Transformable [a] [b] where
  transform x = map transform x

instance Transformable b (Either e (Maybe b))  where
  transform x = Right (Just x)

instance Transformable a b => Transformable (Maybe a) (Maybe b) where
  transform x = map transform x
-- * Functions

parseInvalidSpreadsheet :: Show a
  => Csv.DecodeOptions
  -> Map String [String]
  -> LazyBS.ByteString
  -> a
  -> InvalidSpreadsheet
parseInvalidSpreadsheet opt columnMap bytes err =
  let columns = fromString <$> keys columnMap 
      decoded = toList <$> Csv.decodeWith opt Csv.NoHeader bytes :: Either String [[Either Csv.Field Text]]
      onEmpty = InvalidSpreadsheet "The file is empty" columns [] []
  in case (null bytes, decoded) of
       (True, _ )  -> onEmpty
       (_, Right [])  -> onEmpty
       (False, Left err1) -> InvalidSpreadsheet ("Can't parse file. Please check the file is encoded in UTF8 or is well formatted." <> pack err1) columns [] [[Left (toStrict bytes)]]
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

-- | Helper to parse a field given different row names
-- equivalent to m `parse` a <|> m `parse` b
parseMulti ::
  (Functor t, Ord k, Foldable t, Csv.FromField a) =>
  Map k (t String)
  -> Csv.NamedRecord -> k -> Csv.Parser (Either InvalidField a)
parseMulti columnMap m colname =  do
            let Just colnames'' = (Map.lookup colname columnMap)
                colnames = fromString <$> colnames''
                mts = map (m Csv..:) colnames
                mts' = asum $ map (m Csv..:) colnames
            -- let types = mts :: [Csv.Parser Text]
            t <- asum mts
            res <-  toError t <$>  mts'
            -- return $ trace (show (colname, t, res )) res
            return res

-- | Parse a spread sheet 
parseSpreadsheet :: (Csv.FromNamedRecord a, Show a)
                 => Map String [String] -- ^ Columns (what to display, possible names)
                 ->  Maybe String -- ^ separtors
                 -> ByteString
                 -> Either InvalidSpreadsheet [a]
parseSpreadsheet columnMap seps bytes = do
  let lbytes = fromStrict bytes
      options = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- fromMaybe ",;\t" seps ]
      tries =  [(Csv.decodeByNameWith opt lbytes, opt) | opt <- options ]
  case asum (map fst tries) of
    Left _ -> let
                invalids = [parseInvalidSpreadsheet opt columnMap lbytes err | (Left err, opt) <- tries ]
              in  Left $ minimumByEx (comparing $ length . missingColumns) invalids
    Right (__header, vector) ->  Right $ toList vector
  

validateNonEmpty :: Text -> Either InvalidField (Maybe a) -> Either InvalidField (Maybe a)
validateNonEmpty field RNothing = Left (MissingValueError field) 
validateNonEmpty __field v = v

expandColumnName :: String -> [String]
expandColumnName colname = [ id
                           , unwords . map capitalize . words
                           , map Data.Char.toUpper
                           , map Data.Char.toLower
                           ] <*> [colname]

-- | Builds a Map of column name to aliases compatible with parseSpreadsheet
buildColumnMap :: [(String, [String])] -> Map String [String]
buildColumnMap columnNames = Map.fromList [(col, expand (col:cols)) | (col, cols) <- columnNames]
  where expand = nub . sort . (concatMap expandColumnName) 

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = Data.Char.toUpper x : map Data.Char.toLower xs

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

-- | Like Maybe but needs to be explicitly set to Unknown using "?"
data Known a = Unknown | Known a  deriving (Read, Show, Eq, Ord)

instance Csv.FromField a => Csv.FromField (Known a) where
  parseField "?" = return Unknown
  parseField bs = do
    val <- Csv.parseField bs
    return $ Known val

instance Transformable (Maybe a) (Known a) where
  transform x = maybe Unknown Known x

instance Transformable (Known a) (Maybe a) where
  transform Unknown = Nothing
  transform (Known x) = Just x

unknow :: Known a -> Maybe a
unknow = transform

-- | Generate a Either InvalidField from a cell value and parsed field.
toError :: Text -> Either Csv.Field a -> Either InvalidField a
toError t e = case e of
  Left err -> Left (ParsingError ("Invalid format:" <> decodeUtf8 err) t)
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
    toWidget (invFieldToHtml invField)
    invFieldEmptyWidget

invFieldToHtml invField = 
    let (class_, value) = case invField of
          ParsingError _ v -> ("parsing-error" :: Text, v)
          MissingValueError _ -> ("missing-value" :: Text,"<Empty>")
          InvalidValueError e v -> ("invalid-value e" <> e :: Text, v)
    in [shamlet|
<span class="#{class_}">
  <span.description>#{invalidFieldError invField}
  <span.message.text-danger data-toggle="tooltip" title="#{invalidFieldError invField}">#{value}
|]


-- | Basic css and js for InvalidField
invFieldEmptyWidget =  do
    toWidget [cassius|
.parsing-error, .missing-value, .invalid-value
  .description
     display:none
.missing-value
  .message
    font-style: italic
|]
    toWidget [julius|
$('[data-toggle="tooltip"]').tooltip();
|]
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"

instance Renderable a => Renderable (ValidField a) where
  render (Provided x) = render x
  render (Guessed x) = do
    toWidget [cassius|
span.guessed-value
  color:blue
|]
    [whamlet|
<span.guessed-value>^{render x}
|]

instance Renderable a => Renderable (Known a) where
  render (Known a) = render a
  render Unknown = do
    toWidget [cassius|
span.unknown-value
  color:orange
|]
    [whamlet|<span.unknown-value>?|]

instance Renderable InvalidSpreadsheet where
  render InvalidSpreadsheet{..} = let
    colClass = go 0 (sort columnIndexes)
    go :: Int -> [Int]-> [Text]
    go i [] = "" : go i []
    go i (ix:ixs) | ix == i = "bg-success" : go (i+1) ixs
                  | otherwise = "" : go (i+1) (ix:ixs)
    convertField_ :: Either Csv.Field Text -> (Text, Text)
    convertField_ (Left bs) = ("bg-danger text-danger", decodeUtf8 bs)
    convertField_ (Right t) = ("", t)
    in  {-trace ("toHtml" ++ show i ) -}[whamlet|
<div .invalid-receipt>
  <div .error-description> #{errorDescription}
  $if not (null missingColumns)
    <div.panel.panel-danger.missing-columns>
      <div.panel-heading>
        The following columns are missing:
      <ul>
        $forall column <- missingColumns
          <li> #{column}
  <table.sheet.table.table-bordered>
    <thead>
      $forall line <- take 1 sheet
        <tr data-toggle="collapse" data-target="#invalid-spreadsheet-body">
          $forall (class_, field) <- zip colClass line
            $with (fieldClass, fieldValue) <- convertField_ field
              <th class="#{class_} #{fieldClass}"> #{fieldValue}
    <tbody#invalid-spreadsheet-body.collapse.in>
      $forall line <- drop 1 sheet
        <tr>
          $forall (class_, field) <- zip colClass line
            $with (fieldClass, fieldValue) <- convertField_ field
              <td class="#{class_} #{fieldClass}"> #{fieldValue}
          |]
           
             

renderParsingResult :: (Renderable [row], MonadHandler m)
                    => (m () -> Widget -> out)
                    -> (result -> out)
                    -> ParsingResult row result
                    -> out
renderParsingResult onError onSuccess result = 
        case result of
          WrongHeader invalid -> onError (setError "Invalid file or columns missing") (render invalid)
          InvalidFormat raws -> onError (setError "Invalid cell format") (render raws)
          InvalidData errors raws ->  onError (setError (formatErrors errors "Invalid data")) (render raws)
          ParsingCorrect rows -> onSuccess rows
  where formatErrors [] title = title
        formatErrors errors _ = [shamlet|
<ul>
$forall err <- errors
  <li> #{err}
|]


topBorder :: Widget
topBorder = toWidget [cassius|
tr.table-top-border td
  border-top: thin solid black !important
                     |]
