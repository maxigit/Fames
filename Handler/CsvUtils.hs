-- | Miscellaneous functions and types to parse and render CSV.
module Handler.CsvUtils where

import Import.NoFoundation
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
  
