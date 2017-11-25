{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Boxtake.Upload where

import Import
import Handler.CsvUtils
import Data.List(mapAccumL, (!!))
import Handler.Table
import WH.Barcode (isBarcodeValid)
import qualified Data.Csv as Csv
import Control.Monad.Writer hiding((<>), foldM)
import Data.Text(strip, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either
-- * Type
-- | The raw result of a scanned row
data RawScanRow = RawDate Day
             | RawOperator Text
             | RawLocation Text
             | RawBarcode Text
             deriving (Eq, Read, Show)

data ScanRow = DateRow Day
             | OperatorRow (Entity Operator)
             | LocationRow Text
             | BoxRow (Entity Boxtake)
             deriving (Eq, Show)

-- | The result of a scan
-- No box means the shelf is empty.
data Row = Row
  { rowBoxtake :: Maybe (Entity Boxtake)
  , rowLocation :: Text
  , rowDate :: Day
  , rowOperator :: (Entity Operator)
  } deriving (Eq, Show)

-- | A grouped version of rows by locations
-- There are strict location, not "expandable"
data Session = Session
  { sessionDate :: Day
  , sessionOperator :: Entity Operator
  , sessionLocation :: Text
  , sessionRows :: [Row]
  , sessionMissings :: [Entity Boxtake] -- ^ boxes not present
  }

data StyleMissing = StyleMissing
  { missingStyle :: Text
  , missingBoxes :: [Entity Boxtake] }
-- | If a boxtake has been done on a full shelf, we should
-- be able to inactivate all the box previously on this shelf.
data WipeMode = FullShelves -- ^ Wipe all boxes previously on the scanned location.
              | FullStyles -- ^ Considers the styles as complete. Wipe all previous boxes from the scanned
                  -- styles, regardless of their location.
              | FullStylesAndShelves
              | Addition -- ^ Don't wipe anything. Just add new boxes 
  deriving (Eq, Read, Show, Enum, Bounded)
-- * Util

-- | Reverse op parseRawScan
rawText (RawDate d) = tshow d
rawText (RawOperator o) = o
rawText (RawLocation l) = "LC" <> l
rawText (RawBarcode b) = b

rowVolume row = maybe 0 volume (rowBoxtake row) where
  volume (Entity _ box) = boxVolume box

boxVolume Boxtake{..} = boxtakeLength*boxtakeWidth*boxtakeHeight/1000000

-- * Boxtakes scanning parsing

parseRawScan :: Text -> RawScanRow
parseRawScan line' = let line = strip line' in  case () of
      () | Just location <- stripPrefix "LC" line -> RawLocation location
      () | (isPrefixOf "DL" line || isPrefixOf "ST" line)
           && isBarcodeValid (fromStrict line)  -> RawBarcode line
      () | Just day <- parseDay (unpack line) -> RawDate (allFormatsDay day)
      _ -> RawOperator line

loadRow :: (Text -> Bool) -- ^ Location validator
        -> (Text -> Maybe (Entity Operator)) -- ^ Operator finder
        -> RawScanRow -> Handler (Either InvalidField ScanRow)
loadRow isLocationValid findOperator row = do
  case row of
       RawDate day -> return $ Right (DateRow day)
       RawLocation location -> return $ if isLocationValid location
                          then Right (LocationRow location)
                          else Left (InvalidValueError ("The location " <> location <> " is not valid.")
                                                       location
                                    )
       RawOperator operator -> return $ case findOperator operator of
                                 Just entity -> Right (OperatorRow entity)
                                 Nothing -> Left (InvalidValueError ("Operator " <> operator
                                                                                 <> " doesn't exist. Maybe it is an invalid barcode.")
                                                                    operator
                                                 )
       RawBarcode barcode -> do
            boxM <- runDB $ getBy (UniqueBB barcode)
            return $ case boxM of
                Just box -> Right (BoxRow box)
                Nothing ->  Left (InvalidValueError ("Barcode " <> barcode <> " not found.")
                                                    barcode)

-- parseScan :: Text -> Handler (ParsingResult I-(Either ))

parseScan
  :: WipeMode
  -> ByteString
  -> HandlerT
       App IO (ParsingResult (Either InvalidField ScanRow) ([Session], [StyleMissing]))
parseScan wipeMode spreadsheet = do -- Either
      let rawsE = parseSpreadsheet (mapFromList [("Barcode", [])]) Nothing spreadsheet
      case rawsE of
        Left inv -> return $ WrongHeader inv
        Right raws -> do
          locations <- locationSet 
          findOperator <- operatorFinder
          let isLocationValid loc  = loc `member` locations
          rows <- mapM (loadRow isLocationValid findOperator) raws
          case sequence rows of
            Left _ -> -- there is some error
              return $ InvalidData [] (filter isLeft rows) rows
            Right rights -> do
              let (_, fullEs) = mapAccumL makeRow (Nothing, Nothing, Nothing, Nothing) rights
              case  sequence (catMaybes fullEs) of
                Left _ -> let -- errors, we need to join them with initial rows
                  joinError raw _ (Just (Left e)) = Left (InvalidValueError e (rawText raw))
                  joinError _ row _ = row
                  row'fullS = zipWith3 joinError raws rows fullEs
                  in return $ InvalidData ["Some sections are not complete. Please check that each new scan section has a date an operator and a location"] (filter isLeft row'fullS) row'fullS
                Right fulls ->  do
                  case groupBySession fulls of
                    Left errs -> return $ InvalidData errs [] rows
                    Right sessions0 -> do
                      sessions <- case wipeMode of
                        FullShelves -> loadMissing sessions0
                        FullStyles -> loadMissingFromStyles sessions0
                        FullStylesAndShelves -> loadMissingFromStyleAndShelves sessions0
                        Addition -> return (sessions0, [])
                      return $ ParsingCorrect sessions


-- | Make a row and check that date and operator, and location are provided.
-- Each new operator or date requires and a new operator or date.
-- The last box parameter is used to RESEST the location operartor and date.
-- As they can be set in any order we need to know if a new date (for example)
-- complete a new section or should reset everything
-- Also, a location can be empty. In that case the location needs to be scanned twice in a row.
-- We check that there is no barcode after it.
makeRow :: (Maybe Day, Maybe (Entity Operator), Maybe Text, Maybe (Entity Boxtake)) -- ^ last day, operator , and location and barcode (last box)
        -> ScanRow
        -> ((Maybe Day, Maybe (Entity Operator), Maybe Text, Maybe (Entity Boxtake))
           , Maybe (Either Text Row))
makeRow (daym ,opm, locm, lastbox) row = case (row, lastbox) of
  -- start new section Reset
  (DateRow day, Just _) -> ((Just day, Nothing, Nothing, Nothing), Nothing) -- reset
  (OperatorRow op, Just _) -> ((Nothing, Just op, Nothing, Nothing), Nothing) -- reset
  -- append to new section
  (DateRow day, Nothing) -> ((Just day, opm, locm, Nothing), Nothing) -- append
  (OperatorRow op, Nothing) -> ((daym, Just op, locm, Nothing), Nothing)
  -- check if the same location is called twice
  (LocationRow newLoc, _) | (Just day, Just op, Just loc) <- (daym, opm, locm)
                          , loc == newLoc
                    -- clear the next location so it needs be provide again
                    -> ((daym, opm, Nothing, lastbox), Just $ Right (Row Nothing loc day op))
  (LocationRow loc, lastbox) -> ((daym, opm, Just loc, lastbox), Nothing)
  (BoxRow box, _) | (Just day, Just op, Just loc) <- (daym, opm, locm)
                    -> ((daym, opm, locm, Just box), Just $ Right (Row (Just box) loc day op))
  (BoxRow _  , _)   -> let messages = execWriter $ do
                             tell ["Barcode not expected there."]
                             when (isNothing daym) (tell ["Date is missing."])
                             when (isNothing opm) (tell ["Operator is missing)."])
                             when (isNothing locm) (tell ["Location is missing."])
                           msg = unlines messages
                       in ((daym, opm, locm, Nothing), Just $ Left  msg)

{- Obsolete
-- | Extract all expanded location. As well as a empty one.
-- | An empty location is a location which has been scanned twice in a row.
extractLocations :: [Row] -> Either Text ([Text] , [Text])
extractLocations rows = let
  (emptys, nonEmptys) = partition (isNothing . rowBoxtake) rows
  [fromEmpty, fromNonEmpty] = map (setFromList . map rowLocation) [emptys, nonEmptys] :: [Set Text]
  -- detect if there are collision 
  common = fromEmpty \\ fromNonEmpty
  in case toList common of
    [] -> Right (toList fromNonEmpty, toList fromEmpty)
    commons -> Left $ "The following locations are empty and non empty : " <> intercalate ", " commons
-}
                                        
                                        

groupBySession :: [Row] -> Either [Text] [Session]
groupBySession rows = do -- Either
  let rowByLocs = Map.fromListWith (<>) [(rowLocation row, [row]) | row <- rows]
      sessionEs = map makeSession (toList rowByLocs)
  case sequence sessionEs of
    Left _ -> Left $ lefts sessionEs
    Right sessions -> Right sessions


-- | Create a session from the given rows. Rows are supposed
-- to belong to the same session/shelf , ie same location , date
makeSession :: [Row] -> Either Text Session
makeSession rows = do
  location <- extractUnique "location" id id rowLocation  rows
  date <- extractUnique "date" (tshow) id rowDate rows
    <|&> (<> ("For location" <> location))
  operator <- extractUnique "operator" (operatorNickname . entityVal)  entityKey rowOperator rows
    <|&> (<> ("For location" <> location))
  -- check if shelve is empty or not
  rows' <- case partition (isNothing . rowBoxtake) rows of
    ([], _) -> Right rows
    (_, []) -> Right []
    _ -> Left $ "Shelf " <> location <> " is at the same time empty and contains boxes"

  Right $ Session date operator location rows' []
    
    

-- extractUnique :: Ord b => Text -> (b -> Text) -> (a -> b) -> [a] -> Either Text b
extractUnique fieldname toText toKey field rows = let
  all = Map.fromList $ [(toKey f, f) | r <- rows, let f = field r ]
  in case toList all of
       [unique] -> Right unique
       elems -> Left $ fieldname <> "s are not unique : " <> unlines (map toText elems) 

-- ** Validation
-- * Rendering
instance Renderable [Either InvalidField ScanRow] where
  render rows = do
    displayTable indexes colnameF (map mkRowF rows)
    invFieldEmptyWidget
    where
      indexes = [1]
      colnameF _ = ("Barcode", [])
      mkRowF (Left inv) = (const $ Just (invFieldToHtml inv, []) , ["error"])
      mkRowF (Right row) = case row of
        DateRow day -> (const $ Just (toHtml $ tshow day, []), ["day-row", "bg-info"])
        OperatorRow (Entity _ op) -> (const $ Just (toHtml $ operatorNickname op, []), ["operator-row", "bg-info"])
        LocationRow loc -> (const $ Just (toHtml $ loc, []),["location-row", "bg-success"])
        BoxRow (Entity _ box) -> (const $ Just (toHtml $  boxtakeBarcode box, []), ["box-row"])
      mkRowF _ = (const Nothing, ["error"])


instance Renderable [Row] where
  render rows = renderRows rows

renderRows :: [Row] -> Widget
renderRows rows  =
    displayTableRows indexes colnameF (map mkRowF rows)
    where
      indexes = [1..4]
      colnameF i = map (,[]) ["Barcode", "Description", "Old Location", "New Location"] !! (i-1)
      mkRowF Row{..}  =
        case rowBoxtake of 
          Just (Entity _ Boxtake{..}) -> let
            value 1 = Just . toHtml $ boxtakeBarcode 
            value 2 = Just . toHtml $ fromMaybe "" $ boxtakeDescription
            value 3 = Just . toHtml $ oldLocation
            value 4 = Just . toHtml $ newLocation
            value _ = Nothing
            oldLocation = boxtakeLocation 
            locationSet = Set.fromList $ (splitOn "|" oldLocation)
            inOld = newLocation `elem` locationSet  || oldLocation == "DEF"
            newLocation = rowLocation
            rowClasses = if inOld then [] else ["bg-warning"]
            in (((,[]) <$>) . value, rowClasses )
          Nothing -> let -- empty shelve
            value 1 = Just "∅"
            value 4 = Just . toHtml $ rowLocation
            value _ = Nothing
            in (((,[]) <$>) . value, ["bg-danger"])


-- * Csv
instance Csv.FromNamedRecord RawScanRow where
  parseNamedRecord m = m Csv..: "Barcode" <&> parseRawScan
  

-- * DB
-- | Load boxes from scanned location which have not been moved elsewhere.
barcodeSetFor :: [Session] -> Set Text
barcodeSetFor sessions = let
  boxtakes = mapMaybe rowBoxtake (concatMap sessionRows sessions)
            <> concatMap sessionMissings sessions
  in Set.fromList $ map (boxtakeBarcode . entityVal) boxtakes

-- ** From locations
-- In order to load multiple location (boxtake can have A|B) as a location
-- we need to use a like '%B%' filter. Doing that results on the same
-- boxes being loaded twice for A and B. To avoid this, we expand the barcodeSet
-- with the new loaded boxes
loadMissing :: [Session] -> Handler ([Session], [StyleMissing])
loadMissing sessions = do
  let barcodeSet = barcodeSetFor sessions
  (_, new) <- runDB $ foldM loadMissingForSession (barcodeSet, []) sessions
  return (reverse new, [])


loadMissingForSession :: ((Set Text), [Session]) -> Session -> _ (Set Text, [Session])
loadMissingForSession (barcodes, sessions) session = do
  let location = sessionLocation session
  allboxes <- selectList ( (BoxtakeActive ==. True)
                           :(filterE id BoxtakeLocation (Just $ LikeFilter (location <> "%")))
                         )
                         []
  let missings = filter missing allboxes
      missing (Entity _ Boxtake{..}) = boxtakeBarcode `notElem` barcodes || location `elem` (splitOn "|" boxtakeLocation)

      missingBarcodes = Set.fromList $ map (boxtakeBarcode . entityVal) missings
      newSession = session {sessionMissings = missings}
  return (barcodes <> missingBarcodes, newSession:sessions)
  

-- ** From styles
-- | Loads all boxes from given styles which haven't been moved elsewhere.
-- Create session for each of those styles
loadMissingFromStyles :: [Session] -> Handler ([Session], [StyleMissing])
loadMissingFromStyles sessions = do
  let styles = mconcat (map sessionStyles sessions)
      barcodeSet = barcodeSetFor sessions

  missingMs <- runDB $ mapM (loadMissingFromStyle barcodeSet) (toList styles)
  return $ (sessions, catMaybes missingMs)

sessionStyles :: Session -> Set Text
sessionStyles Session{..} = let
  descriptions = mapMaybe (boxtakeDescription . entityVal <=< rowBoxtake ) sessionRows
  -- TODO: remove hardcoded value
  styles = map (take 8) descriptions
  in Set.fromList styles

loadMissingFromStyle :: Set Text -> Text -> _ (Maybe StyleMissing)
loadMissingFromStyle barcodeSet style = do
  today <- utctDay <$> liftIO getCurrentTime
  allboxes <- selectList ( (BoxtakeActive ==. True)
                           :(filterE Just BoxtakeDescription (Just $ LikeFilter (style <> "%")))
                         )
                         [Asc BoxtakeLocation, Asc BoxtakeDescription]
  let missings = filter missing allboxes
      missing (Entity _ Boxtake{..}) = boxtakeBarcode `notElem` barcodeSet
  return $ case missings of
    [] -> Nothing
    _ -> Just $ StyleMissing style missings

-- ** From styles and shelves
loadMissingFromStyleAndShelves :: [Session] -> Handler ([Session], [StyleMissing])
loadMissingFromStyleAndShelves sessions0 = do
  (sessions1, _) <- loadMissing sessions0
  loadMissingFromStyles sessions1
