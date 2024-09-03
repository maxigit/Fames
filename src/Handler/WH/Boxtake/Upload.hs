{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Handler.WH.Boxtake.Upload 
( parseScan
, renderRows
, saveFromSession
-- * Helper
, rowIsFound
, rowVolume
, boxVolume
, renderBarcode
-- * Types
, Session(..)
, StyleMissing(..)
, WipeMode(..)
-- * For tests
, Row(..)
, ScannedRow(..)
, makeRow
, LastScanned(..)
) where

import Import hiding(all)
import Handler.CsvUtils
import Data.List(mapAccumL, (!!), all, mapAccumL)
import Handler.Table
import WH.Barcode (isBarcodeValid)
import qualified Data.Csv as Csv
import Control.Monad.Writer (tell, execWriter)
import Data.Text(strip, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either
import Handler.WH.Boxtake.Common (extractPosition)
import WarehousePlanner.SimilarBy
-- * Type 
-- | The raw result of a scanned row
data RawScanRow = RawDate Day
             | RawOperator Text
             | RawLocation Text
             | RawBarcode Text
             | RawDepth Int
             | RawOrientation Text
             | RawStrategy Text
             | RawForPlanner Text
             deriving (Eq, Show)

data ScannedRow = DateRow Day
             | OperatorRow (Entity Operator)
             | LocationRow Text
             | BoxRow (Entity Boxtake)
             | DepthRow Int
             | OrientationRow Text
             | StrategyRow Text
             | ForPlannerRow Text
             deriving (Eq, Show)

-- | The result of a scan
-- No box means the shelf is empty.
data Row = Row
  { rowBoxtake :: Maybe (Entity Boxtake)
  , rowLocation :: Text
  , rowDate :: Day
  , rowOperator :: (Entity Operator)
  , rowDepth :: Int
  , rowForPlanner :: [Text]
  } deriving (Eq, Show)

-- | As Row but with maybe
data LastScanned = LastScanned 
  { lastBoxtake :: Maybe (Entity Boxtake)
  , lastLocation :: Maybe Text
  , lastDate :: Maybe Day
  , lastOperator :: Maybe (Entity Operator)
  , lastDepth :: Int
  , lastForPlanner :: [Text]
  } deriving (Eq, Show)

-- | A grouped version of rows by locations
-- There are strict location, not "expandable"
data Session = Session
  { sessionDate :: Day
  , sessionOperator :: Entity Operator
  , sessionLocation :: Text
  , sessionRows :: [Row]
  , sessionMissings :: [Entity Boxtake] -- ^ boxes not present
  , sessionHasPosition :: Bool -- ^ Needed to export to planner 
  }

data RowWithPosition = RowWithPosition
  { row :: Row
  , rowL :: Int
  , rowH :: Int
  } deriving (Eq, Show)

data StyleMissing = StyleMissing
  { missingStyle :: Text
  , missingBoxes :: [Entity Boxtake] }
-- | If a boxtake has been done on a full shelf, we should
-- be able to inactivate all the box previously on this shelf.
data WipeMode = FullShelves --  ^ Wipe all boxes previously on the scanned location.
              | FullStyles --  ^ Considers the styles as complete. Wipe all previous boxes from the scanned
                  -- styles, regardless of their location.
              | FullStylesAndShelves
              | Addition --  ^ Don't wipe anything. Just add new boxes 
              | Deactivate -- deactivate all boxes instead of reactivate them. 
              deriving (Eq, Show, Enum, Bounded)
-- * Util 

-- | Reverse op parseRawScan
rawText :: RawScanRow -> Text
rawText (RawDate d) = tshow d
rawText (RawOperator o) = o
rawText (RawLocation l) = "LC" <> l
rawText (RawBarcode b) = b
rawText (RawDepth d) = "D=" <> tshow d
rawText (RawOrientation o) = "O=" <> tshow o
rawText (RawStrategy s) = "S=" <> tshow s
rawText (RawForPlanner s) = ">>" <> tshow s

rowVolume :: Row -> Double
rowVolume row = maybe 0 volume (rowBoxtake row) where
  volume (Entity _ box) = boxVolume box

boxVolume :: Boxtake -> Double
boxVolume Boxtake{..} = boxtakeLength*boxtakeWidth*boxtakeHeight/1000000

rowIsFound :: Row -> Bool
rowIsFound row = not $ maybe False (boxtakeActive . entityVal) (rowBoxtake row)
-- * Boxtakes scanning parsing 

parseRawScan :: Text -> RawScanRow
parseRawScan line' = let line = strip line' in  case () of
      () | Just location <- stripPrefix "LC" line -> RawLocation location
      () | Just s <- stripPrefix "D=" line, Just depth <- readMay s -> RawDepth depth
      () | Just s <- stripPrefix "O=" line -> RawOrientation s
      () | Just s <- stripPrefix "S=" line -> RawStrategy s
      () | Just s <- stripPrefix ">>" line -> RawForPlanner s
      () | (isPrefixOf "DL" line || isPrefixOf "ST" line)
           && isBarcodeValid (fromStrict line)  -> RawBarcode line
      () | Just day <- parseDay (unpack line) -> RawDate (allFormatsDay day)
      _ -> RawOperator line

loadRow :: (Text -> Bool) --  ^ Location validator
        -> (Text -> Maybe (Entity Operator)) --  ^ Operator finder
        -> RawScanRow -> Handler (Either InvalidField ScannedRow)
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
       RawDepth depth -> return $ Right $ DepthRow depth
       RawOrientation orientation -> return . Right $ OrientationRow orientation
       RawStrategy strategy -> return . Right $ StrategyRow strategy
       RawForPlanner forPlanner -> return . Right $ ForPlannerRow forPlanner
        

-- parseScan :: Text -> Handler (ParsingResult I-(Either ))

parseScan
  :: WipeMode
  -> ByteString
  -> Handler (ParsingResult (Either InvalidField ScannedRow) ([Session], [StyleMissing]))
parseScan wipeMode spreadsheet = do -- Either
      let rawsE = parseSpreadsheet (mapFromList [("Barcode", [])]) Nothing spreadsheet
      case rawsE of
        Left inv -> return $ WrongHeader inv
        Right raws -> do
          locations <- locationSet 
          findOperator <- operatorFinder
          let isLocationValid loc  = 
                if wipeMode == Deactivate 
                then True
                else loc `member` locations
          rows <- mapM (loadRow isLocationValid findOperator) raws
          case sequence rows of
            Left _ -> -- there is some error
              return $ InvalidData [] (filter isLeft rows) rows
            Right rights_ -> do
              let (_, fullEs) = mapAccumL makeRow emptyLastScanned {lastLocation = if wipeMode == Deactivate then Just "LOST" else Nothing} rights_
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
                        Deactivate ->  loadAsMissing  sessions0
                      return $ ParsingCorrect sessions


-- | Make a row and check that date and operator, and location are provided.
-- Each new operator or date requires and a new operator or date.
-- The last box parameter is used to RESEST the location operartor and date.
-- As they can be set in any order we need to know if a new date (for example)
-- complete a new section or should reset everything
-- Also, a location can be empty. In that case the location needs to be scanned twice in a row.
-- We check that there is no barcode after it.
makeRow :: LastScanned --  ^ last day, operator , and location and barcode (last box)
        -> ScannedRow
        -> (LastScanned , Maybe (Either Text Row))
makeRow lastScan row = case (row, lastBoxtake lastScan) of
  -- start new section Reset
  (DateRow day, Just _) -> (emptyLastScanned {lastDate = Just day}, Nothing) -- reset
  (OperatorRow op, Just _) -> (emptyLastScanned {lastOperator = Just op}, Nothing) -- reset
  -- append to new section
  (DateRow day, Nothing) -> (lastScan {lastDate = Just day}, Nothing) -- append
  (OperatorRow op, Nothing) -> (lastScan {lastOperator = Just op}, Nothing)
  -- check if the same location is called twice in a row  with nothing
  -- this means an empty shelf
  (LocationRow newLoc, Nothing) | LastScanned{lastDate=Just day, lastOperator=Just op, lastLocation =Just loc} <- lastScan
                          , loc == newLoc
                    -- clear the next location so it needs be provide again
                    -> ( lastScan {lastLocation = Nothing, lastForPlanner = []}
                       , Just . Right $ Row Nothing loc day op
                                           (lastDepth lastScan)
                                           (lastForPlanner lastScan)
                       )
  (LocationRow loc, _) -> (lastScan {lastLocation = Just loc, lastDepth= 1, lastForPlanner = [] }, Nothing)
  (DepthRow d, _) -> (lastScan { lastDepth = d 
                               , lastForPlanner = lastForPlanner lastScan ++ if d > 1 && d /= lastDepth lastScan
                                                                             then ["NEW", "DEPTH"]
                                                                             else []
                                                                          ++ [ "COMMENT", "depth=" <> tshow d]
                               }
                     , Nothing)
  (OrientationRow or, _) -> (lastScan {lastForPlanner = lastForPlanner lastScan <> [or]}, Nothing)
  (StrategyRow strat, _) -> (lastScan {lastForPlanner = lastForPlanner lastScan <> ["SET STRATEGY", strat]}, Nothing)
  (ForPlannerRow for, _) -> (lastScan {lastForPlanner = lastForPlanner lastScan <> words for}, Nothing)
  (BoxRow box, _) | LastScanned{lastDate=Just day, lastOperator=Just op, lastLocation=Just loc} <- lastScan
                    -> ( lastScan {lastBoxtake = Just box, lastForPlanner = []}
                       , Just . Right $ Row (Just box) loc day op
                                            (lastDepth lastScan)
                                            (lastForPlanner lastScan)
                       )
  (BoxRow _  , _)   -> let messages = execWriter $ do
                             let LastScanned{..} = lastScan
                             tell ["Barcode not expected there."]
                             when (isNothing lastDate) (tell ["Date is missing."])
                             when (isNothing lastOperator) (tell ["Operator is missing)."])
                             when (isNothing lastLocation) (tell ["Location is missing."])
                           msg = unlines messages
                       in (lastScan{lastBoxtake=Nothing}, Just $ Left  msg)

emptyLastScanned :: LastScanned
emptyLastScanned = LastScanned{ lastDate=Nothing
                              , lastBoxtake = Nothing
                              , lastOperator = Nothing
                              , lastLocation = Nothing
                              , lastDepth = 1
                              , lastForPlanner = []
                              }

newtype Location = Location {unLocation :: Text}
  deriving (Show, Eq, Ord)

groupBySession :: [Row] -> Either [Text] [Session]
groupBySession rows = do -- Either
  let rowByLocs = groupSimilar (Location . rowLocation) rows
      sessionEs = map makeSession (toList rowByLocs)
  case sequence sessionEs of
    Left _ -> Left $ lefts sessionEs
    Right sessions -> Right sessions

-- | Create a session from the given rows. Rows are supposed
-- to belong to the same session/shelf , ie same location , date
makeSession :: (SimilarBy Location Row) -> Either Text Session
makeSession sim = do
  let rows_ = unSimilar sim
      -- remove consecutive duplicates in case of double scan
      _rows = removeConsecutiveDuplicates rows_
      -- At the moment we allow duplicate and even triplicate
      -- to show skip
      rows = rows_
      location = unLocation $ similarKey sim
      (woboxes, withboxes) = partition (isNothing . rowBoxtake) rows
  date <- extractUnique "date" (tshow) id rowDate rows
      <|&> (<> ("For location" <> location))
  operator <- extractUnique "operator" (operatorNickname . entityVal)  entityKey rowOperator rows
      <|&> (<> ("For location" <> location))
  -- if position are not required only one set of scans per location should be allowed
  -- check if shelve is empty or not
  (rows', hasPosition) <- case (woboxes, withboxes) of
    ([], wboxes) -> Right $ adjustForPlanner wboxes 
    (_, []) -> Right ([], False)
    _ -> Left $ "Shelf " <> location <> " is at the same time empty and contains boxes"

  Right $ Session date operator location rows' [] hasPosition
  where removeConsecutiveDuplicates = concatMap (take 1) . groupBy ((==) `on` rowBoxtake)
    
    
-- | Add planner export specifif tag, ie new depth , and next column
-- depending on the depth and the box has been scanned twice (indicated first of a column)
adjustForPlanner :: [Row] -> ([Row], Bool)
adjustForPlanner rows = let
  row'counts = processDuplicateRows $ sortOn rowDepth rows
  in if all ((==) 1 . snd) row'counts
     then -- if forPlanner is present return True
          let hasPosition = not . null $ concatMap rowForPlanner rows
          in (rows, hasPosition) 
     else  ( snd $ mapAccumL go Nothing  row'counts
           , True
           )
  where go :: Maybe Row -> (Row, Int) -> (Maybe Row, Row)
        go lastm (row, count) = let 
                  extra = if
                          | depthDiff <- rowDepth row - maybe 1 rowDepth lastm , depthDiff > 0 -> replicate depthDiff "NEW DEPTH"
                          | Just _ <- lastm, count > 1 -> replicate (count - 1) "NEXT COLUMN"
                          | count > 2 -> replicate (count - 2) "NEXT COLUMN"
                          | otherwise -> []
                  in ( Just row
                     , row {rowForPlanner = rowForPlanner row <> extra }
                     )

-- | count similar row and keep only the first occurence
processDuplicateRows :: [Row] -> [(Row, Int)]
processDuplicateRows rows = let
  countMap = Map.fromListWith (+) $ map ((,1) . key) rows
  key = fmap entityKey . rowBoxtake
  keep (toKeep, used) row =
       let k = key row 
       in if k `member` used
          then (toKeep, used)
          else case Map.lookup k countMap of
               Nothing -> error "The unexpected happened. Contact your adminintrator!"
               Just count -> (toKeep <> [(row, count)], Set.insert k used)
  in fst $ foldl' keep ([], mempty) rows
  

-- | extractUnique :: Ord b => Text -> (b -> Text) -> (a -> b) -> [a] -> Either Text b
extractUnique :: (Ord k) => Text -> (b -> Text) -> (b -> k) -> (a -> b) -> [a] -> Either Text b
extractUnique fieldname toText toKey field rows = let
  all = Map.fromList $ [(toKey f, f) | r <- rows, let f = field r ]
  in case toList all of
       [unique] -> Right unique
       elems -> Left $ fieldname <> "s are not unique : " <> unlines (map toText elems) 

-- ** Validation 
-- * Rendering 
instance Renderable [Either InvalidField ScannedRow] where
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
        DepthRow d -> (const $ Just (toHtml $ d, []), ["depth-row", "bg-info"])
        OrientationRow or -> (const $ Just (toHtml $ or, []), ["orientation-row", "bg-info"])
        StrategyRow or -> (const $ Just (toHtml $ or, []), ["orientation-row", "bg-info"])
        ForPlannerRow or -> (const $ Just (toHtml $ or, []), ["orientation-row", "bg-info"])
        BoxRow (Entity _ box) -> (const $ Just (toHtml $  boxtakeBarcode box, []), ["box-row"])


renderBarcode :: (Route App -> [(Text, Text)] -> Text) -> Text -> Html
renderBarcode renderUrl barcode =
  [hamlet|<a href="@{WarehouseR (WHBoxtakeDetailR barcode)}" target=_blank> #{barcode}
         |] renderUrl
renderRows :: _ -> [Row] -> Widget
renderRows renderUrl rows  =
    displayTableRowsAndHeader indexes colnameF (map mkRowF rows)
    where
      indexes = [1..7]
      colnameF i = map (,[]) ["Barcode", "Description", "Old Location", "New Location", "For Planner", "Active", "Date"] !! (i-1)
      mkRowF row@Row{..}  =
        case rowBoxtake of 
          Just (Entity _ Boxtake{..}) -> let
            value 1 = Just ( renderBarcode renderUrl boxtakeBarcode , [])
            value 2 = Just ( toHtml $ fromMaybe "" $ boxtakeDescription, [])
            value 3 = Just ( toHtml $ oldLocation, if inOld then [] else ["text-danger"])
            value 4 = Just ( toHtml $ newLocation, [])
            value 5 =  Just  (toHtml $ intercalate " " rowForPlanner , [])
            value 6 = Just ( toHtml $ boxtakeActive, if rowIsFound row then ["text-danger"] else [])
            value 7 = Just ( toHtml . tshow $ boxtakeDate, [])
            value _ = Nothing
            oldLocation = boxtakeLocation 
            locationSet_ = Set.fromList $ map (fst . extractPosition) $ (splitOn "|" oldLocation)
            inOld = newLocation `elem` locationSet_  || oldLocation == "DEF"
            newLocation = rowLocation
            rowClasses = case (inOld, rowIsFound row ) of
                         (_, True) -> ["bg-info"]
                         (False, _) -> ["bg-warning"]
                         _ -> []
            in ( value, rowClasses )
          Nothing -> let -- empty shelve
            value 1 = Just "∅"
            value 4 = Just . toHtml $ rowLocation
            value _ = Nothing
            in ( ((,[]) <$>) .value, ["bg-danger"])


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
      missing (Entity _ Boxtake{..}) = boxtakeBarcode `notElem` barcodes
        && location `elem` (splitOn "|" boxtakeLocation)
      -- \^ The LIKE filter might result in loading boxes not from the current locations
      -- They need NOT to be shown as missing

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
  descriptions = mapMaybe (boxtakeDescription . entityVal <=< rowBoxtake) sessionRows
  -- TODO: remove hardcoded value
  styles = map (take 8) descriptions
  in Set.fromList styles

loadMissingFromStyle :: Set Text -> Text -> SqlHandler (Maybe StyleMissing)
loadMissingFromStyle barcodeSet style = do
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
  
-- ** Make all boxtake as missing 
loadAsMissing :: [Session] -> Handler ([Session], [StyleMissing])
loadAsMissing sessions =  do
  let barcodeSet = barcodeSetFor sessions
  allBoxes <- runDB $ selectList [BoxtakeBarcode <-. toList barcodeSet] []
  return ([], [StyleMissing "To Deactivate" allBoxes])
-- ** Save boxtake 
-- | Update boxtake to the new location or disable them if missing. wwkk 1k
saveFromSession :: Session -> SqlHandler ()
saveFromSession Session{..} = do
  mapM_ saveLocation (sessionRows)
  mapM_ (setActivateBoxtake False sessionDate) sessionMissings

-- | Save the location even if the location is the same
-- However keep sublocation if new location is in the same bay (so we don't lose sublocation if not given again)
-- Keep position in DB if location (without positon) is the same.
-- it is important to update the date even if the location hasn't changed so that the box
-- is seen has being recently scanned when detected which boxes are alive or not
saveLocation :: Row  -> SqlHandler ()
saveLocation Row{..} = 
  forM_ rowBoxtake $ \boxe@(Entity _ boxtake) -> do
        let (oldLocation, __oldPosM) = extractPosition $ boxtakeLocation boxtake
        when (boxtakeActive boxtake && rowDate >= boxtakeDate boxtake) do
           let location = if oldLocation /= rowLocation 
                          then  rowLocation
                          else boxtakeLocation boxtake
                          -- T
                          -- +-- either oldLocation == new location (: no position)
                          --     or old location == new locatation : old position
           updateBoxtakeLocation location
                                 (entityKey rowOperator)
                                 (rowDate)
                                 boxe
