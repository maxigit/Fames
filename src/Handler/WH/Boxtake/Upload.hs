{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Boxtake.Upload where

import Import
import Handler.CsvUtils
import Data.List(mapAccumL)
import Handler.Table
import WH.Barcode (isBarcodeValid)
import qualified Data.Csv as Csv
import Control.Monad.Writer hiding((<>))

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

data Row = Row
  { rowBoxtake :: (Entity Boxtake)
  , rowLocation :: Text
  , rowDate :: Day
  , rowOperator :: (Entity Operator)
  } deriving (Eq, Show)
-- * Util

-- | Reverse op parseRawScan
rawText (RawDate d) = tshow d
rawText (RawOperator o) = o
rawText (RawLocation l) = "LC" <> l
rawText (RawBarcode b) = b
-- * Boxtakes scanning parsing

parseRawScan :: Text -> RawScanRow
parseRawScan line = case () of
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

parseScan ::
  ByteString
  -> HandlerT
       App IO (ParsingResult (Either InvalidField ScanRow) [Row])
parseScan spreadsheet = do -- Either
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
              return $ InvalidData [] rows
            Right rights -> do
              let (_, fullEs) = mapAccumL makeRow (Nothing, Nothing, Nothing, Nothing) rights
              case  sequence (catMaybes fullEs) of
                Left _ -> let -- errors, we need to join them with initial rows
                  joinError raw _ (Just (Left e)) = Left (InvalidValueError e (rawText raw))
                  joinError _ row _ = row
                  row'fullS = zipWith3 joinError raws rows fullEs
                  in return $ InvalidData ["Some sections are not complete. Please check that each new scan section has a date an operator and a location"] row'fullS
                Right fulls ->  do
                  return $ ParsingCorrect fulls


-- | Make a row and check that date and operator, and location are provided.
-- Each new operator or date requires and a new operator or date.
-- The last box parameter is used to RESEST the location operartor and date.
-- As they can be set in any order we need to know if a new date (for example)
-- complete a new section or should reset everything
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
  (LocationRow loc, lastbox) -> ((daym, opm, Just loc, lastbox), Nothing)
  (BoxRow box, _) | (Just day, Just op, Just loc) <- (daym, opm, locm)
                    -> ((daym, opm, locm, Just box), Just $ Right (Row box loc day op))
  (BoxRow _  , _)   -> let messages = execWriter $ do
                             tell ["Barcode not expected there."]
                             when (isNothing daym) (tell ["Date is missing."])
                             when (isNothing opm) (tell ["Operator is missing)."])
                             when (isNothing locm) (tell ["Location is missing."])
                           msg = unlines messages
                       in ((daym, opm, locm, Nothing), Just $ Left  msg)

-- ** Validation
-- We need to validate that the file starts with a date an, a operator and a location.
startData :: [ScanRow] -> Maybe (Day, Entity Operator, Text)
startData rows = case sortBy (comparing cons) (take 3 rows) of
   [DateRow start, OperatorRow op, LocationRow loc] -> Just (start, op, loc)
   _ -> Nothing
  where cons (DateRow _) = 1
        cons (OperatorRow _) = 2
        cons (LocationRow _) = 3
        cons _ = 4





-- ** Rendering
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


-- instance Renderable [Row] where
--   render rows = do
--     displayTable indexes colnameF (map mkRowF rows)
--     where
--       indexes = [1]
--       colnameF _ = [""]
--       mkRowF (Left inv) = (const $ Just (invFieldToHtml inv, []) , ["error"])
--       mkRowF (Right row) = case row of
--         DateRow day -> (const $ Just (toHtml $ tshow day, []), ["day-row", "bg-info"])
--         OperatorRow (Entity _ op) -> (const $ Just (toHtml $ operatorNickname op, []), ["operator-row", "bg-info"])
--         LocationRow loc -> (const $ Just (toHtml $ loc, []),["location-row", "bg-success"])
--         BoxRow (Entity _ box) -> (const $ Just (toHtml $  boxtakeBarcode box, []), ["box-row"])
--       mkRowF _ = (const Nothing, ["error"])


-- ** Csv
instance Csv.FromNamedRecord RawScanRow where
  parseNamedRecord m = m Csv..: "Barcode" <&> parseRawScan
