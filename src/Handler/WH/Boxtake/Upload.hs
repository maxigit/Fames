{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Boxtake.Upload where

import Import
import Handler.CsvUtils
import Data.List(mapAccumR)
import Handler.Table
import qualified Data.Csv as Csv

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

data Row = Row
  { rowBoxtake :: (Entity Boxtake)
  , rowLocation :: Text
  , rowDate :: Day
  , rowOperator :: (Entity Operator)
  }
-- * Boxtakes scanning parsing

parseRawScan :: Text -> RawScanRow
parseRawScan line = case  () of
      () | Just location <- stripPrefix "LC" line -> RawLocation location
      () | isPrefixOf "DL" line || isPrefixOf "ST" line -> RawBarcode line
      () | Just day <- readMay line -> RawDate day
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
                                                                                 <> " doesn't exist.")
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
       App IO (ParsingResult (Either InvalidField ScanRow) [Maybe Row])
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
              case startData rights of
                _ -> return $ InvalidData ["File should set an operator, a date and a location."]rows
                Just start ->  do
                  let (_, fulls) = mapAccumR makeRow start rights
                  return $ ParsingCorrect fulls


makeRow :: (Day, Entity Operator, Text) -> ScanRow -> ((Day, Entity Operator, Text), Maybe Row)
makeRow start@(day0 ,op0, loc0) row = case row of
  DateRow day -> ((day, op0, loc0), Nothing)
  OperatorRow op -> ((day0, op, loc0), Nothing)
  LocationRow loc -> ((day0, op0, loc), Nothing)
  BoxRow box -> (start, Just (Row box loc0 day0 op0))

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
        DateRow day -> (const $ Just (toHtml $ tshow day, []), ["day-row"])
        OperatorRow (Entity _ op) -> (const $ Just (toHtml $ operatorNickname op, []), ["operator-row"])
        LocationRow loc -> (const $ Just (toHtml $ loc, []),["location-row"])
        BoxRow (Entity _ box) -> (const $ Just (toHtml $  boxtakeBarcode box, []), ["box-row"])
      mkRowF _ = (const Nothing, ["error"])


-- ** Csv
instance Csv.FromNamedRecord RawScanRow where
  parseNamedRecord m = m Csv..: "Barcode" <&> parseRawScan
