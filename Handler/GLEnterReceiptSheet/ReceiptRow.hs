module Handler.GLEnterReceiptSheet.ReceiptRow where

import Import

-- Represents a row of the spreadsheet.
data ReceiptRow = ReceiptRow
  { rowDate :: Maybe Text  
  , rowCounterparty :: Maybe Text
  , rowBankAccount :: Maybe Text
  , rowTotal :: Maybe Double
  , rowGlAccount :: Int
  , rowAmount :: Double
  , rowTax :: Text
  } deriving (Read, Show, Eq)

