{-# LANGUAGE TypeFamilies, DataKinds #-}
module Handler.GLEnterReceiptSheet.ReceiptRow where

import Import hiding(InvalidHeader)

-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptRow s = ReceiptRow
  { rowDate :: RowDateTF s
  , rowCounterparty :: RowCounterpartyTF s
  , rowBankAccount :: RowBankAccountTF s
  , rowTotal :: RowTotalTF s

  , rowGlAccount :: RowGLAccountTF s
  , rowAmount :: RowAmountTF s
  , rowTax :: RowAmountTF s
  } -- deriving (Read, Show, Eq)


data ReceiptRowType
  = Raw
  | ValidHeader
  | InvalidHeader
  | ValidRow
  | InvalidRow

type ParsingError = Text

type family RowDateTF (s :: ReceiptRowType) where
  RowDateTF Raw = (Maybe Text)
  RowDateTF ValidHeader = Day
  RowDateTF InvalidHeader = Text
  RowDateTF ValidRow = ()

type family RowCounterpartyTF (s :: ReceiptRowType) where
  RowCounterpartyTF Raw = (Maybe Text)
  RowCounterpartyTF ValidHeader = Text
  RowCounterpartyTF InvalidHeader = Text
  RowCounterpartyTF ValidRow = ()
  RowCounterpartyTF InvalidRow = Text

type family RowBankAccountTF (s :: ReceiptRowType) where
  RowBankAccountTF Raw = (Maybe Text)
  RowBankAccountTF ValidHeader = Text
  RowBankAccountTF InvalidHeader = Text
  RowBankAccountTF ValidRow = ()
  RowBankAccountTF InvalidRow = Text

type family RowTotalTF (s :: ReceiptRowType) where
  RowTotalTF Raw = (Maybe Double)
  RowTotalTF ValidHeader = Double
  RowTotalTF InvalidHeader = Double
  RowTotalTF ValidRow = ()
  RowTotalTF InvalidRow = Double

type family RowGLAccountTF (s :: ReceiptRowType) where
  RowGLAccountTF Raw = Int
  RowGLAccountTF ValidHeader = Int
  RowGLAccountTF InvalidHeader = Int
  RowGLAccountTF ValidRow = Int
  RowGLAccountTF InvalidRow = Int

type family RowAmountTF (s :: ReceiptRowType) where
  RowAmountTF Raw = Double
  RowAmountTF ValidHeader = Double
  RowAmountTF InvalidHeader = Double
  RowAmountTF ValidRow = Double
  RowAmountTF InvalidRow = Double

type family RowTaxTF (s :: ReceiptRowType) where
  RowTaxTF Raw = (Maybe Text)
  RowTaxTF ValidHeader = Text
  RowTaxTF InvalidHeader = Text
  RowTaxTF ValidRow = Text
  RowTaxTF InvalidRow = Text
