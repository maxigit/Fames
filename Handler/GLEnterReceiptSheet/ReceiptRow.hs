{-# LANGUAGE TypeFamilies, DataKinds #-}
module Handler.GLEnterReceiptSheet.ReceiptRow where

import Import hiding(InvalidHeader)

-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptRow' s = ReceiptRow'
  { rowDate :: RowDateTF s
  , rowCounterparty :: RowCounterpartyTF s
  , rowBankAccount :: RowBankAccountTF s
  , rowTotal :: RowTotalTF s

  , rowGlAccount :: RowGLAccountTF s
  , rowAmount :: RowAmountTF s
  , rowTax :: RowAmountTF s
  } -- deriving (Read, Show, Eq)

data ReceiptRow 
  = ValidHeader (ReceiptRow' ValidHeaderT)
  | InvalidHeader (ReceiptRow' InvalidHeaderT)
  | ValidRow (ReceiptRow' ValidRowT)
  | InvalidRow (ReceiptRow' InvalidRowT)

data ReceiptRowType
  = Raw
  | ValidHeaderT
  | InvalidHeaderT
  | ValidRowT
  | InvalidRowT
  deriving (Read, Show, Eq)

type ParsingError = Text

type family RowDateTF (s :: ReceiptRowType) where
  RowDateTF Raw = (Maybe Text)
  RowDateTF ValidHeaderT = Text
  RowDateTF InvalidHeaderT = Maybe Text
  RowDateTF InvalidRowT = ()
  RowDateTF ValidRowT = ()

type family RowCounterpartyTF (s :: ReceiptRowType) where
  RowCounterpartyTF Raw = (Maybe Text)
  RowCounterpartyTF ValidHeaderT = Text
  RowCounterpartyTF InvalidHeaderT = Maybe Text
  RowCounterpartyTF ValidRowT = ()
  RowCounterpartyTF InvalidRowT = Text

type family RowBankAccountTF (s :: ReceiptRowType) where
  RowBankAccountTF Raw = (Maybe Text)
  RowBankAccountTF ValidHeaderT = Text
  RowBankAccountTF InvalidHeaderT = Maybe Text
  RowBankAccountTF ValidRowT = ()
  RowBankAccountTF InvalidRowT = Text

type family RowTotalTF (s :: ReceiptRowType) where
  RowTotalTF Raw = (Maybe Double)
  RowTotalTF ValidHeaderT = Double
  RowTotalTF InvalidHeaderT = Maybe Double
  RowTotalTF ValidRowT = ()
  RowTotalTF InvalidRowT = Double

type family RowGLAccountTF (s :: ReceiptRowType) where
  RowGLAccountTF Raw = Int
  RowGLAccountTF ValidHeaderT = Int
  RowGLAccountTF InvalidHeaderT = Int
  RowGLAccountTF ValidRowT = Int
  RowGLAccountTF InvalidRowT = Int

type family RowAmountTF (s :: ReceiptRowType) where
  RowAmountTF Raw = Double
  RowAmountTF ValidHeaderT = Double
  RowAmountTF InvalidHeaderT = Double
  RowAmountTF ValidRowT = Double
  RowAmountTF InvalidRowT = Double

type family RowTaxTF (s :: ReceiptRowType) where
  RowTaxTF Raw = (Maybe Text)
  RowTaxTF ValidHeaderT = Text
  RowTaxTF InvalidHeaderT = Text
  RowTaxTF ValidRowT = Text
  RowTaxTF InvalidRowT = Text

analyseReceiptRow :: ReceiptRow' Raw -> ReceiptRow
analyseReceiptRow (ReceiptRow'{..}) =
  case (rowDate , rowCounterparty , rowBankAccount , rowTotal) of
       ( Just date , Just counterparty , Just bankAccount , Just total)
         -> ValidHeader $ ReceiptRow' date counterparty bankAccount total
                                   rowGlAccount rowAmount rowTax
       (Nothing, Nothing, Nothing, Nothing)
         -> ValidRow $ ReceiptRow' () () () ()
                                   rowGlAccount rowAmount rowTax
       _ -> InvalidHeader $ ReceiptRow' rowDate rowCounterparty rowBankAccount rowTotal
                          rowGlAccount rowAmount rowTax
