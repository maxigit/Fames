{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)
import Handler.CsvUtils


-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptRow s = ReceiptRow
  { rowDate :: HeaderFieldTF s Text --  RowDateTF s
  , rowCounterparty :: HeaderFieldTF s Text -- RowCounterpartyTF s
  , rowBankAccount :: HeaderFieldTF s Text -- RowBankAccountTF s
  , rowComment :: HeaderFieldTF s (Maybe Text)
  , rowTotal :: HeaderFieldTF s Double

  , rowGlAccount :: RowFieldTF s (Maybe Int)-- RowGLAccountTF s
  , rowAmount :: RowFieldTF s (Maybe Double)-- RowAmountTF s
  , rowNetAmount :: RowFieldTF s (Maybe Double)-- RowAmountTF s
  , rowMemo :: RowFieldTF s (Maybe Text)
  , rowTax :: RowFieldTF s (Maybe Text) -- RowTaxTF s
  , rowGLDimension1 :: RowFieldTF s (Maybe Int)
  , rowGLDimension2 :: RowFieldTF s (Maybe Int)
  } -- deriving (Read, Show, Eq)

  
data ReceiptRowType
  = RawT
  | ValidHeaderT
  | InvalidHeaderT
  | ValidRowT
  | InvalidRowT
  deriving (Read, Show, Eq)

type RawRow = ReceiptRow 'RawT
type ValidHeader = ReceiptRow 'ValidHeaderT
type InvalidHeader = ReceiptRow 'InvalidHeaderT
type ValidRow = ReceiptRow 'ValidRowT
type InvalidRow = ReceiptRow 'InvalidRowT
 
instance Show (RawRow) where show = showReceiptRow ValidHeaderT
instance Show (ValidHeader) where show = showReceiptRow ValidHeaderT
instance Show (InvalidHeader) where show = showReceiptRow InvalidHeaderT
instance Show (ValidRow) where show = showReceiptRow ValidRowT
instance Show (InvalidRow) where show = showReceiptRow InvalidRowT
         


pattern NonHeaderRow gl amount net memo tax dim1 dim2 = ReceiptRow RNothing RNothing RNothing RNothing RNothing gl amount net memo tax dim1 dim2
pattern RowP gl amount net memo tax dim1 dim2 = ReceiptRow () () () () () gl amount net memo tax dim1 dim2
                                             
showReceiptRow rType ReceiptRow{..} = show "ReceiptRow " ++ show rType ++ " {"
  ++ "rowDate=" ++ show rowDate ++ ", " 
  ++ "rowCounterparty=" ++ show rowCounterparty ++ ", " 
  ++ "rowBankAccount=" ++ show rowBankAccount ++ ", " 
  ++ "rowComment=" ++ show rowComment ++ ", " 
  ++ "rowTotal=" ++ show rowTotal ++ ", " 
  ++ "rowGlAccount=" ++ show rowGlAccount ++ ", " 
  ++ "rowAmount=" ++ show rowAmount ++ ", " 
  ++ "rowNetAmount=" ++ show rowNetAmount ++ ", " 
  ++ "rowTax=" ++ show rowTax ++ "," 
  ++ "rowGlDimension1=" ++ show rowGLDimension1 ++ ", " 
  ++ "rowGlDimension2=" ++ show rowGLDimension2 ++ "}" 


class ReceiptRowTypeClass a where
  rowType :: a -> ReceiptRowType

instance ReceiptRowTypeClass ValidHeader where rowType = const ValidHeaderT
instance ReceiptRowTypeClass InvalidHeader where rowType = const InvalidHeaderT
instance ReceiptRowTypeClass ValidRow where rowType = const ValidRowT
instance ReceiptRowTypeClass InvalidRow where rowType = const InvalidRowT
instance (ReceiptRowTypeClass l, ReceiptRowTypeClass r) => ReceiptRowTypeClass (Either l r) where
  rowType = either rowType rowType

type family HeaderFieldTF (s :: ReceiptRowType) a where
  HeaderFieldTF 'RawT  (Maybe a) = Either InvalidField (Maybe a)
  HeaderFieldTF 'RawT  a = Either InvalidField (Maybe a)
  HeaderFieldTF 'ValidHeaderT a = a
  HeaderFieldTF 'InvalidHeaderT (Maybe a) = Either InvalidField (Maybe a)
  HeaderFieldTF 'InvalidHeaderT a = Either InvalidField (Maybe a)
  HeaderFieldTF 'ValidRowT a = ()
  HeaderFieldTF 'InvalidRowT a = ()

type family RowFieldTF (s :: ReceiptRowType) a where
  RowFieldTF 'RawT  (Maybe a) = Either InvalidField (Maybe a)
  RowFieldTF 'RawT  a = Either InvalidField (Maybe a)
  RowFieldTF 'ValidHeaderT a = a
  RowFieldTF 'InvalidHeaderT (Maybe a) = Either InvalidField (Maybe a)
  RowFieldTF 'InvalidHeaderT a = Either InvalidField (Maybe a)
  RowFieldTF 'ValidRowT a = a
  RowFieldTF 'InvalidRowT (Maybe a) = Either InvalidField (Maybe a)
  RowFieldTF 'InvalidRowT a = Either InvalidField (Maybe a)
  
-- What are invalid header ? with valid header field but missing  or invalid header field
analyseReceiptRow :: RawRow -> Either (Either InvalidRow ValidRow)
                                      (Either InvalidHeader ValidHeader)
analyseReceiptRow (ReceiptRow
                   (RJust date) (RJust counterparty) (RJust bankAccount) (Right comment) (RJust total)
                   (Right glAccount) (Right amount) (Right net) (Right memo) (Right tax) (Right dim1) (Right dim2))
  = Right . Right $ ReceiptRow date counterparty bankAccount comment total
                                   glAccount amount net memo tax dim1 dim2
analyseReceiptRow (NonHeaderRow RNothing RNothing RNothing RNothing RNothing RNothing RNothing)
  -- empty row
  = Left . Left $ RowP RNothing RNothing RNothing RNothing RNothing RNothing RNothing
analyseReceiptRow (NonHeaderRow (Right glAccount) (Right amount) (Right net) (Right memo) (Right tax) (Right dim1) (Right dim2))
  -- valid row. at least one is non empty
  = Left . Right $  RowP glAccount amount net memo tax dim1 dim2
analyseReceiptRow (NonHeaderRow glAccount amount net memo tax dim1 dim2)
  -- invalid row
  = Left . Left $  RowP glAccount amount net memo tax dim1 dim2
analyseReceiptRow ReceiptRow{..}
  -- invalid header
  = Right. Left $ ReceiptRow (validateNonEmpty "Date" rowDate)
                             (validateNonEmpty "Counterparty" rowCounterparty)
                             (validateNonEmpty "Bank Account" rowBankAccount)
                             rowComment
                             (validateNonEmpty "Total" rowTotal)
                             rowGlAccount
                             rowAmount
                             rowNetAmount
                             rowMemo
                             rowTax
                             rowGLDimension1
                             rowGLDimension2

  where row = ReceiptRow () () () () () () ()

type family UnMaybe a where
  UnMaybe (Maybe a) = a
  UnMaybe a = a
  
type family NotEq a b where
  NotEq a a = 'True
  NotEq a b = 'False

transformRow ReceiptRow{..} = ReceiptRow
  (transform rowDate)
  (transform rowCounterparty)
  (transform rowBankAccount)
  (transform rowComment)
  (transform rowTotal)
  (transform rowGlAccount)
  (transform rowAmount)
  (transform rowNetAmount)
  (transform rowMemo)
  (transform rowTax)
  (transform rowGLDimension1)
  (transform rowGLDimension2)
