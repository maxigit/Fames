{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GL.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)
import Handler.CsvUtils hiding (RowTypes(..))


-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptHeader s = ReceiptHeader
  { rowDate :: HeaderFieldTF s Text --  RowDateTF s
  , rowCounterparty :: HeaderFieldTF s Text -- RowCounterpartyTF s
  , rowBankAccount :: HeaderFieldTF s Text -- RowBankAccountTF s
  , rowComment :: HeaderFieldTF s (Maybe Text)
  , rowTotal :: HeaderFieldTF s Double
  } 
data ReceiptItem s = ReceiptItem 
  { rowGlAccount :: RowFieldTF s (Maybe Int)-- RowGLAccountTF s
  , rowAmount :: RowFieldTF s (Maybe Double)-- RowAmountTF s
  , rowNetAmount :: RowFieldTF s (Maybe Double)-- RowAmountTF s
  , rowMemo :: RowFieldTF s (Maybe Text)
  , rowTax :: RowFieldTF s (Maybe Text) -- RowTaxTF s
  , rowGLDimension1 :: RowFieldTF s (Maybe Int)
  , rowGLDimension2 :: RowFieldTF s (Maybe Int)
  }

type ReceiptRow s = These (ReceiptHeader s) (ReceiptItem s)
  
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
 
instance {-#OVERLAPPING #-} Show (RawRow) where show = showReceiptRow ValidHeaderT
instance {-#OVERLAPPING #-}Show (ValidHeader) where show = showReceiptRow ValidHeaderT
instance {-#OVERLAPPING #-}Show (InvalidHeader) where show = showReceiptRow InvalidHeaderT
instance {-#OVERLAPPING #-}Show (ValidRow) where show = showReceiptRow ValidRowT
instance {-#OVERLAPPING #-}Show (InvalidRow) where show = showReceiptRow InvalidRowT
         



pattern NonHeaderRow gl amount net memo tax dim1 dim2 = These (ReceiptHeader RNothing RNothing RNothing RNothing RNothing) (ReceiptItem gl amount net memo tax dim1 dim2)
-- pattern RowP gl amount net memo tax dim1 dim2 = These (ReceiptHeader () () () () ()) (ReceiptItem gl amount net memo tax dim1 dim2)
pattern NonItemRow date counterparty bank comment total = These (ReceiptHeader date counterparty bank comment total  ) (ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing)
                                             
showReceiptRow rType row = show "ReceiptRow " ++ show rType ++ show row
-- showReceiptHeader ReceiptHeader{..} = show "ReceiptHeader {"
--   ++ "rowDate=" ++ show rowDate ++ ", " 
--   ++ "rowCounterparty=" ++ show rowCounterparty ++ ", " 
--   ++ "rowBankAccount=" ++ show rowBankAccount ++ ", " 
--   ++ "rowComment=" ++ show rowComment ++ ", " 
--   ++ "rowTotal=" ++ show rowTotal ++ "}" 

-- showReceiptITEM ReceiptItem{..} = show "ReceipItem {"
--   ++ "rowGlAccount=" ++ show rowGlAccount ++ ", " 
--   ++ "rowAmount=" ++ show rowAmount ++ ", " 
--   ++ "rowNetAmount=" ++ show rowNetAmount ++ ", " 
--   ++ "rowTax=" ++ show rowTax ++ "," 
--   ++ "rowGlDimension1=" ++ show rowGLDimension1 ++ ", " 
--   ++ "rowGlDimension2=" ++ show rowGLDimension2 ++ "}" 


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
analyseReceiptRow (These 
                   (ReceiptHeader (RJust date) (RJust counterparty) (RJust bankAccount) (Right comment) (RJust total))
                   (ReceiptItem (Right glAccount) (Right amount) (Right net) (Right memo) (Right tax) (Right dim1) (Right dim2)))
  = Right . Right $ These (ReceiptHeader date counterparty bankAccount comment total)
                          (ReceiptItem glAccount amount net memo tax dim1 dim2)
analyseReceiptRow (NonHeaderRow RNothing RNothing RNothing RNothing RNothing RNothing RNothing)
  -- empty row
  = Left . Left . That $ ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing
analyseReceiptRow (NonHeaderRow (Right glAccount) (Right amount) (Right net) (Right memo) (Right tax) (Right dim1) (Right dim2))
  -- valid row. at least one is non empty
  = Left . Right $  That $ ReceiptItem glAccount amount net memo tax dim1 dim2
analyseReceiptRow (NonHeaderRow glAccount amount net memo tax dim1 dim2)
  -- invalid row
  = Left . Left .  That $ ReceiptItem glAccount amount net memo tax dim1 dim2
analyseReceiptRow (NonItemRow rowDate rowCounterparty rowBankAccount rowComment rowTotal)
  -- invalid header
  = Right. Left . This $ ReceiptHeader  (validateNonEmpty "Date" rowDate)
                                        (validateNonEmpty "Counterparty" rowCounterparty)
                                        (validateNonEmpty "Bank Account" rowBankAccount)
                                        rowComment
                                        (validateNonEmpty "Total" rowTotal)
analyseReceiptRow (These ReceiptHeader{..} ReceiptItem{..})
  -- invalid header
  = Right. Left $ These  (ReceiptHeader (validateNonEmpty "Date" rowDate)
                             (validateNonEmpty "Counterparty" rowCounterparty)
                             (validateNonEmpty "Bank Account" rowBankAccount)
                             rowComment
                             (validateNonEmpty "Total" rowTotal)
                         )
                         (ReceiptItem 
                             rowGlAccount
                             rowAmount
                             rowNetAmount
                             rowMemo
                             rowTax
                             rowGLDimension1
                             rowGLDimension2
                         )
analyseReceiptRow row = error  $ "Which should have These at that point , instead we got " ++ show row

transformRow row  = bimap transformHeader transformItem row
transformHeader ReceiptHeader{..} = ReceiptHeader
  (transform rowDate)
  (transform rowCounterparty)
  (transform rowBankAccount)
  (transform rowComment)
  (transform rowTotal)
transformItem ReceiptItem{..} = ReceiptItem
  (transform rowGlAccount)
  (transform rowAmount)
  (transform rowNetAmount)
  (transform rowMemo)
  (transform rowTax)
  (transform rowGLDimension1)
  (transform rowGLDimension2)
