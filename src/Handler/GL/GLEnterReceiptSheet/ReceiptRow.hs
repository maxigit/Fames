{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GL.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)
import Handler.CsvUtils


-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptHeader s = ReceiptHeader
  { rowDate :: FieldTF s Day --  RowDateTF s
  , rowCounterparty :: FieldTF s Text -- RowCounterpartyTF s
  , rowBankAccount :: FieldTF s Text -- RowBankAccountTF s
  , rowComment :: FieldTF s (Maybe Text)
  , rowTotal :: FieldTF s Double
  } 
data ReceiptItem s = ReceiptItem 
  { rowGlAccount :: FieldTF s (Maybe Int)-- RowGLAccountTF s
  , rowAmount :: FieldTF s (Maybe Double)-- RowAmountTF s
  , rowNetAmount :: FieldTF s (Maybe Double)-- RowAmountTF s
  , rowMemo :: FieldTF s (Maybe Text)
  , rowTax :: FieldTF s (Maybe Text) -- RowTaxTF s
  , rowGLDimension1 :: FieldTF s (Maybe Int)
  , rowGLDimension2 :: FieldTF s (Maybe Int)
  }

type ReceiptRow s = These (ReceiptHeader s) (ReceiptItem s)
  
type RawRow = ReceiptRow 'RawT
type ValidRow = ReceiptRow 'ValidT
type PartialRow = ReceiptRow 'PartialT
-- type InvalidRow = ReceiptRow 'InvalidT
type ValidHeader = ReceiptHeader 'ValidT
type InvalidHeader = ReceiptHeader 'RawT
type RawHeader = ReceiptHeader 'RawT
type PartialHeader = ReceiptHeader 'PartialT
type ValidItem = ReceiptItem 'ValidT
type InvalidItem = ReceiptItem 'RawT
type RawItem = ReceiptItem 'RawT
type PartialItem = ReceiptItem 'PartialT
 
instance {-#OVERLAPPING #-} Show (RawRow) where show = showReceiptRow RawT
instance {-#OVERLAPPING #-}Show (ValidRow) where show = showReceiptRow ValidT
-- instance {-#OVERLAPPING #-}Show (InvalidRow) where show = showReceiptRow InvalidRowT
         



pattern EmptyItem = ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing
pattern EmptyHeader = ReceiptHeader RNothing RNothing RNothing RNothing RNothing
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
  rowType :: a -> RowTypes

instance ReceiptRowTypeClass ValidRow where rowType = const ValidT
-- instance ReceiptRowTypeClass InvalidRow where rowType = const InvalidRowT
instance (ReceiptRowTypeClass l, ReceiptRowTypeClass r) => ReceiptRowTypeClass (Either l r) where
  rowType = either rowType rowType

  
-- What are invalid header ? with valid header field but missing  or invalid header field
analyseReceiptRow :: RawRow -> (Either RawRow PartialRow)
analyseReceiptRow h@(This header) = maybe (Left h) (Right . This) (validateHeader header)
analyseReceiptRow i@(That item) = maybe (Left i) (Right . That) (validateItem item)
analyseReceiptRow t@(These header item) = case (validateHeader header, validateItem item) of
  (Just h, Just i) -> Right (These h i)
  _ -> Left t

-- | Check that all fields have been parsed correctly. Blank are allowed
validateHeader :: RawHeader -> Maybe PartialHeader
validateHeader (ReceiptHeader (Right rowDate)
                              (Right rowCounterparty)
                              (Right rowBankAccount)
                              (Right rowComment)
                              (Right rowTotal)
               ) = Just $ ReceiptHeader{..}
validateHeader _ = Nothing

validateItem :: RawItem -> Maybe PartialItem
validateItem (ReceiptItem (Right rowGlAccount)
                          (Right rowAmount)
                          (Right rowNetAmount)
                          (Right rowMemo)
                          (Right rowTax)
                          (Right rowGLDimension1)
                          (Right rowGLDimension2)
             ) = Just $ ReceiptItem{..}
validateItem _ = Nothing
              
-- | Create a Raw row with an error or not
invalidateHeader ReceiptHeader{..} =
  ReceiptHeader (validateNonEmpty "Date" rowDate)
  (validateNonEmpty "Counterparty" rowCounterparty)
  (validateNonEmpty "Bank Account" rowBankAccount)
  rowComment
  (validateNonEmpty "Total" rowTotal) 
invalidateItem ReceiptItem{..} = 
  ReceiptItem 
  (validateNonEmpty "GLAccount" rowGlAccount)
  (validateNonEmpty "Amount" rowAmount)
  (validateNonEmpty "Net Amount" rowNetAmount)
  rowMemo
  rowTax
  rowGLDimension1
  rowGLDimension2

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
