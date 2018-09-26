{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GL.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)
import Handler.CsvUtils
import GL.Receipt (ReceiptTemplate(..))


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
  { rowGlAccount :: FieldTF s (Int)-- RowGLAccountTF s
  , rowAmount :: FieldTF s (Double)-- RowAmountTF s
  , rowNetAmount :: FieldTF s (Double)-- RowAmountTF s
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
 
-- instance {-#OVERLAPPING #-} Show (RawRow) where show = showReceiptRow RawT
-- instance {-#OVERLAPPING #-}Show (ValidRow) where show = showReceiptRow ValidT
-- instance {-#OVERLAPPING #-}Show (PartialRow) where show = showReceiptRow PartialT
-- instance {-#OVERLAPPING #-}Show (InvalidRow) where show = showReceiptRow InvalidRowT
         



pattern EmptyItem = ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing
pattern EmptyHeader = ReceiptHeader RNothing RNothing RNothing RNothing RNothing
pattern NonHeaderRow gl amount net memo tax dim1 dim2 = These (ReceiptHeader RNothing RNothing RNothing RNothing RNothing) (ReceiptItem gl amount net memo tax dim1 dim2)
-- pattern RowP gl amount net memo tax dim1 dim2 = These (ReceiptHeader () () () () ()) (ReceiptItem gl amount net memo tax dim1 dim2)
pattern NonItemRow date counterparty bank comment total = These (ReceiptHeader date counterparty bank comment total  ) (ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing)
                                             
-- showReceiptRow rType row = show "ReceiptRow " ++ show rType ++ show row
showReceiptHeader ReceiptHeader{..} = show "ReceiptHeader {"
  ++ "rowDate=" ++ show rowDate ++ ", " 
  ++ "rowCounterparty=" ++ show rowCounterparty ++ ", " 
  ++ "rowBankAccount=" ++ show rowBankAccount ++ ", " 
  ++ "rowComment=" ++ show rowComment ++ ", " 
  ++ "rowTotal=" ++ show rowTotal ++ "}" 

showReceiptItem ReceiptItem{..} = show "ReceipItem {"
  ++ "rowGlAccount=" ++ show rowGlAccount ++ ", " 
  ++ "rowAmount=" ++ show rowAmount ++ ", " 
  ++ "rowNetAmount=" ++ show rowNetAmount ++ ", " 
  ++ "rowTax=" ++ show rowTax ++ "," 
  ++ "rowGlDimension1=" ++ show rowGLDimension1 ++ ", " 
  ++ "rowGlDimension2=" ++ show rowGLDimension2 ++ "}" 

instance Show (ReceiptHeader 'RawT) where show = showReceiptHeader
instance Show (ReceiptItem 'RawT) where show = showReceiptItem
instance Show (ReceiptHeader 'PartialT) where show = showReceiptHeader
instance Show (ReceiptItem 'PartialT) where show = showReceiptItem

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
-- validateHeader header = either (const Nothing) Just $ traverseHeader header

traverseHeader' = ReceiptHeader <$> rowDate
                               <*> rowCounterparty
                               <*> rowBankAccount
                               <*> rowComment
                               <*> rowTotal
validateHeader' :: PartialHeader -> Maybe ValidHeader
validateHeader' (ReceiptHeader (Just rowDate)
                              (Just rowCounterparty)
                              (Just rowBankAccount)
                              (rowComment)
                              (Just rowTotal)
               ) = Just $ ReceiptHeader{..}
validateHeader' _ = Nothing

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

validateItem' :: PartialItem -> Maybe ValidItem
validateItem' (ReceiptItem (Just rowGlAccount)
                          (Just rowAmount)
                          (Just rowNetAmount)
                          (rowMemo)
                          (rowTax)
                          (rowGLDimension1)
                          (rowGLDimension2)
             ) = Just $ ReceiptItem{..}
validateItem' _ = Nothing
              
-- | Create a Raw row with an error or not
invalidateHeader :: RawHeader -> RawHeader
invalidateHeader ReceiptHeader{..} =
  ReceiptHeader (validateNonEmpty "Date" rowDate)
  (validateNonEmpty "Counterparty" rowCounterparty)
  (validateNonEmpty "Bank Account" rowBankAccount)
  rowComment
  (validateNonEmpty "Total" rowTotal) 
invalidateItem :: RawItem -> RawItem
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


validReceipt :: (PartialHeader, [PartialItem])
             -> Either (RawHeader, [RawItem])
                        (ValidHeader, [ValidItem])
validReceipt (header, items) = maybe (Left invalidateAll) Right validate where
  validate = do
    h <- validateHeader' header
    is <- mapM validateItem' items
    return (h, is)
  invalidateAll = (invalidateHeader $ transformHeader header, map (invalidateItem . transformItem) items)
    

flattenReceipt :: (ReceiptHeader t, [ReceiptItem t]) -> [ReceiptRow t]
flattenReceipt (header, []) = [This header]
flattenReceipt (header, i:items) = These header i : map That items



applyTemplate :: ReceiptTemplate -> (PartialHeader, [PartialItem]) -> (PartialHeader, [PartialItem])
applyTemplate setter h'is@(header, items) = case setter of
  (CounterpartySetter counterparty) -> (header {rowCounterparty = addGuess (rowCounterparty header) counterparty}, items)
  (BankAccountSetter bank) -> (header {rowBankAccount =  addGuess (rowBankAccount header) bank}, items)
  (CompoundTemplate []) -> h'is
  (CompoundTemplate (t:ts)) -> applyTemplate (CompoundTemplate ts) (applyTemplate t h'is)
  (ItemMemoSetter memo) -> (header, [i {rowMemo = addGuess (rowMemo i) memo} |i <- items])
  (ItemVATDeducer rate account) -> (header, [deduceVAT rate account i |i <- items])

-- only set Both rate and net together
deduceVAT :: Double -> Text -> PartialItem -> PartialItem
deduceVAT rate account r@ReceiptItem{..} = case (rowTax, rowAmount, rowNetAmount) of
  (Just (Provided _) , _ , _) -> r
  (_,  Just (Provided amount),  _) -> let
      tax = amount * rate 
      net = amount - tax
      in r {rowTax = Just (Guessed account), rowNetAmount = Just (Guessed net)}
  (_, _,  Just (Provided amount)) -> let
      gross =  amount * (1+rate)
      in r {rowTax = Just (Guessed account), rowAmount = Just (Guessed gross)}
  (_,_,_) -> r

addGuess old new  = case old of
  Nothing -> g
  (Just (Guessed _ )) -> g
  p@(Just (Provided _)) -> p
  where  g = Just (Guessed new)
