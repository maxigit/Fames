{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GL.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)
import Handler.CsvUtils
import GL.Receipt (ReceiptTemplate(..))
import Data.List(mapAccumL)
import GL.FA

-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptHeader s = ReceiptHeader
  { rowDate :: FieldTF s Day --  RowDateTF s
  , rowCounterparty :: FieldTF s Text -- RowCounterpartyTF s
  , rowBankAccount :: RefFieldTF s (BankAccountRef) -- RowBankAccountTF s
  , rowComment :: FieldTF s (Maybe Text)
  , rowTotal :: FieldTF s Double
  , rowTemplate :: FieldTF s (Maybe Text) -- template to apply to whole receipt
  } 
data ReceiptItem s = ReceiptItem 
  { rowGlAccount :: RefFieldTF s (GLAccountRef)-- RowGLAccountTF s
  , rowAmount :: FieldTF s (Double)-- RowAmountTF s
  , rowNetAmount :: FieldTF s (Double)-- RowAmountTF s
  , rowMemo :: FieldTF s (Maybe Text)
  , rowTax :: FieldTF s (Maybe Text) -- RowTaxTF s
  , rowGLDimension1 :: RefFieldTF s (Maybe Dimension1Ref)
  , rowGLDimension2 :: RefFieldTF s (Maybe Dimension2Ref)
  , rowItemTemplate :: FieldTF s (Maybe Text) -- template to apply to item only
  }

type RefFieldTF s a = FieldTF s (RefTF s a)
type family RefTF (s :: RowTypes) a where
  RefTF 'RawT (Maybe a) = Either Text a
  RefTF 'RawT a = Either Text a
  RefTF 'PartialT (Maybe a) = Either Text a
  RefTF 'PartialT a = Either Text a
  RefTF 'ValidT a = a
  RefTF 'FinalT a = a

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
         



pattern EmptyItem = ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing RNothing
-- | Check if a header is empty. Don't look at template, as it will be picked up by the item too.
pattern EmptyHeader <- ReceiptHeader RNothing RNothing RNothing RNothing RNothing _
pattern NonHeaderRow gl amount net memo tax dim1 dim2 itemTemplate <- These (ReceiptHeader RNothing RNothing RNothing RNothing RNothing _) (ReceiptItem gl amount net memo tax dim1 dim2 itemTemplate)
-- pattern RowP gl amount net memo tax dim1 dim2 = These (ReceiptHeader () () () () ()) (ReceiptItem gl amount net memo tax dim1 dim2)
pattern NonItemRow date counterparty bank comment total template = These (ReceiptHeader date counterparty bank comment total template  ) (ReceiptItem RNothing RNothing RNothing RNothing RNothing RNothing RNothing RNothing)
                                             
-- showReceiptRow rType row = show "ReceiptRow " ++ show rType ++ show row
showReceiptHeader ReceiptHeader{..} = show "ReceiptHeader {"
  ++ "rowDate=" ++ show rowDate ++ ", " 
  ++ "rowCounterparty=" ++ show rowCounterparty ++ ", " 
  ++ "rowBankAccount=" ++ show rowBankAccount ++ ", " 
  ++ "rowComment=" ++ show rowComment ++ ", " 
  ++ "rowTotal=" ++ show rowTotal ++ "," 
  ++ "rowTemplate=" ++ show rowTemplate ++ "}" 

showReceiptItem ReceiptItem{..} = show "ReceipItem {"
  ++ "rowGlAccount=" ++ show rowGlAccount ++ ", " 
  ++ "rowAmount=" ++ show rowAmount ++ ", " 
  ++ "rowNetAmount=" ++ show rowNetAmount ++ ", " 
  ++ "rowTax=" ++ show rowTax ++ "," 
  ++ "rowGlDimension1=" ++ show rowGLDimension1 ++ ", " 
  ++ "rowGlDimension2=" ++ show rowGLDimension2 ++ "," 
  ++ "rowItemTemplate=" ++ show rowItemTemplate ++ "}" 

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
  (Just h, Just i) -> Right (These h i {rowItemTemplate = Nothing}) -- cancel item template already on header
  _ -> Left t

-- | Check that all fields have been parsed correctly. Blank are allowed
validateHeader :: RawHeader -> Maybe PartialHeader
validateHeader (ReceiptHeader (Right rowDate)
                              (Right rowCounterparty)
                              (Right rowBankAccount)
                              (Right rowComment)
                              (Right rowTotal)
                              (Right rowTemplate)
               ) = Just $ ReceiptHeader{..}
validateHeader _ = Nothing
-- validateHeader header = either (const Nothing) Just $ traverseHeader header

traverseHeader' = ReceiptHeader <$> rowDate
                               <*> rowCounterparty
                               <*> rowBankAccount
                               <*> rowComment
                               <*> rowTotal
                               <*> rowTemplate
validateHeader' :: PartialHeader -> Maybe ValidHeader
validateHeader' (ReceiptHeader (Just rowDate)
                              (Just rowCounterparty)
                              (Just rowBankAccount')
                              (rowComment)
                              (Just rowTotal)
                              (rowTemplate)
               )
  | Right rowBankAccount'' <- validValue rowBankAccount'
  = let rowBankAccount = fmap (const rowBankAccount'') rowBankAccount'
   in Just ReceiptHeader{..}
validateHeader' _ = Nothing

-- | Expand if possible the reference. The validity is checked afterward.
-- We need those 2 steps to be able to track easily which expension have gone wrong
expandHeaderRef :: ReferenceMap -> PartialHeader -> PartialHeader
expandHeaderRef refMap ReceiptHeader{..} = ReceiptHeader{rowBankAccount= expandField refMap <$$> rowBankAccount,..}

expandField :: Referable s => ReferenceMap -> Either Text (Reference s) -> Either Text (Reference s)
expandField refMap (Left ref) = maybe (Left ref) Right   $ findReference refMap ref
expandField _ x = x


validateItem :: RawItem -> Maybe PartialItem
validateItem (ReceiptItem (Right rowGlAccount)
                          (Right rowAmount)
                          (Right rowNetAmount)
                          (Right rowMemo)
                          (Right rowTax)
                          (Right rowGLDimension1)
                          (Right rowGLDimension2)
                          (Right rowItemTemplate)
             ) = Just $ ReceiptItem{..}
validateItem _ = Nothing

validateItem' :: PartialItem -> Maybe ValidItem
validateItem' (ReceiptItem (Just rowGlAccount')
                          (Just rowAmount)
                          (Just rowNetAmount)
                          (rowMemo)
                          (rowTax)
                          (rowGLDimension1')
                          (rowGLDimension2')
                          (rowItemTemplate)
             )
  | Right rowGlAccount'' <- validValue rowGlAccount'
  , Right rowGLDimension1'' <- traverse sequence rowGLDimension1'
  , Right rowGLDimension2'' <- traverse sequence rowGLDimension2'
  = let rowGlAccount = const rowGlAccount'' <$> rowGlAccount'
        rowGLDimension1 = Nothing -- const rowGLDimension1'' <$> rowGLDimension1'
        rowGLDimension2 = Nothing -- const rowGLDimension2'' <$> rowGLDimension2'
  in Just $ ReceiptItem{..}
validateItem' _  = Nothing
              
expandItemRef :: ReferenceMap -> PartialItem -> PartialItem
expandItemRef refMap ReceiptItem{..} = ReceiptItem { rowGlAccount = expandField refMap <$$> rowGlAccount
                                                   , rowGLDimension1 = expandField refMap <$$> rowGLDimension1
                                                   , rowGLDimension2 = expandField refMap <$$> rowGLDimension2
                                                   , ..
                                                   }
-- | Check if a reference has been expanded properly
validateRef :: Referable s => ReferenceMap -> Either InvalidField (Maybe (ValidField (Either Text (Reference s)))) -> Either InvalidField (Maybe (ValidField (Either Text (Reference s))))
validateRef refMap (RJust te)  | Left t <- validValue te= let -- we know the conversion doesn't work but we need to do it again
  -- to get the error message
  ref=  findReferenceEither refMap t
  _types = [te, const ref <$> te ] -- help types inference
  Left err = ref
  in Left (InvalidValueError err t)
validateRef _ x = x

-- | Create a Raw row with an error or not
invalidateHeader :: ReferenceMap ->  RawHeader -> RawHeader
invalidateHeader refMap ReceiptHeader{..} =
  ReceiptHeader (validateNonEmpty "Date" rowDate)
  (validateNonEmpty "Counterparty" rowCounterparty)
  (validateRef refMap $ validateNonEmpty "Bank Account" rowBankAccount)
  rowComment
  (validateNonEmpty "Total" rowTotal) 
  rowTemplate
invalidateItem :: ReferenceMap -> RawItem -> RawItem
invalidateItem refMap ReceiptItem{..} = 
  ReceiptItem 
  (validateRef refMap $ validateNonEmpty "GLAccount" rowGlAccount)
  (validateNonEmpty "Amount" rowAmount)
  (validateNonEmpty "Net Amount" rowNetAmount)
  rowMemo
  rowTax
  (validateRef refMap rowGLDimension1)
  (validateRef refMap rowGLDimension2)
  rowItemTemplate
transformRow row  = bimap transformHeader transformItem row
transformHeader ReceiptHeader{..} = ReceiptHeader
  (transform rowDate)
  (transform rowCounterparty)
  (transform rowBankAccount)
  (transform rowComment)
  (transform rowTotal)
  (transform rowTemplate)
transformItem ReceiptItem{..} = ReceiptItem
  (transform rowGlAccount)
  (transform rowAmount)
  (transform rowNetAmount)
  (transform rowMemo)
  (transform rowTax)
  (transform rowGLDimension1)
  (transform rowGLDimension2)
  (transform rowItemTemplate)


validReceipt :: ReferenceMap
             -> (PartialHeader, [PartialItem])
             -> Either (RawHeader, [RawItem])
                        (ValidHeader, [ValidItem])
validReceipt refMap h'is = maybe (Left invalidateAll) Right validate where
  (header, items) = expandReceiptRef refMap h'is
  validate = do
    h <- validateHeader' header
    is <- mapM (validateItem' ) items
    return (h, is)
  invalidateAll = (invalidateHeader refMap $ transformHeader header, map (invalidateItem refMap . transformItem) items)
    
expandReceiptRef :: ReferenceMap
             -> (PartialHeader, [PartialItem])
             -> (PartialHeader, [PartialItem])
expandReceiptRef refMap (header, items) = (expandHeaderRef refMap header, map (expandItemRef refMap) items )


flattenReceipt :: (ReceiptHeader t, [ReceiptItem t]) -> [ReceiptRow t]
flattenReceipt (header, []) = [This header]
flattenReceipt (header, i:items) = These header i : map That items



applyTemplate :: ReceiptTemplate -> (PartialHeader, [PartialItem]) -> (PartialHeader, [PartialItem])
applyTemplate setter h'is@(header, items) = case setter of
  (CounterpartySetter counterparty) -> (header {rowCounterparty = addGuess (rowCounterparty header) counterparty}, items)
  (BankAccountSetter bank) -> (header {rowBankAccount =  addGuess (rowBankAccount header) (Left bank)}, items)
  (CompoundTemplate []) -> h'is
  (CompoundTemplate (t:ts)) -> applyTemplate (CompoundTemplate ts) (applyTemplate t h'is)
  (ItemMemoSetter memo) -> (header, [i {rowMemo = addGuess (rowMemo i) memo} |i <- items])
  (ItemDimension1Setter dimension1) -> (header, [i {rowGLDimension1 = addGuess (rowGLDimension1 i) (Left dimension1)} |i <- items])
  (ItemDimension2Setter dimension2) -> (header, [i {rowGLDimension2 = addGuess (rowGLDimension2 i) (Left dimension2)} |i <- items])
  (ItemVATDeducer rate account) -> (header, [deduceVAT rate account i |i <- items])

-- Process all items with header template and then apply individual item template
-- We do it so that item template have priority but also in acse the item template use informatoon
-- from the header. The header could be modified (like updating the total)
applyInnerTemplate :: Map Text ReceiptTemplate -> (PartialHeader, [PartialItem]) -> (PartialHeader, [PartialItem])
applyInnerTemplate templateMap h'is0@(header0,_) = let
  (header, items) = case flip lookup templateMap . validValue =<< rowTemplate header0 of
                      Nothing -> h'is0
                      Just template -> applyTemplate template h'is0
  in mapAccumL (applyInnerItemTemplate templateMap ) header items

applyInnerItemTemplate :: Map Text ReceiptTemplate -> PartialHeader -> PartialItem -> (PartialHeader, PartialItem)
applyInnerItemTemplate templateMap header0 item0 = 
    case flip lookup templateMap . validValue =<< rowItemTemplate item0 of
      Nothing -> (header0, item0)
      Just template -> let (header, [item] ) = applyTemplate template (header0, [item0])
                       in (header, item)

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

addGuess :: Maybe (ValidField a) -> a -> Maybe (ValidField a)
addGuess old new  = case old of
  Nothing -> g
  (Just (Guessed _ )) -> g
  p@(Just (Provided _)) -> p
  where  g = Just (Guessed new)

