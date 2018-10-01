{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GL.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)
import Handler.CsvUtils
import GL.Receipt (ReceiptTemplateExpanded, ReceiptTemplate'(..))
import Data.List(mapAccumL)
import GL.FA
import Data.Time(toGregorian, fromGregorianValid)

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
  { rowGLAccount :: RefFieldTF s (GLAccountRef)-- RowGLAccountTF s
  , rowAmount :: FieldTF s (Double)-- RowAmountTF s
  , rowNetAmount :: FieldTF s (Double)-- RowAmountTF s
  , rowMemo :: FieldTF s (Maybe Text)
  , rowTax :: RefFieldTF s TaxRef -- RowTaxTF s
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
  ++ "rowGLAccount=" ++ show rowGLAccount ++ ", " 
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
               ) = Just $ ReceiptHeader{..} -- where rowTemplate = Just $ Guessed "pepito"
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

-- | Try to complete date if the only a day of the month has been given
fillRowDate :: Maybe Day -> ReceiptRow 'RawT -> (Maybe Day, ReceiptRow 'RawT)
fillRowDate  previousDay row = case row of
  This header -> This <$> fixDate header
  These header item -> flip These item <$> fixDate header
  That item -> (previousDay , That item)

  where fixDate h@ReceiptHeader{..} = let
          newDate = case (rowDate,  toGregorian <$> previousDay) of
            (Left (ParsingError _ t) , Just (year0, month0, day0)) | Just dayOfMonth <- readMay t -> let
                                                                     (month, year) = if dayOfMonth < day0 -- next month if neeede
                                                                                     then (month0 + 1, year0 + ((fromIntegral month0+1) `div` 12))
                                                                                     else (month0, year0)
                                                                     -- increment month if neede
                                                                     in fromGregorianValid year month dayOfMonth
            (RJust date, _) -> Just $ validValue date
            _ -> previousDay
          in case newDate of
              Nothing  -> (previousDay, h)
              Just d -> (Just d, h {rowDate = Right . Just $ Provided d})

                                          
                                        
-- | Expand if possible the reference. The validity is checked afterward.
-- We need those 2 steps to be able to track easily which expension have gone wrong
expandHeaderRef :: ReferenceMap -> PartialHeader -> PartialHeader
expandHeaderRef refMap ReceiptHeader{..} = ReceiptHeader{rowBankAccount= expandField refMap <$$> rowBankAccount,..}

expandField :: Referable s e => ReferenceMap -> Either Text (Reference s e) -> Either Text (Reference s e)
expandField refMap (Left ref) = maybe (Left ref) Right   $ findReference refMap ref
expandField _ x = x


validateItem :: RawItem -> Maybe PartialItem
validateItem (ReceiptItem (Right rowGLAccount)
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
validateItem' (ReceiptItem (Just rowGLAccount')
                          (Just rowAmount)
                          (Just rowNetAmount)
                          (rowMemo)
                          (Just rowTax')
                          (rowGLDimension1')
                          (rowGLDimension2')
                          (rowItemTemplate)
             )
  | Right rowGLAccount'' <- validValue rowGLAccount'
  , Right rowTax'' <- validValue rowTax'
  , Right rowGLDimension1'' <- traverse sequence rowGLDimension1'
  , Right rowGLDimension2'' <- traverse sequence rowGLDimension2'
  = let rowGLAccount = const rowGLAccount'' <$> rowGLAccount'
        rowTax = const rowTax'' <$> rowTax'
        rowGLDimension1 = rowGLDimension1'' :: Maybe (ValidField (Dimension1Ref)) -- const rowGLDimension1'' <$> rowGLDimension1'
        rowGLDimension2 = rowGLDimension2''  -- const rowGLDimension2'' <$> rowGLDimension2'
  in Just $ ReceiptItem{..}
validateItem' _  = Nothing
              
expandItemRef :: ReferenceMap -> PartialItem -> PartialItem
expandItemRef refMap ReceiptItem{..} = ReceiptItem { rowGLAccount = expandField refMap <$$> rowGLAccount
                                                   , rowTax = expandField refMap <$$> rowTax
                                                   , rowGLDimension1 = expandField refMap <$$> rowGLDimension1
                                                   , rowGLDimension2 = expandField refMap <$$> rowGLDimension2
                                                   , ..
                                                   }
-- | Check if a reference has been expanded properly
validateRef :: Referable s e => ReferenceMap -> Either InvalidField (Maybe (ValidField (Either Text (Reference s e)))) -> Either InvalidField (Maybe (ValidField (Either Text (Reference s e))))
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
  (validateRef refMap $ validateNonEmpty "GLAccount" rowGLAccount)
  (validateNonEmpty "Amount" rowAmount)
  (validateNonEmpty "Net Amount" rowNetAmount)
  rowMemo
  (validateRef refMap rowTax)
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
  (transform rowGLAccount)
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



applyTemplate :: ReceiptTemplateExpanded -> (PartialHeader, [PartialItem]) -> (PartialHeader, [PartialItem])
applyTemplate setter h'is@(header, items) = case setter of
  (CounterpartySetter counterparty) -> (header {rowCounterparty = addGuess (rowCounterparty header) counterparty}, items)
  (BankAccountSetter (Identity bank)) -> (header {rowBankAccount =  addGuess (rowBankAccount header) (Right bank)}, items)
  (BankAccountMapper mapping) -> (mapBankAccount mapping header, items)
  (ItemTaxMapper mapping) -> (header, map (mapTax mapping) items)
  (ItemGLAccountMapper mapping) -> (header, map (mapGLAccount mapping) items)
  (CompoundTemplate []) -> h'is
  (CompoundTemplate (t:ts)) -> applyTemplate (CompoundTemplate ts) (applyTemplate t h'is)
  (ItemMemoSetter memo) -> (header, [i {rowMemo = addGuess (rowMemo i) memo} |i <- items])
  (ItemDimension1Setter (Identity dimension1)) -> (header, [i {rowGLDimension1 = addGuess (rowGLDimension1 i) (Right dimension1)} |i <- items])
  (ItemDimension2Setter (Identity dimension2)) -> (header, [i {rowGLDimension2 = addGuess (rowGLDimension2 i) (Right dimension2)} |i <- items])
  (ItemGLAccountSetter (Identity glAccount)) -> (header, [i {rowGLAccount = addGuess (rowGLAccount i) (Right glAccount)} |i <- items])
  (ItemVATDeducer (Identity tax)) -> (header, [setAndDeduceTax tax i |i <- items])

-- Process all items with header template and then apply individual item template
-- We do it so that item template have priority but also in acse the item template use informatoon
-- from the header. The header could be modified (like updating the total)
applyInnerTemplate :: Map Text ReceiptTemplateExpanded -> (PartialHeader, [PartialItem]) -> (PartialHeader, [PartialItem])
applyInnerTemplate templateMap h'is0@(header0,_) = let
  (header, items) = applyTemplates (rowTemplate header0) templateMap h'is0
  in mapAccumL (applyInnerItemTemplate templateMap ) header items

applyTemplates template templateMap receipt = let
  names = maybeToList template >>= (words . validValue)
  templates = mapMaybe (flip lookup templateMap) names
  in  foldl' (flip applyTemplate) receipt templates 

applyInnerItemTemplate :: Map Text ReceiptTemplateExpanded -> PartialHeader -> PartialItem -> (PartialHeader, PartialItem)
applyInnerItemTemplate templateMap header0 item0 = let
  (header, [item] ) = applyTemplates (rowItemTemplate item0) templateMap  (header0, [item0])
  in (header, item)

setTax :: TaxRef -> PartialItem -> PartialItem
setTax tax item  = item {rowTax = addGuess (rowTax item) (Right tax)}

setAndDeduceTax tax item = deduceVAT (setTax tax item)
-- set tax and compute missing amount
deduceVAT :: PartialItem -> PartialItem
deduceVAT item@ReceiptItem{..} | Just rate' <- rowTax, Right rate'' <- validValue rate' =
   let (rate, _)  = refExtra rate''
   in case (rowNetAmount, rowAmount) of
   (Just net, Nothing) -> item {rowAmount = Just $ Guessed (validValue net * (1+rate)) }
   (Nothing , Just gross) -> item {rowNetAmount = Just $ Guessed (validValue gross / (1+rate)) }
   -- (Just (Provided net), Just (Guessed _)) -> item {rowAmount = Just $ Guessed (net * (1+rate)) }rate
   (Just net, Just (Guessed _)) -> item {rowAmount = Just $ Guessed (validValue net * (1+rate)) }
   (Just (Guessed _) , Just gross) -> item {rowNetAmount = Just $ Guessed (validValue gross / (1+rate)) }
   _ -> item
deduceVAT item = item


mapBankAccount :: Map Text (Identity BankAccountRef) -> PartialHeader -> PartialHeader
mapBankAccount mapping header | Just bankAccount' <- rowBankAccount header
                              , Left bankAccount <- validValue bankAccount'=
  case  lookup bankAccount mapping of
    Nothing -> header
    Just bankRef -> header {rowBankAccount = Just (const (Right (runIdentity bankRef)) <$> bankAccount' )} 

mapBankAccount _ header = header

mapTax :: Map Text (Identity TaxRef) -> PartialItem -> PartialItem
mapTax mapping item | Just taxAccount' <- rowTax item
                              , Left taxAccount <- validValue taxAccount'=
  case  lookup taxAccount mapping of
    Nothing -> item
    Just taxRef -> deduceVAT $ item {rowTax = Just (const (Right (runIdentity taxRef)) <$> taxAccount' )} 

mapTax _ item = item

mapGLAccount :: Map Text (Identity GLAccountRef) -> PartialItem -> PartialItem
mapGLAccount mapping item | Just glAccount' <- rowGLAccount item
                              , Left glAccount <- validValue glAccount'=
  case  lookup glAccount mapping of
    Nothing -> item
    Just glRef -> item {rowGLAccount = Just (const (Right (runIdentity glRef)) <$> glAccount' )} 

mapGLAccount _ item = item


-- deduceVAT taxRef@(_ _ rate account) r@ReceiptItem{..} = case (rowTax, rowAmount, rowNetAmount) of
--   (Just (Provided _) , _ , _) -> r
--   (_,  Just (Provided amount),  _) -> let
--       tax = amount * rate 
--       net = amount - tax
--       in r {rowTax = Just (Provided taxRef), rowNetAmount = Just (Guessed net)}
--   (_, _,  Just (Provided amount)) -> let
--       gross =  amount * (1+rate)
--       in r {rowTax = Just (Provided taxRef), rowAmount = Just (Guessed gross)}
--   (_,_,_) -> r

addGuess :: Maybe (ValidField a) -> a -> Maybe (ValidField a)
addGuess old new  = case old of
  Nothing -> g
  (Just (Guessed _ )) -> g
  p@(Just (Provided _)) -> p
  where  g = Just (Guessed new)


-- * Final 
-- | Validate, once all field are correct that the receipt is consistent (total matchs, tax are correct etc ...)
validateConsistency :: Day -> (ValidHeader, [ValidItem]) -> [Text] 
             -- -> Either (Text, (ValidHeader, [ValidItem]))
             --           (ValidHeader, [ValidItem])
validateConsistency minday r = catMaybes errors
  where errors = validateTotalAmount r
                 : validateDate minday r
                 : map validateItemTax (snd r)

eqDouble :: (Fractional a, Ord a) => a -> a -> Bool
eqDouble a b = abs (a -b) < 1e-2

validateDate minDay (ReceiptHeader{..}, _) =
  if validValue rowDate >= minDay
  then Nothing
  else Just $ "Transaction to old : "   <> tshow rowDate 
validateTotalAmount (ReceiptHeader{..}, items) =
  if totalItems  `eqDouble` validValue rowTotal
  then Nothing
  else Just $ "Amounts doesn't add up. Total : " <> tshow (validValue rowTotal) <> " != from sums: " <> tshow totalItems
  where totalItems = sum (map (validValue . rowAmount) items)

validateItemTax :: ValidItem -> Maybe Text
validateItemTax ReceiptItem{..} =
  -- if (1 + rate ) * net `eqDouble` gross
  if eqDouble ((1 + rate ) * net) gross
  then Nothing
  else Just $ "Gross amount (" <> tshow gross <> ") doesn't match net (" <> tshow net
         <> " for rate of " <> tshow rate
  where rate = fst (refExtra $ validValue rowTax)
        net = validValue rowNetAmount
        gross = validValue rowAmount
                        

                  
  

-- | Round amount to 2 decimals, needed so that, later sum of items (rounded) = of round(sum items)
roundReceipt :: (ValidHeader, [ValidItem]) -> (ValidHeader, [ValidItem])
roundReceipt (ReceiptHeader{..}, items) =
  ( ReceiptHeader{rowTotal=roundTo <$> rowTotal,..}
  , map (\ReceiptItem{..} -> ReceiptItem{ rowAmount= roundTo <$> rowAmount
                                        , rowNetAmount = roundTo <$> rowNetAmount
                                        , ..}) items
  )
  where roundTo x = fromIntegral (round (x *100)) /100
