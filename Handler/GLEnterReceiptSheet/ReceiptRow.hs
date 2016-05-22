{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Handler.GLEnterReceiptSheet.ReceiptRow where


import Import hiding(InvalidHeader)


-- | Represents a row of the spreadsheet.
-- The actual type of each field depend of a status or stage
-- This is needed to model different types of row which have 
-- Which have the same structure but different semantic.
data ReceiptRow s = ReceiptRow
  { rowDate :: HeaderFieldTF s Text --  RowDateTF s
  , rowCounterparty :: HeaderFieldTF s Text -- RowCounterpartyTF s
  , rowBankAccount :: HeaderFieldTF s Text -- RowBankAccountTF s
  , rowTotal :: HeaderFieldTF s Double

  , rowGlAccount :: RowFieldTF s Int -- RowGLAccountTF s
  , rowAmount :: RowFieldTF s Double -- RowAmountTF s
  , rowTax :: RowFieldTF s (Maybe Text) -- RowTaxTF s
  } -- deriving (Read, Show, Eq)

  
data ReceiptRowType
  = RawT
  | ValidHeaderT
  | InvalidHeaderT
  | ValidRowT
  | InvalidRowT
  deriving (Read, Show, Eq)

type RawRow = ReceiptRow RawT
type ValidHeader = ReceiptRow ValidHeaderT
type InvalidHeader = ReceiptRow InvalidHeaderT
type ValidRow = ReceiptRow ValidRowT
type InvalidRow = ReceiptRow InvalidRowT
 
instance Show (ReceiptRow ValidHeaderT) where show = showReceiptRow ValidHeaderT
instance Show (ReceiptRow InvalidHeaderT) where show = showReceiptRow InvalidHeaderT
instance Show (ReceiptRow ValidRowT) where show = showReceiptRow ValidRowT
instance Show (ReceiptRow InvalidRowT) where show = showReceiptRow InvalidRowT
         

pattern RJust x = Right (Just x)
pattern RNothing = Right Nothing
pattern RRight x = Right (Right x)
pattern RLeft x = Right (Left x)
pattern LRight x = Left (Right x)
pattern LLeft x = Left (Left x)

pattern NonHeaderRow gl amount tax = ReceiptRow RNothing RNothing RNothing RNothing gl amount tax
pattern RowP gl amount tax = ReceiptRow () () () () gl amount tax
                                             
showReceiptRow rType ReceiptRow{..} = show "ReceiptRow " ++ show rType ++ " {"
  ++ "rowDate=" ++ show rowDate ++ ", " 
  ++ "rowCounterparty=" ++ show rowCounterparty ++ ", " 
  ++ "rowBankAccount=" ++ show rowBankAccount ++ ", " 
  ++ "rowTotal=" ++ show rowTotal ++ ", " 
  ++ "rowGlAccount=" ++ show rowGlAccount ++ ", " 
  ++ "rowAmount=" ++ show rowAmount ++ ", " 
  ++ "rowTax=" ++ show rowTax ++ "}" 


class ReceiptRowTypeClass a where
  rowType :: a -> ReceiptRowType

instance ReceiptRowTypeClass (ReceiptRow ValidHeaderT) where rowType = const ValidHeaderT
instance ReceiptRowTypeClass (ReceiptRow InvalidHeaderT) where rowType = const InvalidHeaderT
instance ReceiptRowTypeClass (ReceiptRow ValidRowT) where rowType = const ValidRowT
instance ReceiptRowTypeClass (ReceiptRow InvalidRowT) where rowType = const InvalidRowT
instance (ReceiptRowTypeClass l, ReceiptRowTypeClass r) => ReceiptRowTypeClass (Either l r) where
  rowType = either rowType rowType

data InvalidField = ParsingError { invFieldType :: Text
                                 , invFieldValue :: Text
                                 }
  deriving (Read, Show)

invalidFieldError :: InvalidField -> Text
invalidFieldError ParsingError{..} = "Can't parse '"
                                     <> invFieldValue
                                     <> "' as "
                                     <> invFieldType
                                     <> "."

type family HeaderFieldTF (s :: ReceiptRowType) a where
  HeaderFieldTF RawT  a = Either InvalidField (Maybe a)
  HeaderFieldTF ValidHeaderT a = a
  HeaderFieldTF InvalidHeaderT a = Either InvalidField (Maybe a)
  HeaderFieldTF ValidRowT a = ()
  HeaderFieldTF InvalidRowT a = ()

type family RowFieldTF (s :: ReceiptRowType) a where
  RowFieldTF RawT  (Maybe a) = Either InvalidField (Maybe a)
  RowFieldTF RawT  a = Either InvalidField (Maybe a)
  RowFieldTF ValidHeaderT a = a
  RowFieldTF InvalidHeaderT (Maybe a) = Either InvalidField (Maybe a)
  RowFieldTF InvalidHeaderT a = Either InvalidField (Maybe a)
  RowFieldTF ValidRowT a = a
  RowFieldTF InvalidRowT (Maybe a) = Either InvalidField (Maybe a)
  RowFieldTF InvalidRowT a = Either InvalidField (Maybe a)
  
type EitherRow a b = Either (ReceiptRow a) (ReceiptRow b)

-- What are invalid header ? with valid header field but missing  or invalid header field
analyseReceiptRow :: ReceiptRow RawT -> Either (EitherRow InvalidRowT ValidRowT )
                                               (EitherRow InvalidHeaderT ValidHeaderT )
analyseReceiptRow (ReceiptRow (RJust date) (RJust counterparty) (RJust bankAccount) (RJust total) (RJust glAccount) (RJust amount) (Right tax))
  = Right . Right $ ReceiptRow date counterparty bankAccount total
                                   glAccount amount tax
analyseReceiptRow (NonHeaderRow (RJust glAccount) (RJust amount) (Right tax))
  -- valid row
  = Left . Right $  RowP glAccount amount tax
analyseReceiptRow (NonHeaderRow glAccount amount tax)
  -- invalid row
  = Left . Left $  RowP glAccount amount tax
analyseReceiptRow ReceiptRow{..}
  -- invalid header
  = Right. Left $ ReceiptRow rowDate
                                     rowCounterparty
                                     rowBankAccount
                                     rowTotal
                                     rowGlAccount
                                     rowAmount
                                     rowTax
  where row = ReceiptRow () () () ()

class Transformable a b where
  transform :: a -> b

instance Transformable a () where
  transform = const ()

instance {-# OVERLAPPABLE #-} a ~ b =>  Transformable a b where
  transform x = x

instance {-# OVERLAPPABLE #-} (Transformable l l', Transformable r r' )
         => Transformable (Either l r) (Either l' r') where
  transform = bimap transform transform
  
instance (Transformable a b) => Transformable a (Maybe b) where
  transform = Just . transform  

instance Transformable () (Maybe a) where
  transform = const Nothing

instance Transformable (Maybe Double) Double where
  transform x = -1

instance Transformable (Maybe Int) Int where
  transform x = error (show x) --  -1

instance Transformable (Maybe Double) Text where
  transform x = "-1"

instance Transformable (Maybe Text) Text where
  transform x = "-1"

instance Transformable Double Text where
  transform = tshow

transformRow ReceiptRow{..} = ReceiptRow
  (transform rowDate)
  (transform rowCounterparty)
  (transform rowBankAccount)
  (transform rowTotal)
  (transform rowGlAccount )
  (transform rowAmount )
  (transform rowTax )
