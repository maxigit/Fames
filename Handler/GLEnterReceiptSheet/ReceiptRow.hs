{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
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
  , rowTax :: RowTaxTF s
  } -- deriving (Read, Show, Eq)

  
data ReceiptRowType
  = Raw
  | ValidHeaderT
  | InvalidHeaderT
  | ValidRowT
  | InvalidRowT
  deriving (Read, Show, Eq)

instance Show (ReceiptRow ValidHeaderT) where show = showReceiptRow ValidHeaderT
instance Show (ReceiptRow InvalidHeaderT) where show = showReceiptRow InvalidHeaderT
instance Show (ReceiptRow ValidRowT) where show = showReceiptRow ValidRowT
instance Show (ReceiptRow InvalidRowT) where show = showReceiptRow InvalidRowT
         

showReceiptRow rType ReceiptRow{..} = show "ReceiptRow " ++ show rType ++ " {"
  ++ "rowDate=" ++ show rowDate ++ ", " 
  ++ "rowCounterparty=" ++ show rowCounterparty ++ ", " 
  ++ "rowBankAccount=" ++ show rowBankAccount ++ ", " 
  ++ "rowTotal=" ++ show rowTotal ++ ", " 
  ++ "rowGlAccount=" ++ show rowGlAccount ++ ", " 
  ++ "rowAmount=" ++ show rowAmount ++ ", " 
  ++ "rowTax=" ++ show rowTax ++ "}" 

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
  RowTaxTF ValidHeaderT = (Maybe Text)
  RowTaxTF InvalidHeaderT = (Maybe Text)
  RowTaxTF ValidRowT = (Maybe Text)
  RowTaxTF InvalidRowT = (Maybe Text)

  {-
analyseReceiptRow :: ReceiptRow Raw -> ReceiptRow
analyseReceiptRow (ReceiptRow{..}) =
  case (rowDate , rowCounterparty , rowBankAccount , rowTotal) of
       ( Just date , Just counterparty , Just bankAccount , Just total)
         -> ValidHeader $ ReceiptRow date counterparty bankAccount total
                                   rowGlAccount rowAmount rowTax
       (Nothing, Nothing, Nothing, Nothing)
         -> ValidRow $ ReceiptRow () () () ()
                                   rowGlAccount rowAmount rowTax
       _ -> InvalidHeader $ ReceiptRow rowDate rowCounterparty rowBankAccount rowTotal
                          rowGlAccount rowAmount rowTax
-}
type EitherRow a b = Either (ReceiptRow a) (ReceiptRow b)

analyseReceiptRow :: ReceiptRow Raw -> Either (EitherRow InvalidRowT ValidRowT )
                                               (EitherRow InvalidHeaderT ValidHeaderT )
analyseReceiptRow (ReceiptRow{..}) =
  case (rowDate , rowCounterparty , rowBankAccount , rowTotal) of
       ( Just date , Just counterparty , Just bankAccount , Just total)
         -> Right . Right $ ReceiptRow date counterparty bankAccount total
                                   rowGlAccount rowAmount rowTax
       (Nothing, Nothing, Nothing, Nothing)
         -> Left . Right $  ReceiptRow () () () ()
                                   rowGlAccount rowAmount rowTax
       _ -> Right. Left $ ReceiptRow rowDate rowCounterparty rowBankAccount rowTotal
                          rowGlAccount rowAmount rowTax


class Transformable a b where
  transform :: a -> b

instance Transformable a () where
  transform = const ()

instance (Transformable a b) => Transformable a (Maybe b) where
  transform = Just . transform  

instance Transformable () (Maybe a) where
  transform = const Nothing

instance {-# OVERLAPS #-} Transformable a a where
  transform x = x

instance Transformable (Maybe Double) Double where
  transform x = -1

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
  (transform rowTotal )
