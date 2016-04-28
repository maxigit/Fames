{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Handler.GLEnterReceiptSheet.ReceiptRow where

import Import hiding(InvalidHeader)
import Data.Bifunctor (bimap)

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

data ErrorDescription a = ErrorDescription Text (Maybe a)
instance (Show a) => Show (ErrorDescription a)
  where
    show (ErrorDescription desc v) = "ErrorDescription " ++ show desc ++ " " ++ show v
instance Functor (ErrorDescription) where
  fmap f (ErrorDescription t x) = ErrorDescription t (f <$> x)
  
type ParsingError t = Either (ErrorDescription t) t

type family RowDateTF (s :: ReceiptRowType) where
  RowDateTF Raw = Either Text (Maybe Text)
  RowDateTF ValidHeaderT = Text
  RowDateTF InvalidHeaderT = ParsingError Text
  RowDateTF ValidRowT = ()
  RowDateTF InvalidRowT = ()

type family RowCounterpartyTF (s :: ReceiptRowType) where
  RowCounterpartyTF Raw = Either Text (Maybe Text)
  RowCounterpartyTF ValidHeaderT = Text
  RowCounterpartyTF InvalidHeaderT = ParsingError Text
  RowCounterpartyTF ValidRowT = ()
  RowCounterpartyTF InvalidRowT = ()

type family RowBankAccountTF (s :: ReceiptRowType) where
  RowBankAccountTF Raw = Either Text (Maybe Text)
  RowBankAccountTF ValidHeaderT = Text
  RowBankAccountTF InvalidHeaderT = ParsingError Text
  RowBankAccountTF ValidRowT = ()
  RowBankAccountTF InvalidRowT = ()

type family RowTotalTF (s :: ReceiptRowType) where
  RowTotalTF Raw = Either Text (Maybe Double)
  RowTotalTF ValidHeaderT = Double
  RowTotalTF InvalidHeaderT = ParsingError Double
  RowTotalTF ValidRowT = ()
  RowTotalTF InvalidRowT = ()

type family RowGLAccountTF (s :: ReceiptRowType) where
  RowGLAccountTF Raw = Either Text Int
  RowGLAccountTF ValidHeaderT = Int
  RowGLAccountTF InvalidHeaderT = ParsingError Int
  RowGLAccountTF ValidRowT = Int
  RowGLAccountTF InvalidRowT = ParsingError Int

type family RowAmountTF (s :: ReceiptRowType) where
  RowAmountTF Raw = Either Text Double
  RowAmountTF ValidHeaderT = Double
  RowAmountTF InvalidHeaderT = ParsingError Double
  RowAmountTF ValidRowT = Double
  RowAmountTF InvalidRowT = ParsingError Double

type family RowTaxTF (s :: ReceiptRowType) where
  RowTaxTF Raw = Either Text (Maybe Text)
  RowTaxTF ValidHeaderT = (Maybe Text)
  RowTaxTF InvalidHeaderT = ParsingError (Maybe Text)
  RowTaxTF ValidRowT = (Maybe Text)
  RowTaxTF InvalidRowT = ParsingError (Maybe Text)

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

-- What are invalid header ? with valid header field but missing  or invalid header field
analyseReceiptRow :: ReceiptRow Raw -> Either (EitherRow InvalidRowT ValidRowT )
                                               (EitherRow InvalidHeaderT ValidHeaderT )
analyseReceiptRow (ReceiptRow{..}) =
  case (rowDate , rowCounterparty , rowBankAccount , rowTotal, rowGlAccount, rowAmount, rowTax) of
       -- valid header
       ( Right (Just date) , Right (Just counterparty) , Right (Just bankAccount) , Right (Just total) , Right glAccount, Right amount, Right tax )
         -> Right . Right $ ReceiptRow date counterparty bankAccount total
                                   glAccount amount tax
       -- valid row
       (Right Nothing, Right Nothing, Right Nothing, Right Nothing , Right glAccount, Right amount, Right tax )
         -> Left . Right $  ReceiptRow () () () ()
                                   glAccount amount tax
       -- invalid row
       (Right Nothing, Right Nothing, Right Nothing, Right Nothing , glAccount, amount, tax )
         -> Left . Left $  ReceiptRow () () () ()
                                      (validateEither "GL account" (Just <$>glAccount))
                                      (validateEither "Amount" (Just <$> amount))
                                      (validateEither "Tax" (Just <$> tax))
       -- invalid header
       _ -> Right. Left $ ReceiptRow (validateEither "Date" rowDate)
                                     (validateEither "Counterparty" rowCounterparty)
                                     (validateEither "Bank account" rowBankAccount)
                                     (validateEither "Total" rowTotal)
                                      (validateEither "GL account" (Just <$> rowGlAccount))
                                      (validateEither "Amount" (Just <$> rowAmount))
                                      (validateEither "Tax" (Just <$> rowTax))

  where validateMaybe name Nothing = Left $ ErrorDescription (name <> " is missing") Nothing
        validateMaybe _ (Just v) = Right v

        validateEither :: Show a => Text -> Either Text (Maybe a) -> ParsingError a
        validateEither name (Left v) = Left $ ErrorDescription (name <> " is invalid") Nothing
        validateEither name (Right v) = validateMaybe name v

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

instance Transformable (Maybe Double) Text where
  transform x = "-1"

instance Transformable (Maybe Text) Text where
  transform x = "-1"

instance Transformable Double Text where
  transform = tshow

instance Transformable a b => Transformable (ErrorDescription a) (ErrorDescription b) where
 transform = map transform

transformRow ReceiptRow{..} = ReceiptRow
  (transform rowDate)
  (transform rowCounterparty)
  (transform rowBankAccount)
  (transform rowTotal)
  (transform rowGlAccount )
  (transform rowAmount )
  (transform rowTax )
