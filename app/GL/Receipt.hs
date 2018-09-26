-- | Event corresponding to a receipt.
-- It translates in FA to a payment or a supplier invoice.
-- The main difference between an event and an FA payment is :
-- an even can generate more than one payment and can have
-- and can abstract things like GLAccount and GLDimension into one abstraction (to decide)
module GL.Receipt
  ( Amount
  , TaxType(..)
  , Receipt(..)
  , ReceiptItem(..)
  , translate
  , ReceiptTemplate(..)
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Types(Parser,typeMismatch)
import Data.Scientific(toRealFloat)


-- * General types
type Amount = Rational
-- | Rate dimensionless number. Used for tax rates.
type Rate = Rational
type GLAccount = Int
type Counterparty = Text
type BankAccount = Text


-- | Tax type 
data TaxType = TaxType
  { taxRate :: Rate
  , taxAccount :: GLAccount
  } deriving (Read, Show, Eq, Ord)

-- *  Receipt

data Receipt = Receipt
  { date :: Text
  , counterparty :: Counterparty
  , bankAccount :: BankAccount
  , totalAmount :: Amount
  , items :: [ReceiptItem]
  } deriving (Read, Show, Eq)

data ReceiptItem = ReceiptItem
  { glAccount :: GLAccount
  , amount :: Amount
  , taxType :: TaxType
  } deriving (Read, Show, Eq)

-- * Translation to FA objects
translate :: Receipt -> FA.Payment
translate receipt = FA.Payment (nets ++ taxes) where
  groupByTax =
    Map.fromListWith (<>)
                 [ (taxType item , [item])
                 | item <- items receipt
                 ]

  nets = [ FA.PaymentItem glAccount (f amount) Nothing Nothing Nothing Nothing
         | ReceiptItem {..} <- items receipt
         ]
           
  taxes = [ FA.PaymentItem taxAccount (f $ netAmount*rate)  (Just . f $ netAmount) Nothing Nothing Nothing
          | (TaxType rate taxAccount, items) <- Map.toList groupByTax

          , let netAmount = sum (map amount items)
          ]

  f = fromRational
  -- TaxType rate' taxAccount  = taxType

         
  


-- * Template
-- Templates allow to prefill or "guess" missing values from csv
-- such as counterparty, GLAccount but also compute Tax backward etc

data ReceiptTemplate
  = CounterpartySetter Text -- ^ set the counterparty
  | BankAccountSetter Text -- ^ set a bank account
  | CompoundTemplate [ReceiptTemplate] 
  | ItemVATDeducer  Double Text -- ^ compute the net and tax from the gross
  | ItemMemoSetter  Text
  | ItemDimension1Setter  Text
  | ItemDimension2Setter  Text
  deriving (Eq, Show, Read)

instance Semigroup ReceiptTemplate where
  a <> b = normalizeTemplate (CompoundTemplate [a,b])

instance Monoid ReceiptTemplate where
  mempty = CompoundTemplate []
  mappend = (<>)

-- | Remove unused compound
normalizeTemplate :: ReceiptTemplate -> ReceiptTemplate
normalizeTemplate template = case template of
  CompoundTemplate ts -> case map normalizeTemplate ts of
       [one] -> one
       -- (CompountTemplate us):ts' -> CompoundTemplate (us <> ts')
       manys -> CompoundTemplate manys
  all -> all

    
-- ** JSON
-- parseJSON = withObject "primary"

instance FromJSON ReceiptTemplate where
  parseJSON = fmap normalizeTemplate . parseJSON'

parseJSON' (Object o) = CompoundTemplate <$> traverse (uncurry parsePair) (sortOn fst $ mapToList o)
parseJSON' (Array a) = CompoundTemplate <$> traverse parseJSON' (toList a)
parseJSON' json = traceShow json $ error (show json)

parsePair :: Text -> Value -> Parser ReceiptTemplate
parsePair key value = case toLower key of
  "counterparty" -> flip (withText "Counterparty") value (return . CounterpartySetter)
  "bank" -> flip (withText "BankAccount") value (return . BankAccountSetter)
  "vat" -> parseVAT value
  "memo" -> flip (withText "Memo") value (return . ItemMemoSetter)
  "dimension1" -> ItemDimension1Setter <$> parseJSON value
  "dimension2" -> ItemDimension2Setter <$> parseJSON value
  _ -> typeMismatch (show key ++ " not a recognized key for") value

parseVAT v@(Object o) = case (lookup "rate" o, lookup "tax" o) of
           (Just (Number rate), Just (String name)) -> return $ ItemVATDeducer (toRealFloat $ rate/100) name
           (Just (Number _), Just v) -> typeMismatch "Tax" v
           (Just v, Just (String _)) -> typeMismatch "Rate" v
           _ -> typeMismatch "VAT record" v
parseVAT v = typeMismatch "VAT record"  v


instance ToJSON ReceiptTemplate where
  toJSON (CounterpartySetter a) = object ["counterparty" .= a]
  toJSON (BankAccountSetter a) = object ["bank" .= a]
  toJSON (ItemMemoSetter a) = object ["memo" .= a]
  toJSON (ItemDimension1Setter a) = object ["dimension1" .= a]
  toJSON (ItemDimension2Setter a) = object ["dimension2" .= a]
  toJSON (ItemVATDeducer rate tax) = object ["vat" .= [object ["rate" .= rate, "tax" .= tax]]]
  toJSON (CompoundTemplate ts)  = toJSON ts
