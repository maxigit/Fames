{-# LANGUAGE StandaloneDeriving #-}
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
  , ReceiptTemplate'(..)
  , ReceiptTemplate
  , ReceiptTemplateExpanded
  , expandTemplate
  ) where

import ClassyPrelude
import qualified GL.FA as FA
import qualified Data.Map as Map
import Data.Aeson
import Data.Aeson.Types(Parser,typeMismatch)
import Control.Applicative (Const(..))


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
-- It is parametrized by a functor but should only be use with two: const or Identity
-- The point , the reference map needed to replace text by the referee can only
-- be loaded after the parameter has been read from the coni
 
data ReceiptTemplate' f
  = CounterpartySetter Text --  ^ set the counterparty
  | BankAccountSetter (f FA.BankAccountRef) --  ^ set a bank account
  | CompoundTemplate [ReceiptTemplate' f] 
  | ItemGLAccountSetter (f FA.GLAccountRef) --  ^ set a gl account
  | ItemVATDeducer (f FA.TaxRef)  -- ^ compute the net and tax from the gross
  | ItemMemoSetter  Text
  | ItemDimension1Setter  (f FA.Dimension1Ref)
  | ItemDimension2Setter  (f FA.Dimension2Ref)
  | BankAccountMapper (Map Text (f FA.BankAccountRef))
  | ItemTaxMapper (Map Text (f FA.TaxRef))
  | ItemGLAccountMapper (Map Text (f FA.GLAccountRef))
  -- deriving (Eq, Show)

type ReceiptTemplate = ReceiptTemplate' (Const Text)
type ReceiptTemplateExpanded = ReceiptTemplate' Identity

deriving instance Show ReceiptTemplate
deriving instance Eq ReceiptTemplate

instance Semigroup (ReceiptTemplate' f) where
  a <> b = normalizeTemplate (CompoundTemplate [a,b])

instance Monoid (ReceiptTemplate' f) where
  mempty = CompoundTemplate []

-- | Remove unused compound
normalizeTemplate :: ReceiptTemplate' f -> ReceiptTemplate' f
normalizeTemplate template = case template of
  CompoundTemplate ts -> case map normalizeTemplate ts of
       [one] -> one
       -- (CompountTemplate us):ts' -> CompoundTemplate (us <> ts')
       manys -> CompoundTemplate manys
  l_all -> l_all

expandTemplate :: FA.ReferenceMap -> ReceiptTemplate' (Const Text) -> Either Text (ReceiptTemplate' Identity)
expandTemplate refMap (BankAccountSetter (Const text)) = BankAccountSetter . Identity <$> FA.findReferenceEither refMap text
expandTemplate refMap (BankAccountMapper (mapping)) = BankAccountMapper <$> traverse ((fmap Identity) . FA.findReferenceEither refMap . getConst) mapping
expandTemplate refMap (ItemGLAccountSetter (Const text)) = ItemGLAccountSetter . Identity <$> FA.findReferenceEither refMap text
expandTemplate refMap (CompoundTemplate ts) = CompoundTemplate <$> mapM (expandTemplate refMap) ts
expandTemplate refMap (ItemDimension1Setter (Const text)) = ItemDimension1Setter . Identity <$> FA.findReferenceEither refMap text
expandTemplate refMap (ItemDimension2Setter (Const text)) = ItemDimension2Setter . Identity <$> FA.findReferenceEither refMap text
expandTemplate refMap (ItemVATDeducer (Const text)) = ItemVATDeducer . Identity <$> FA.findReferenceEither refMap text
expandTemplate refMap (ItemTaxMapper (mapping)) = ItemTaxMapper <$> traverse ((fmap Identity) . FA.findReferenceEither refMap . getConst) mapping
expandTemplate refMap (ItemGLAccountMapper (mapping)) = ItemGLAccountMapper <$> traverse ((fmap Identity) . FA.findReferenceEither refMap . getConst) mapping
expandTemplate __refMap (CounterpartySetter text) = Right $ CounterpartySetter text
expandTemplate __refMap (ItemMemoSetter text) = Right $ ItemMemoSetter  text
   
-- ** JSON 
-- parseJSON = withObject "primary"

instance FromJSON (ReceiptTemplate' (Const Text)) where
  parseJSON = fmap normalizeTemplate . parseJSON'

parseJSON' :: Value -> Parser (ReceiptTemplate' (Const Text))
parseJSON' (Object o) = CompoundTemplate <$> traverse (uncurry parsePair) (sortOn fst $ mapToList o)
parseJSON' (Array a) = CompoundTemplate <$> traverse parseJSON' (toList a)
parseJSON' l_json = error (show l_json)

parsePair :: Text -> Value -> Parser (ReceiptTemplate' (Const Text))
parsePair key value = case toLower key of
  "counterparty" -> flip (withText "Counterparty") value (return . CounterpartySetter)
  "bank" -> flip (withText "BankAccount") value (return . BankAccountSetter . Const )
  "bankalias" -> do -- flip (withObject "BankAlias") value (return . BankAccountSetter . fmap Const )
      l_pairs <- parseJSON value
      return $ BankAccountMapper (fmap Const l_pairs)
  "tax" -> flip (withText "tax") value (return . ItemVATDeducer . Const)
  "memo" -> flip (withText "Memo") value (return . ItemMemoSetter)
  "glaccount" -> flip (withText "glAccount") value (return . ItemGLAccountSetter . Const)
  "dimension1" -> ItemDimension1Setter <$> parseJSON value
  "dimension2" -> ItemDimension2Setter <$> parseJSON value
  "taxalias" -> do
      l_pairs <- parseJSON value
      return $ ItemTaxMapper (fmap Const l_pairs)
  "glalias" -> do
      l_pairs <- parseJSON value
      return $ ItemGLAccountMapper (fmap Const l_pairs)
  _ -> typeMismatch (show key ++ " not a recognized key for") value

instance ToJSON (ReceiptTemplate' (Const Text)) where
  toJSON (CounterpartySetter a) = object ["counterparty" .= a]
  toJSON (BankAccountSetter a) = object ["bank" .= a]
  toJSON (BankAccountMapper a) = object ["bankAlias" .= toJSON a]
  toJSON (ItemTaxMapper a) = object ["taxAlias" .= toJSON a]
  toJSON (ItemGLAccountMapper a) = object ["glAlias" .= toJSON a]
  toJSON (ItemMemoSetter a) = object ["memo" .= a]
  toJSON (ItemGLAccountSetter a) = object ["glAccount" .= a]
  toJSON (ItemDimension1Setter a) = object ["dimension1" .= a]
  toJSON (ItemDimension2Setter a) = object ["dimension2" .= a]
  toJSON (ItemVATDeducer a) = object ["tax" .= a]
  toJSON (CompoundTemplate ts)  = toJSON ts
  
