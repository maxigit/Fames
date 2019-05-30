module GL.TaxReport.Types where
import ClassyPrelude
import ModelField

-- * Type
type Bucket = Text
-- data Bucket = DefaultBucket | NamedBucket Text
--   deriving (Show, Read, Eq, Ord)

-- | Rules to determine the bucket of a given tax transaction
data Rule
  = BucketRule Bucket 
  | RuleList [Rule]
  | TransactionRule FATransType Rule
  | CustomerRule (Maybe Int) Rule -- Nothing matches all customer
  | SupplierRule (Maybe Int) Rule
  | TaxTypeRule  Int Rule
  | TaxRateRule Double   Rule
  deriving (Show, Read, Eq, Ord)

defaultBucket :: Bucket
defaultBucket = "<undefined>"

defaultRule :: Rule
defaultRule = BucketRule defaultBucket

data RuleInput = RuleInput
 { riTransType :: FATransType
 , riEntity :: Maybe Int
 , riTaxType :: Int
 , riTaxRate :: Double
 } deriving (Eq, Show, Read)


data TaxBox = TaxBox
   { tbName :: Text -- 
   , tbDescription :: Maybe Text
   , tbRule :: TaxBoxRule
   } deriving (Eq, Show, Read)

-- | Rules to determine the content of a box
data TaxBoxRule
  = TaxBoxSum [TaxBoxRule] -- sun all
  | TaxBoxNet Bucket
  | TaxBoxTax Bucket
  | TaxBoxGross Bucket
  | TaxBoxSub TaxBoxRule TaxBoxRule
  | TaxBoxNegate TaxBoxRule
  -- rounding
  | TaxBoxFloor TaxBoxRule
  | TaxBoxCeil TaxBoxRule
  | TaxBoxRound TaxBoxRule
  | TaxBoxBanker TaxBoxRule -- Banker round. round function in Haskell
  deriving (Show, Read, Eq)
