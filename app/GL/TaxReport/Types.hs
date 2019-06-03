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
  | CustomerRule [Int64] Rule -- Nothing matches all customer
  | SupplierRule [Int64] Rule
  | TaxTypeRule  [Int64] Rule
  | TaxRateRule [Double]   Rule
  deriving (Show, Read, Eq, Ord)

defaultBucket :: Bucket
defaultBucket = "<undefined>"

defaultRule :: Rule
defaultRule = BucketRule defaultBucket

data RuleInput = RuleInput
 { riTransType :: FATransType
 , riEntity :: Maybe Int64
 , riTaxType :: Int64
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
  | TaxBoxNet Bucket -- list can be parsed as json
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
