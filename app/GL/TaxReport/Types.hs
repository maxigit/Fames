module GL.TaxReport.Types where
import ClassyPrelude
import ModelField
import Util.Decimal

-- * Type 
type Bucket = Text
-- data Bucket = DefaultBucket | NamedBucket Text
--   deriving (Show, Eq, Ord)

-- | Rules to determine the bucket of a given tax transaction
data Rule
  = BucketRule Bucket 
  | RuleList [Rule]
  | TransactionRule [FATransType] Rule
  | CustomerRule [Int64] Rule -- Nothing matches all customer
  | SupplierRule [Int64] Rule
  | TaxTypeRule  [Int64] Rule
  | TaxRateRule [Double]   Rule
  | InputRule Rule
  | OutputRule Rule
  | CustomerCategory Text (Maybe Text)
  deriving (Show, Eq, Ord)

defaultBucket :: Bucket
defaultBucket = "<undefined>"

defaultRule :: Rule
defaultRule = BucketRule defaultBucket

data RuleInput = RuleInput
 { riTransType :: FATransType
 , riEntity :: Maybe Int64
 , riTaxType :: Int64
 , riTaxRate :: Double
 , riAmount :: Double
 , riCustCategoryFinder:: Text -> Maybe Text
 } -- deriving (Eq, Show)

data TaxBox = TaxBox
   { tbName :: Text -- 
   , tbDescription :: Maybe Text
   , tbShouldBe :: Maybe Ordering -- expected sign of amount
   , tbRule :: TaxBoxRule
   , tbRound :: Maybe RoundingMethod
   } deriving (Eq, Show)

tbDefaultDecimal :: Word8
tbDefaultDecimal = 6

tbRound0 :: TaxBox -> RoundingMethod
tbRound0 = fromMaybe (RoundBanker tbDefaultDecimal) . tbRound

-- | Rules to determine the content of a box
data TaxBoxRule
  = TaxBoxSum [TaxBoxRule] -- sun all
  | TaxBoxNet Bucket -- list can be parsed as json
  | TaxBoxTax Bucket
  | TaxBoxGross Bucket
  | TaxBoxSub TaxBoxRule TaxBoxRule
  | TaxBoxNegate TaxBoxRule
  | TaxBoxTaxWith Double Bucket
  -- rounding
  | TaxBoxFloor Word8 TaxBoxRule
  | TaxBoxCeil Word8 TaxBoxRule
  | TaxBoxRound Word8 TaxBoxRule
  | TaxBoxBanker Word8 TaxBoxRule -- Banker round. round function in Haskell
  deriving (Show, Eq)
