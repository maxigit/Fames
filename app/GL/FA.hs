{-# LANGUAGE DataKinds #-}
-- | Exposes FrontAccounting GL Concepts such as JournalEntry, GLAccount etc ...
-- It's a one to one mapping, so we there is not ADT etc ...
-- It can be used to talk to FA (via API or even forms)
-- or load them straight away from the database.
-- This module is meant to be imported qualified (as FA) as some
-- of it's concepts can conflicts with more general ones.
module GL.FA where

import ClassyPrelude
import qualified System.FilePath.Glob as Glob
import qualified Data.Text as Text

-- * General types
type Amount = Double -- that's how there are in FA
type GLAccount = Int  -- to change
type DimensionId = Int

-- * Reference
-- References are a pair of Id and name of entitie existing in FA.
-- There are use to be able to display or parse such entites (such GL accounts, dimensions etc)
data ReferenceType
  = BankAccountR
  | GLAccountR
  | Dimension1R
  | Dimension2R

data Reference (s :: ReferenceType) = Reference {refId :: !Int, refName :: !Text, refActive :: !Bool}
  deriving (Show, Read, Eq, Ord)

type BankAccountRef = Reference 'BankAccountR
type GLAccountRef = Reference 'GLAccountR
type Dimension1Ref = Reference 'Dimension1R
type Dimension2Ref = Reference 'Dimension2R


--  | Provide a map allowing to find reference by id or name
data ReferenceMap  = ReferenceMap
   { rmBankAccountMap :: IntMap BankAccountRef 
   , rmGLAccountMap :: IntMap GLAccountRef 
   , rmDimension1Map :: IntMap Dimension1Ref 
   , rmDimension2Map :: IntMap Dimension2Ref 
   }

class Referable s where
  getReferenceMap'Name :: ReferenceMap -> (IntMap (Reference s), Text)

instance Referable BankAccountR where
  getReferenceMap'Name refMap = (rmBankAccountMap refMap, "bank account")
instance Referable GLAccountR where
  getReferenceMap'Name refMap = (rmGLAccountMap refMap, "GL account")
instance Referable Dimension1R where
  getReferenceMap'Name refMap = (rmDimension1Map refMap, "Dimension 1")
instance Referable Dimension2R where
  getReferenceMap'Name refMap = (rmDimension2Map refMap, "Dimension 2")


findReferenceEither refMap ref =
  let (refm, name) = getReferenceMap'Name refMap
  in maybe (Left $ "Cant' find " <> name <> " for " <> ref) Right (parseReference refm ref)

findReference refMap ref = either (const Nothing) Just $ findReferenceEither refMap ref

-- | find a reference by glob pattern. Should only match one.
findReferenceByPattern :: IntMap (Reference r) -> Text -> Maybe (Reference r)
findReferenceByPattern refMap ref = let
  pat = Glob.compile (unpack ref)
  in case filter (Glob.match pat . unpack . refName) (toList refMap) of
        [one] -> Just one
        _ -> Nothing
     
buildRefMap :: [(Int, Text, Bool)] -> IntMap (Reference r)
buildRefMap i'ref'actives = mapFromList [(i, Reference i ref active) | (i, ref, active) <-i'ref'actives ]

-- | lookup by id or name the reference
parseReference :: IntMap (Reference r) -> Text -> Maybe (Reference r)
parseReference refMap ref =
  case Text.stripPrefix "#" ref of
    Just ref | Just refId <- readMay ref -> lookup refId  refMap
    Nothing | Just refId <- readMay ref,  Just found <- lookup refId refMap -> Just found
            | otherwise ->  findReferenceByPattern refMap ref
    _ -> Nothing


-- * Payment related
-- | Correspond to the payment form to "Misc"
data Payment = Payment [PaymentItem] deriving (Show, Read, Eq)


-- | A row in the payment. Will generate one line in the General Ledger.
-- taxThroughput is the tax input or tax output, ie the net amount
-- a tax payment refers too. For example a payment of 100 + 20 VAT corresponds
-- to two items : one with an amount of 100 and no taxThroughput and one
-- with an amont of 20 (the TAX) and a taxThroughput of 100. (the 20 relates to 100)
data PaymentItem = PaymentItem
  { glAccount :: GLAccount
  , amount :: Amount
  , taxThroughput :: Maybe Amount
  , dimension1 :: Maybe DimensionId
  , dimension2 :: Maybe DimensionId
  , memo :: Maybe Text
  } deriving (Show, Read, Eq)
               




