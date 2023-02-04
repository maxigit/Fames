{-# OPTIONS_GHC -Wno-orphans #-}
-- | For TH stage problem
module ModelField
( module ModelField
, module FATransType
, PayrollFrequency
) where 

import ClassyPrelude.Yesod hiding(Proxy)
import Database.Persist.Sql(PersistFieldSql(..))
import Text.Printf(printf)
import GL.Payroll.Timesheet (PayrollFrequency)
import Data.Decimal
import Data.Proxy
import Data.ISO3166_CountryCodes
import FATransType

-- * Warehouse 
-- | Where as a transaction has been processed or not.
data PendingStatus = Pending | Process deriving (Eq, Show, Read, Enum, Bounded, Ord)

derivePersistField "PendingStatus"

-- | Quality of Batch. 0 is bad -  100 is perfect
newtype MatchScore = MatchScore { unMatchScore:: Double }
  deriving(Eq,Show, Ord)

instance PersistField MatchScore where
  toPersistValue = toPersistValue . unMatchScore
  fromPersistValue pv = fmap MatchScore (fromPersistValue pv)
  
instance PersistFieldSql MatchScore where
  sqlType _ = SqlReal

-- * Payroll 
-- | Wether a payroll cost is a added to the employer bill (cost) or paid by the employee (deduction)
data CostOrDeduction = Cost | Deduction deriving(Eq, Show, Read, Enum, Bounded, Ord)
derivePersistField "CostOrDeduction"

derivePersistField "PayrollFrequency"

-- * Miscelaneous 
-- Trick to use in Persistent model declaration
-- as using normal tuples doesn't seem to work
type Pair a b = (a,b)
type Pairs a b = [(a,b)]


-- * FrontAccounting 
-- | Type of Fames "event" To be used in TransactionMap
data EventType = StockAdjustmentE
               | PayrollShiftE
               | PayrollItemE
               | TimesheetE
               | PackingListShippingE --  ^ Shipping invoice for a delivery
               | PackingListDutyE --  ^ duty invoice for a delivery
               | PackingListInvoiceE --  ^ supplier invoice for a delivery
               | GLReceiptE
               | ItemCostValidationE --  ^ fixing of gl or cost price 
               | GLSupplierMirroredE -- ^ supplier invoice/credit note pair
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | derivePersistField uses String instead of an INt
-- we want an Int to be consistent with FA
instance PersistField EventType where
  toPersistValue = toPersistValue . fromEnum
  fromPersistValue = fmap toEnum . fromPersistValue

-- | FA transaction types
-- We want exactly the same number as the one used in FA
-- so that we can do join with fa tables
-- Therefore we need to define the Enum instance manually



instance PersistField FATransType where
  toPersistValue = toPersistValue . fromEnum
  fromPersistValue = map toEnum . fromPersistValue


instance ToJSON FATransType where
  toJSON = toJSON . fromEnum

instance FromJSON FATransType where
  parseJSON o = toEnum <$> parseJSON o

inTypes :: [FATransType] -> Text
inTypes types = intercalate "," $ map (tshow . fromEnum) types
  

urlForFA :: Text -> FATransType -> Int -> Text
urlForFA base type_ no = base <> "/" <> pack url  where
  tp = fromEnum type_
  url = case type_ of
    ST_CUSTDELIVERY -> printf "sales/view/view_dispatch.php?trans_no=%d?trans_type=%d" no tp
    ST_CUSTCREDIT -> printf "sales/view/view_credit.php?trans_no=%d?trans_type=%d" no tp
    ST_CUSTPAYMENT -> printf "sales/view/view_receipt.php?trans_no=%d&trans_type=%d" no tp
    ST_SALESINVOICE -> printf "sales/view/view_invoice.php?trans_no=%d&trans_type=%d" no tp
    ST_LOCTRANSFER -> printf "inventory/view/view_transfer.php?trans_no=%d" no
    ST_INVADJUST -> printf "inventory/view/view_adjustment.php?trans_no=%d" no
    ST_SUPPRECEIVE -> printf "purchasing/view/view_grn.php?trans_no=%d" no
    ST_SUPPAYMENT -> printf "purchasing/view/view_supp_payment.php?trans_no=%d" no
    ST_SUPPINVOICE -> printf "purchasing/view/view_supp_invoice.php?trans_no=%d" no
    ST_SUPPCREDIT -> printf "purchasing/view/view_supp_credit.php?trans_no=%d" no
    ST_BANKTRANSFER -> printf "gl/view/bank_transfer_view.php?trans_no=%d" no
    ST_BANKPAYMENT -> printf "gl/view/gl_payment_view.php?trans_no=%d" no
    ST_BANKDEPOSIT -> printf "gl/view/gl_deposit_view.php?trans_no=%d" no
    ST_JOURNAL -> printf "gl/view/gl_trans_view.php?type_id=%d&trans_no=%d" tp no
    ST_COSTUPDATE -> printf "gl/view/gl_trans_view.php?type_id=%d&trans_no=%d" tp no
    _ -> "not found"

glViewUrlForFA :: Text -> FATransType -> Int -> Text
glViewUrlForFA base type_ no = base <> "/" <> pack url where
  tp = fromEnum type_
  url = printf "gl/view/gl_trans_view.php?type_id=%d&trans_no=%d" tp no

-- * PersistField Instance 
-- ** Value 
instance PersistField Value where
  toPersistValue = toPersistValueJSON
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql Value where
  sqlType _ = SqlBlob -- sqlType (Proxy :: Proxy Text)
-- ** Decimal 
instance PersistField Decimal where
  toPersistValue = toPersistValue . tshow
  fromPersistValue = fromPersistValue >=> f where
    f :: Text -> Either Text Decimal
    f = maybe (Left "Problem decoding Decimal" ) Right . readMay -- :: String -> Either Text Decimal
instance PersistFieldSql Decimal where
  sqlType _ = sqlType (Proxy :: Proxy Rational)

-- instance ToJSON Decimal where
--   toJSON = Number (scientific kk)

-- instance FromJSON Decimal where
--   parseJSON v = do
--     r <- parseJSON v
--     case eitherFromRational r of
--       Left err -> fail err
--       Right dec -> return dec

derivePersistField "CountryCode"

