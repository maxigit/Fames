{-# OPTIONS_GHC -Wno-orphans #-}
-- | For TH stage problem
module ModelField
( module ModelField
, PayrollFrequency
) where 

import ClassyPrelude.Yesod hiding(Proxy)
import Database.Persist.Sql(PersistFieldSql(..))
import Text.Printf(printf)
import GL.Payroll.Timesheet (PayrollFrequency)
import Data.Decimal
import Data.Proxy
import Data.ISO3166_CountryCodes

-- * Warehouse
-- | Where as a transaction has been processed or not.
data PendingStatus = Pending | Process deriving (Eq, Read, Show, Enum, Bounded, Ord)

derivePersistField "PendingStatus"

-- | Quality of Batch. 0 is bad -  100 is perfect
newtype MatchScore = MatchScore { unMatchScore:: Double }
  deriving(Eq, Read,Show, Ord)

instance PersistField MatchScore where
  toPersistValue = toPersistValue . unMatchScore
  fromPersistValue pv = fmap MatchScore (fromPersistValue pv)
  
instance PersistFieldSql MatchScore where
  sqlType _ = SqlReal

-- * Payroll
-- | Wether a payroll cost is a added to the employer bill (cost) or paid by the employee (deduction)
data CostOrDeduction = Cost | Deduction deriving(Eq, Read, Show, Enum, Bounded, Ord)
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
               | PackingListShippingE -- ^ Shipping invoice for a delivery
               | PackingListDutyE -- ^ duty invoice for a delivery
               | PackingListInvoiceE -- ^ supplier invoice for a delivery
               | GLReceiptE
               | ItemCostValidationE -- ^ fixing of gl or cost price 
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

data FATransType
  = ST_JOURNAL
  | ST_BANKPAYMENT
  | ST_BANKDEPOSIT
  | ST_BANKTRANSFER
  | ST_SALESINVOICE
  | ST_CUSTCREDIT
  | ST_CUSTPAYMENT
  | ST_CUSTDELIVERY
  | ST_LOCTRANSFER
  | ST_INVADJUST
  | ST_PURCHORDER
  | ST_SUPPINVOICE
  | ST_SUPPCREDIT
  | ST_SUPPAYMENT
  | ST_SUPPRECEIVE
  | ST_WORKORDER
  | ST_MANUISSUE
  | ST_MANURECEIVE
  | ST_SALESORDER
  | ST_SALESQUOTE
  | ST_COSTUPDATE
  | ST_DIMENSION
  deriving (Eq, Show, Read, Ord, Bounded)

instance Enum FATransType where
  fromEnum ST_JOURNAL = 0
  fromEnum ST_BANKPAYMENT = 1
  fromEnum ST_BANKDEPOSIT = 2
  fromEnum ST_BANKTRANSFER = 4
  fromEnum ST_SALESINVOICE = 10
  fromEnum ST_CUSTCREDIT = 11
  fromEnum ST_CUSTPAYMENT = 12
  fromEnum ST_CUSTDELIVERY = 13
  fromEnum ST_LOCTRANSFER = 16
  fromEnum ST_INVADJUST = 17
  fromEnum ST_PURCHORDER = 18
  fromEnum ST_SUPPINVOICE = 20
  fromEnum ST_SUPPCREDIT = 21
  fromEnum ST_SUPPAYMENT = 22
  fromEnum ST_SUPPRECEIVE = 25
  fromEnum ST_WORKORDER = 26
  fromEnum ST_MANUISSUE = 28
  fromEnum ST_MANURECEIVE = 29
  fromEnum ST_SALESORDER = 30
  fromEnum ST_SALESQUOTE = 32
  fromEnum ST_COSTUPDATE = 35
  fromEnum ST_DIMENSION = 40

  toEnum 0 = ST_JOURNAL
  toEnum 1 = ST_BANKPAYMENT
  toEnum 2 = ST_BANKDEPOSIT
  toEnum 4 = ST_BANKTRANSFER
  toEnum 10 = ST_SALESINVOICE
  toEnum 11 = ST_CUSTCREDIT
  toEnum 12 = ST_CUSTPAYMENT
  toEnum 13 = ST_CUSTDELIVERY
  toEnum 16 = ST_LOCTRANSFER
  toEnum 17 = ST_INVADJUST
  toEnum 18 = ST_PURCHORDER
  toEnum 20 = ST_SUPPINVOICE
  toEnum 21 = ST_SUPPCREDIT
  toEnum 22 = ST_SUPPAYMENT
  toEnum 25 = ST_SUPPRECEIVE
  toEnum 26 = ST_WORKORDER
  toEnum 28 = ST_MANUISSUE
  toEnum 29 = ST_MANURECEIVE
  toEnum 30 = ST_SALESORDER
  toEnum 32 = ST_SALESQUOTE
  toEnum 35 = ST_COSTUPDATE
  toEnum i = error $ "Can't convert " ++ show i ++ " to FATransType"

customerFATransactions, supplierFATransactions :: [FATransType]
customerFATransactions =  
  [ ST_SALESINVOICE
  , ST_CUSTCREDIT
  , ST_CUSTPAYMENT
  , ST_CUSTDELIVERY
  , ST_SALESORDER
  , ST_SALESQUOTE
  ]

supplierFATransactions = 
  [ ST_PURCHORDER
  , ST_SUPPINVOICE
  , ST_SUPPCREDIT
  , ST_SUPPAYMENT
  , ST_SUPPRECEIVE
  ]


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

