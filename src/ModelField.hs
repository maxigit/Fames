-- | For TH stage problem
module ModelField
( module ModelField
, PayrollFrequency(..)
) where 

import ClassyPrelude.Yesod
import Text.Printf(printf)
import GL.Payroll.Timesheet (PayrollFrequency)
-- * Warehouse
-- | Where as a transaction has been processed or not.
data PendingStatus = Pending | Process deriving (Eq, Read, Show, Enum, Bounded, Ord)

derivePersistField "PendingStatus"

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
  deriving (Eq, Show, Enum)

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
  deriving (Eq, Show)

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

instance PersistField FATransType where
  toPersistValue = toPersistValue . fromEnum
  fromPersistValue = map toEnum . fromPersistValue


inTypes :: [FATransType] -> Text
inTypes types = intercalate "," $ map (tshow . fromEnum) types
  

urlForFA :: Text -> FATransType -> Int -> Text
urlForFA base type_ no = base <> "/" <> pack url  where
  tp = fromEnum type_
  url = case type_ of
    ST_CUSTDELIVERY -> printf "sales/view/view_dispatch.php?trans_no=%d?trans_type=%d" no tp
    ST_CUSTCREDIT -> printf "sales/view/view_credit.php?trans_no=%d?trans_type=%d" no tp
    ST_LOCTRANSFER -> printf "inventory/view/view_transfer.php?trans_no=%d" no
    ST_INVADJUST -> printf "inventory/view/view_adjustment.php?trans_no=%d" no
    ST_SUPPRECEIVE -> printf "purchasing/view/view_grn.php?trans_no=%d" no
    _ -> "not found"
                                        
