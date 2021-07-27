{-# LANGUAGE NoImplicitPrelude #-}
--  | Types corresponding to stock related Frontaccounting concepts.
module WH.FA.Types
( module WH.FA.Types
, FATransType(..)
) where

import ClassyPrelude
import Data.Decimal
import FATransType

-- * Connection parametes
data FAConnectInfo = FAConnectInfo
  { faURL :: String
  , faUser :: String
  , faPassword :: String
  }  deriving (Eq, Show)

-- | An GLAccount. Even though GLAccount should be Int
-- the formatting matters, so we need to store them as Text
newtype GLAccount = GLAccount {unGLAccount :: Text}
  deriving (Eq, Show, Read, Ord)
-- * Items
-- ** StockAdjustment
-- | Information needed to post a FA stock adjustment
data StockAdjustment = StockAdjustment
  { adjReference :: !Text
  , adjLocation :: !Text
  , adjDate :: !Day
  , adjDetails :: [StockAdjustmentDetail]
  , adjAdjType :: AdjustmentType
  } deriving (Eq, Show)

data StockAdjustmentDetail = StockAdjustmentDetail
  { adjSku :: !Text
  , adjQuantity :: !Double
  , adjCost :: !Double
  } deriving (Eq, Show)

data AdjustmentType = PositiveAdjustment  | NegativeAdjustment
  deriving (Eq, Show, Enum)

-- ** Location Transfer
-- | needed to post a FA stock adjustment
data LocationTransfer = LocationTransfer
  { ltrReference:: !Text
  , ltrLocationFrom :: !Text
  , ltrLocationTo :: !Text
  , ltrDate :: !Day
  , ltrDetails :: [LocationTransferDetail]
  } deriving (Eq, Show)

data LocationTransferDetail = LocationTransferDetail
  { ltrSku :: !Text
  , ltrQuantity :: !Int
  } deriving (Eq, Show)

-- ** CostUpdate
data CostUpdate = CostUpdate
  { cuSku :: !Text
  , cuCost :: Double
  } deriving (Eq, Show)
-- * GL
-- ** Payments
data BankPayment = BankPayment
  { bpDate :: !Day
  , bpReference  :: !(Maybe Text)
  , bpCounterparty :: !Text
  , bpBankAccount :: !Int
  , bpMemo :: !(Maybe Text)
  , bpItems :: [GLItemD]
  } deriving (Eq, Show)

-- ** Deposits
data BankDeposit = BankDeposit
  { bdDate :: !Day
  , bdReference  :: !(Maybe Text)
  , bdCounterparty :: !Text
  , bdBankAccount :: !Int
  , bdMemo :: !(Maybe Text)
  , bdItems :: [GLItemD]
  } deriving (Eq, Show)


-- ** Journal Entry
data JournalEntry = JournalEntry
  { jeDate :: !Day
  , jeReference :: !(Maybe Text)
  , jeItems :: [GLItemD]
  , jeMemo :: !(Maybe Text)
  } deriving (Eq, Show)

-- * Purchases
-- ** GRN
data GRN = GRN
  { grnSupplier :: !Int
  , grnDeliveryDate :: !Day
  , grnReference :: !(Maybe Text)
  , grnSupplierReference :: !(Maybe Text)
  , grnLocation :: !Text
  , grnDeliveryInformation :: !(Maybe Text)
  , grnMemo :: !Text
  , grnDetails :: [GRNDetail]
  } deriving (Eq, Show)

data GRNDetail = GRNDetail
  { grnSku :: !Text
  -- , grnDescription :: !(Maybe Text)
  , grnQuantity :: !Double
  , grnPrice :: !Double
  } deriving (Eq, Show)

-- ** Invoice
data PurchaseInvoice = PurchaseInvoice
  { poiSupplier :: !Int
  , poiReference :: !(Maybe Text)
  , poiSupplierReference :: !Text
  , poiDate :: !Day
  , poiDueDate :: !Day
  , poiMemo :: !Text
  , poiDeliveryIds :: ![(Int, Maybe Int)] -- Id + number of expected items. invoice all available quantity
  , poiGLItems :: ![GLItem]
  } deriving (Eq, Show)

data GLItem' a = GLItem
  { gliAccount :: !GLAccount -- ^ can't be a int as leading 0 matters ...
  , gliDimension1 :: !(Maybe Int)
  , gliDimension2 :: !(Maybe Int)
  , gliAmount :: !a 
  , gliTaxOutput :: !(Maybe a)
  , gliMemo :: !(Maybe Text) 
  } deriving (Eq, Show)

type GLItem = GLItem' Double
type GLItemD = GLItem' Decimal
-- ** Credit Note
data PurchaseCreditNote = PurchaseCreditNote
  { pcnSupplier :: !Int
  , pcnReference :: !(Maybe Text)
  , pcnSupplierReference :: !Text
  , pcnDate :: !Day
  , pcnDueDate :: !Day 
  , pcnMemo :: !Text
  , pcnInvoiceNo :: !(Maybe Int) -- invoice to allocate credit note too
  , pcnDeliveryIds :: ![()] -- Not saved yet (Int, Maybe Int)] -- Id + number of expected items
  , pcnGLItems :: ![GLItem]
  } deriving (Eq,Show)
-- * Sales
data SalesOrder = SalesOrder
    { soCustomerId :: !Int
    , soBranchNo  :: !Int
    , soReference :: !Text
    , soItems :: ![SalesOrderItem]
    -- , soPayment :: !
    , soOrderDate :: !Day
    , soDeliveryDate :: !Day
    , soDeliverTo :: !Text
    , soDeliveryAddress :: !Text
    , soPhone :: !Text
    , soComment :: !Text
    -- Optional, taken from the customer default if needed
    , soPayment :: !(Maybe Text)
    , soSalesType :: !(Maybe Text)
    , soNowOrNever :: !(Maybe Text)
    , soLocation :: !(Maybe Text)
    , soShipVia :: !(Maybe Text)
    } deriving (Eq, Show)

data SalesOrderItem = SalesOrderItem
    { soiStockId :: !Text
    , soiQuantity :: !Int
    , soiPrice :: !Double
    , soiDiscountPercent :: !Double
    , soiNowOrNever :: !(Maybe NowOrNever)
    } deriving (Eq, Show)

data NowOrNever = HappyToWait | NowOrNever
    deriving (Eq, Show, Enum)

-- * Payment
-- | Payment to a supplier. Items are the 
data SupplierPayment = SupplierPayment
  { spSupplier :: !Int
  , spBankAccount :: !Int
  , spTotalAmount :: !Double
  , spDate :: !Day
  , spReference :: !(Maybe Text)
  , spBankCharge :: !(Maybe Double)
  , spAllocatedTransactions :: ![PaymentTransaction]
  } deriving (Eq, Show)

-- |  A transaction to allocate a payment to
data PaymentTransaction = PaymentTransaction
  {ptTransactionNo :: !Int
  , ptTransactionType :: !FATransType
  , ptTransactionAmount :: !Double
  } deriving (Eq, Show)
  

-- * Voiding
data VoidTransaction = VoidTransaction
  { vtTransNo :: !Int
  , vtTransType :: !FATransType
  , vtDate :: !Day
  , vtComment :: !(Maybe Text)
  }
