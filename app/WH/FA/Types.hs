--  | Types corresponding to stock related Frontaccounting concepts.
module WH.FA.Types where

import ClassyPrelude

-- * Connection parametes
data FAConnectInfo = FAConnectInfo
  { faURL :: String
  , faUser :: String
  , faPassword :: String
  }  deriving (Eq, Show)

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
  , poiDeliveryIds :: ![(Int, Maybe Int)] -- Id + number of expected items
  , poiGLItems :: ![GLItem]
  } deriving (Eq, Show)

data GLItem = GLItem
  { gliAccount :: !Int
  , gliDimension1 :: !(Maybe Int)
  , gliDimension2 :: !(Maybe Int)
  , gliAmount :: !Double 
  , gliMemo :: !(Maybe Text) 
  } deriving (Eq, Show)

-- ** Credit Note
type PurchaseCreditNote = PurchaseInvoice

-- ** Payment
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
  , ptTransactionType :: !Int
  , ptTransactionAmount :: !Double
  } deriving (Eq, Show)
  
