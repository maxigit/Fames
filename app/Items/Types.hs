{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Items.Types where

import ClassyPrelude
import Metamorphosis
import Metamorphosis.Applicative
import FA
import Language.Haskell.TH.Syntax
import Lens.Micro
import qualified GHC.Generics as GHC
import Data.IntMap.Strict (IntMap)
import qualified Data.Map as Map
import ModelField
import qualified Data.Foldable as Foldable
import Database.Persist.Types
import qualified Data.Map.Lazy as LMap
import Import.NoFoundation

-- * General
-- | Holder for miscellaneous information relative to an item.
-- Allows mainly to call operation which need to group by style and or variations.
data ItemInfo a = ItemInfo
  { iiStyle :: Text
  , iiVariation:: Text
  , iiInfo :: a
  } deriving Functor
deriving instance (Show a) => Show (ItemInfo a)
deriving instance (Eq a) => Eq (ItemInfo a)
deriving instance (Ord a) => Ord (ItemInfo a)

-- | MinMax Min and Max functor
data MinMax a = MinMax a a deriving (Functor, Eq, Ord, Show)

instance Ord a => Semigroup (MinMax a) where
  (MinMax x y) <> (MinMax x' y') = MinMax (min x x') (max y y')

instance Applicative MinMax where
  pure x = MinMax x x
  (MinMax fx fy) <*> (MinMax x y) = MinMax (fx x) (fy y)

instance (Ord a, Num a, Fractional a) => Monoid (MinMax a) where
  mempty = MinMax (1/0) 0
  mappend = (<>)

-- * Index
-- | Status of a variation within a group
-- Given a group of items and a list of variations
-- tells if the current items belongs , is missing or is extra
data VariationStatus= VarOk | VarMissing | VarExtra deriving (Eq, Show, Read, Ord)

type FieldWithClasses a = ([Text], a)
-- ** Functor parametrize data types
-- | Functor parameterized version of StockMaster
$(metamorphosis
 ( (return)
 . (fdTypes %~ ("f":))
 . (fdFieldName . mapped %~ ("smf"++). drop 11)
 . (fdBang .~ Bang NoSourceUnpackedness NoSourceStrictness)
 . (fdTConsName .~ "StockMasterF")
 )
 [''StockMaster]
 (const $ Just applicativeBCR)
 (const []) 
 )

-- instance Generic (StockMasterF f)

-- | Functor parametrized version of Price
$(metamorphosis
 ( (return)
 . (fdTypes %~ ("f":))
 . (fdFieldName . mapped %~ ("pf"++). drop 5)
 . (fdBang .~ Bang NoSourceUnpackedness NoSourceStrictness)
 . (fdTConsName .~ "PriceF")
 )
 [''Price]
 (const $ Just applicativeBCR)
 (const [])
 )
-- | Functor parametrized version of PurchData
$(metamorphosis
 ( (return)
 . (fdTypes %~ ("f":))
 . (fdFieldName . mapped %~ ("pdf"++). drop 9)
 . (fdBang .~ Bang NoSourceUnpackedness NoSourceStrictness)
 . (fdTConsName .~ "PurchDataF")
 )
 [''PurchData]
 (const $ Just applicativeBCR)
 (const [])
 )

-- | Sales Price information
data ItemPriceF f = ItemPriceF (IntMap (f Double))
instance Semigroup (ItemPriceF f) where
   (ItemPriceF a) <> (ItemPriceF b) = ItemPriceF $ a <> b
instance Monoid (ItemPriceF f) where
  mempty = ItemPriceF mempty
  mappend = (<>)

-- | FA status
data ItemStatusF f = ItemStatusF
  { isfQoh :: f Int -- real Qoh 
  , isfAllQoh :: f Int -- Qoh including lost location 
  , isfOnDemand :: f Int -- active sales
  , isfAllOnDemand :: f Int -- sales including cancelled or expired
  , isfOnOrder :: f Int -- coming
  , isfUsed :: f Bool -- has been used
  } 

-- | Web Status
data ItemWebStatusF f = ItemWebStatusF
  { iwfProductDisplay :: f (Maybe Text)
  , iwfActive :: f Bool
  }
-- | Information hold in item index
-- aggregate of stock master table, sales and purchase prices
-- as well as FA and web status
data ItemMasterAndPrices f = ItemMasterAndPrices
  { impMaster :: Maybe (StockMasterF f)
  , impSalesPrices :: Maybe (IntMap (PriceF f))
  , impPurchasePrices :: Maybe (IntMap (PurchDataF f))
  , impFAStatus :: Maybe (ItemStatusF f)
  , impWebStatus :: Maybe (ItemWebStatusF f)
  , impWebPrices :: Maybe (ItemPriceF f)
  } 

-- ** Instances
deriving instance Show (StockMasterF Identity)
deriving instance Show (PriceF Identity)
deriving instance Show (PurchDataF Identity)
deriving instance Show (ItemStatusF Identity)
deriving instance Show (ItemWebStatusF Identity)
deriving instance Show (ItemPriceF Identity)
deriving instance Show (StockMasterF ((,) [Text]))
deriving instance Show (PriceF ((,) [Text]))
deriving instance Show (PurchDataF ((,) [Text]))
deriving instance Show (ItemStatusF ((,) [Text]))
deriving instance Show (ItemPriceF ((,) [Text]))
deriving instance Show (ItemWebStatusF ((,) [Text]))
deriving instance Show (ItemMasterAndPrices Identity)
deriving instance Show (ItemMasterAndPrices ((,) [Text]))
deriving instance Eq (StockMasterF Identity)
deriving instance Eq (PriceF Identity)
deriving instance Eq (PurchDataF Identity)
deriving instance Eq (ItemStatusF Identity)
deriving instance Eq (ItemWebStatusF Identity)
deriving instance Eq (ItemPriceF Identity)
deriving instance Eq (StockMasterF ((,) [Text]))
deriving instance Eq (PriceF ((,) [Text]))
deriving instance Eq (PurchDataF ((,) [Text]))
deriving instance Eq (ItemStatusF ((,) [Text]))
deriving instance Eq (ItemPriceF ((,) [Text]))
deriving instance Eq (ItemWebStatusF ((,) [Text]))
deriving instance Eq (ItemMasterAndPrices Identity)
deriving instance Eq (ItemMasterAndPrices ((,) [Text]))
 
instance Monoid (ItemMasterAndPrices f) where
  mempty = ItemMasterAndPrices Nothing Nothing Nothing Nothing Nothing Nothing
  mappend (ItemMasterAndPrices m s p st ws wp)
          (ItemMasterAndPrices m' s' p' st' ws' wp')
     = ItemMasterAndPrices (m <|> m') (s <|> s') (p <|> p') (st <|> st') (ws <|> ws') (wp <|> wp')

-- | Whereas an item is running or not.
data FARunningStatus = FARunning -- ^ can and need to be sold
                    | FAAsleep -- ^ Not used but still present in cancelled or expired order/location. Probably needs cleaning up before set to inactive.

                    | FADead -- ^ Not used anymore but can't be deleted because of exists in previous trans.
                    | FAGhost -- ^ In the system but can be deleted.
                    deriving (Show, Read, Eq)


-- * Reporting
type Quantity = Double
type Amount = Double
-- | Quantity and Price
-- to make it a semigroup we need to also store the amount
data QPrice = QPrice
  { qpIO :: InOutward
  , _qpQty :: Quantity
  , _qpAmount :: Amount
  , qpPrice :: MinMax Amount
  } deriving (Eq, Ord, Show)

mkQPrice io qty price = QPrice io qty (qty*price) (pure $ abs price)
qpQty io qp = _qpQty (qpTo io qp)
qpAmount io qp = _qpAmount (qpTo io qp)
qpAveragePrice qp = _qpAmount qp / _qpQty qp
qpMinPrice qp = m where (MinMax m _) = qpPrice qp
qpMaxPrice qp = m where (MinMax _ m ) = qpPrice qp



mulQP :: Double -> QPrice  -> QPrice
mulQP m (QPrice io qty amount price  ) =  QPrice io (qty*m) (amount*m) price

-- | Change the in/ouward field and update other values accordingly
qpTo :: InOutward -> QPrice -> QPrice
qpTo io qp | io == qpIO qp = qp
           | otherwise = QPrice io (- (_qpQty qp)) (-(_qpAmount qp)) (qpPrice qp)

instance Semigroup QPrice where
  (QPrice io q a mm) <> qp' = QPrice io (q+q') (a+a') (mm <> mm')
    where (QPrice _ q' a' mm') = qpTo io qp'

instance Monoid QPrice where
  mempty = QPrice Inward 0 0 mempty 
  mappend = (<>)

-- | Specifies whether the quantities are seen  as inward or outward (from the company point of view)
-- Positive quantities for Inward transaction means we are getting more in
-- For example, a Sales is outward, whereas a customer credit note in inward (<0) or (outward <0)
data InOutward = Inward | Outward deriving (Show, Read, Eq, Ord, Enum, Bounded) 

-- | Type of transaction corresponding to a QPrice
-- Types are actually not disjoint, and can be converted from one (sub)type to another one.
-- | example Sales Credit -> Credit ,or Sales Credit to Sales
data QPType = QPSales | QPPurchase | QPAdjustment | QPCredit | QPInvoice 
            | QPSalesInvoice | QPSalesCredit | QPPurchInvoice | QPPurchCredit
            | QPSummary
  deriving(Read, Show, Eq, Ord, Enum, Bounded)

-- | Quantity price for a transaction, either sales or purchase
-- A lazy map depending on the QPType.
-- The same price can correspond to differnt QPTypes
-- example Sales, Invoice, Summary
-- the map is created at construction (but only calculated on demand)
newtype TranQP = TranQP (LMap.Map QPType  QPrice)
 deriving (Ord, Eq)

instance Show TranQP where
  show (TranQP tmap) = "TranQP (" <> show tmap <> ")"

instance Semigroup TranQP where
  (TranQP tqp) <> (TranQP tqp') = TranQP (LMap.unionWith (<>) tqp tqp')
  -- (TranQP s p a) <> (TranQP s' p' a') = TranQP (go s s') (go p p') (go a a') where
  --   go Nothing b = b
  --   go a Nothing = a
  --   go (Just a) (Just b) = Just $ a <> b
instance Monoid TranQP where
  mempty = TranQP mempty
  mappend = (<>)
-- type TranQP = Map QPType QPrice
tranQP :: QPType -> QPrice -> TranQP
tranQP qtype qp = let
  qtypes = case qtype of
                   QPSalesInvoice -> [QPSales, QPInvoice ]
                   QPSalesCredit -> [QPSales, QPCredit]
                   QPPurchInvoice -> [QPPurchase, QPInvoice]
                   QPPurchCredit -> [QPPurchase, QPCredit]
                   QPAdjustment -> [QPAdjustment]
                   QPSummary -> [minBound..maxBound]
                   _ -> error $ "Fix ME: creating with TranQP with " <> show qtype
  in TranQP $ LMap.fromList (map (, qp) (qtype:QPSummary:qtypes))
                   

-- | Projection
lookupGrouped qtype (TranQP tmap) = LMap.lookup qtype tmap

salesQPrice = lookupGrouped QPSales
purchQPrice = lookupGrouped QPPurchase
summaryQPPrice = lookupGrouped QPPurchase
adjQPrice = lookupGrouped QPAdjustment


mulTranQP :: Double -> TranQP -> TranQP
mulTranQP m (TranQP qmap) = TranQP (fmap (mulQP m) qmap)

-- | Create a TranQP from list of QPType QPrice
collapseQPs :: [(QPType, QPrice)] -> Maybe TranQP
collapseQPs [] = Nothing
collapseQPs tqs = Just . mconcat $ map (uncurry tranQP) tqs
  

-- | Key to hold TranQP information
data TranKey = TranKey
  { tkDay :: Day
  , tkCustomerSupplier :: Maybe (Either (Int64, Int64) Int64)
  , tkSku :: Text
  , tkStyle :: Maybe Text
  , tkVar :: Maybe Text
  , tkCategory :: Map Text Text
  , tkType :: FATransType
  } deriving (Show, Eq, Ord)

tkCustomer :: TranKey -> Maybe (Int64, Int64)
tkCustomer = (either Just (const Nothing)) <=< tkCustomerSupplier
tkSupplier :: TranKey -> Maybe Int64
tkSupplier = (either (const Nothing) Just) <=< tkCustomerSupplier

-- * Nested Map with pseudo-heterogenous key
-- we use here PersistValue as a Sum type containing
-- the main basic types as well hav having an Ord instance
-- This should be much more efficient than converting everything to Text.
-- The key holds an optional rank part, which allows to the Map to be sorted
-- by something different from the actual key.
data NMapKey = NMapKey { nkRank :: Maybe PersistValue
                       , nkKey :: PersistValue
                       } deriving (Eq, Ord, Show)

mkNMapKey :: PersistValue -> NMapKey
mkNMapKey v = NMapKey Nothing v

data NMap a = NMap a [Maybe Text] (Map NMapKey (NMap a))
            | NLeaf a
            deriving (Show, Eq)

nmapToMap :: NMap a -> Map NMapKey (NMap a)
nmapToMap (NMap _ _ m) = m
nmapToMap nmap@(NLeaf _) = Map.singleton (mkNMapKey PersistNull) nmap

-- | Names of the different keys of a NMap
nmapLevels :: NMap a -> [Maybe Text]
nmapLevels (NMap _ levels _) = levels
nmapLevels (NLeaf _) = []

nmapMargin :: NMap a -> a
nmapMargin (NLeaf x) = x
nmapMargin (NMap x _ _) = x

nmapFromList :: (Monoid a) => Maybe Text -> [(NMapKey, a)] -> NMap a
nmapFromList level xs = let
  m = Map.fromListWith mappend (map (fmap NLeaf) xs)
  in NMap (mconcat $ map nmapMargin $ toList m) [level] m

groupAsNMap :: Monoid v => [(Maybe Text, k -> NMapKey)] -> [(k, v)] -> NMap v
groupAsNMap [] trans  = NLeaf (mconcat $ map snd trans)
groupAsNMap gs@((level, grouper):groupers) trans = let
  grouped = groupAsMap (grouper . fst) (:[]) trans
  m = fmap (groupAsNMap groupers) grouped
  in NMap (mconcat $ map nmapMargin $ toList m)(map fst gs) m
 
data RankMode = RMResidual -- replace residuals with concat
              -- | RMResidualAvg -- replace residuals with avg
              -- | RMTotal -- add concat everything
              -- | RMAverage -- add avg everything
              -- | RMBests -- replace best with concat
              -- | RMBestAvg -- replace best with average
              | RMBestAndRes -- replace best and tops with
              deriving (Eq, Ord, Show, Enum, Bounded)

sortAndLimit :: (Ord r, Monoid a)
             => [Maybe ( NMapKey ->  a -> r
                       , Maybe RankMode
                       , Maybe Int)
                ]  -> NMap a -> NMap a
sortAndLimit  [] nmap = nmap
sortAndLimit  _ (NLeaf a) = NLeaf a
sortAndLimit  (Nothing:cols) (NMap mr levels m) = NMap mr levels (fmap (sortAndLimit cols) m)
sortAndLimit  (Just (sortFn, modeM, limitM):cols) (NMap mr levels m) = let
  sorted = map snd $ sortOn fst [ ((sortFn key (nmapMargin nmap)),  (key, nmap))
                                | (key, nmap) <- Map.toList m
                                ]
  (bests, residuals) = case limitM of
    Nothing -> (sorted, [])
    Just limit ->  splitAt limit sorted

  limited = makeResidual modeM bests residuals
  ranked = [ (NMapKey (Just $ PersistInt64 i) key, sortAndLimit cols n)
           | (i, (NMapKey _ key, n)) <- zip [1..] limited
           ]
  in NMap mr (levels) (Map.fromList $ ranked)


-- makeResidual :: Maybe RankMode -> [NMap ] -> []
makeResidual :: Monoid a
             => Maybe RankMode
             -> [(NMapKey, NMap a)] -- bests
             -> [(NMapKey, NMap a)] -- last
             -> [(NMapKey, NMap a)]
makeResidual Nothing bests residuals = bests
makeResidual (Just RMResidual) bests residuals = bests <> [aggregateResiduals "Last" residuals]
makeResidual (Just RMBestAndRes) bests residuals =
  [aggregateResiduals "Top" bests] <> [aggregateResiduals "Last" residuals]



aggregateResiduals :: Monoid a
                   => Text  -> [(NMapKey, NMap a)] -> (NMapKey, NMap a)
aggregateResiduals title key'nmaps = ( NMapKey Nothing (PersistText $ title <> "-" <> tshow (length key'nmaps))
                                     , mconcat $ map snd key'nmaps
                                     )
-- nmapFromNMaps :: Semigroup a => Maybe Text -> [(NMapKey, NMap a)] -> NMap a
-- nmapFromNMaps level nmaps = NMap 

-- m,n :: NMap Text
-- m = NMap [Just "level1"] (mapFromList [(PersistText "x", NLeaf "1")])
-- n = NLeaf "a"

appendNMap :: (a -> a -> a) -> NMap a -> NMap a -> NMap a
appendNMap appN (NLeaf x) (NLeaf y) =  NLeaf (x `appN` y)
appendNMap appN (NMap mr ls m) (NMap mr' ls' m') = let
  mergeLevels ls ls' = take (max (length ls) (length ls'))
                      $ zipWith mergeLevel (ls <> repeat Nothing) (ls' <> repeat Nothing)
  mergeLevel Nothing l = l
  mergeLevel l Nothing = l
  mergeLevel (Just l) (Just l') | l == l' = Just l
                                | otherwise = Just $ l <> "|" <> l'
  in normalizeNMap $ NMap (mr `appN` mr')
                       (mergeLevels ls ls')
                       (unionWith (appendNMap appN) m m')
appendNMap appN  n n' = normalizeNMap $ appendNMap appN 
  (NMap (nmapMargin n)
        (nmapLevels n)
        (nmapToMap n))
  (NMap (nmapMargin n')
        (nmapLevels n')
        (nmapToMap n'))

instance Semigroup a=> Semigroup (NMap a) where
  (<>) = appendNMap (<>)

normalizeNMap n@(NMap _ [] m) = case Map.toList m of
  [(_,  n')] -> normalizeNMap n'
  _ -> n
normalizeNMap n = n 

instance Monoid a => Monoid (NMap a) where
  mappend = appendNMap mappend
  mempty = NMap mempty [] mempty

instance Functor NMap where
  fmap f (NMap mr ls m) = NMap (f mr) ls (fmap (fmap f) m)
  fmap f (NLeaf x) = NLeaf (f x)

-- Monotraversable
type instance Element (NMap a) = a
instance MonoFunctor (NMap a)
instance MonoFoldable (NMap a)
instance Foldable.Foldable NMap where
  foldr f b (NMap _ _ m) = foldr (flip (foldr f) ) b m
  foldr f b (NLeaf l) = f l b


nmapToNMapList :: NMap a -> [(NMapKey, NMap a)]
nmapToNMapList = Map.toList . nmapToMap

nmapToList :: NMap a -> [([NMapKey], a)]
nmapToList (NLeaf x) = [([], x)] 
nmapToList (NMap _ _ m) = [ (key : subkeys, es)
                      | (key, nmap) <- Map.toList m
                      , (subkeys, es)<- nmapToList nmap
                      ]
