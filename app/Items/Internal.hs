{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, StandaloneDeriving #-}
-- | Main pure functions related to items ...
module Items.Internal where

import ClassyPrelude
import Items.Types
import Data.These
import Data.Align(align)
import qualified Data.Map as Map
import Metamorphosis
import FA
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Monoid as Monoid
import Database.Persist
import Data.Copointed(Copointed,copoint)

diffField :: Eq a => Identity a -> Identity a -> ((,) [Text]) a
diffField (Identity a) (Identity b) = if a == b then ([], a) else (["text-danger"], b)

setDanger :: Identity a -> ((,) [Text]) a
setDanger (Identity a) = (["text-danger"], a)
setInfo :: Identity a -> ((,) [Text]) a
setInfo (Identity a) = (["text-info"], a)
setWarn :: Identity a -> ((,) [Text]) a
setWarn (Identity a) = (["text-warning"], a)
setPure :: Identity a -> ((,) [Text] a)
setPure (Identity a) = ([], a)
-- Generates diffFieldStockMasterF 
$(mmZip "diffField" ''StockMasterF)
$(mmZip "diffField" ''PriceF)
$(mmZip "diffField" ''PurchDataF)
$(mmZip "diffField" ''ItemWebStatusF)
$(mmZipN 1 "setDanger" ''PriceF Nothing)
$(mmZipN 1 "setInfo" ''PriceF Nothing)
$(mmZipN 1 "setWarn" ''PriceF Nothing)
$(mmZipN 1 "setDanger" ''PurchDataF Nothing)
$(mmZipN 1 "setInfo" ''PurchDataF Nothing)
$(mmZipN 1 "setWarn" ''PurchDataF Nothing)
$(mmZipN 1 "setPure" ''ItemStatusF Nothing)
$(mmZipN 1 "setPure" ''ItemWebStatusF Nothing)


-- * MinMax
minMax :: a -> MinMax a
minMax a = MinMax a a
 
-- $(mmZip "mappend" ''StockMasterF)

  -- mappend = mappendStockMasterF

-- * Diff
-- | Check the status of an item variation given a list of expected variation
-- As well as checking if the variation exists, or is extra is also compares it
-- to a reference items, to check if the information are the same.
-- | The reference item used to check if fields are different between the current variation
-- and the reference needs to be adjusted depending on the variation to take into account
-- fields which actual depends on the variation itself. For example, the description might
-- contain the name of the variation. In that case, we need to adjust the description field
-- to match the expected result (not the reference one)
-- For example, if base is Black and the description is "Black T-Shirt". We will need to change
-- the description to "Red T-Shirt" for the Red variation. 
computeItemsStatus :: (ItemInfo a -> Text -> ItemInfo a )
                   -> (ItemInfo a -> ItemInfo a -> ItemInfo diff)
                   -> ItemInfo a
                   -> [Text]
                   -> [ItemInfo a]
                   -> [(VariationStatus, ItemInfo diff)]
computeItemsStatus adjustItem0 computeDiff_ item0 varMap items = let
  styleMap = mapFromList [(iiVariation i, i) | i <- items ]
  varMap' = mapFromList [(var, adjustItemBase var) | var <- varMap]
  joineds = align varMap' styleMap
  ok _ item = (VarOk,  item)
  r = map (these (VarMissing,) (VarExtra,) ok) (Map.elems joineds)
  adjustItemBase var = (adjustItem0 item0 var) {iiVariation = var} -- | Force variation
  in map (\(s, i) -> (s, computeDiff_ (adjustItemBase (iiVariation i)) i)) r

computeDiff :: ItemInfo (ItemMasterAndPrices Identity)
            -> ItemInfo (ItemMasterAndPrices Identity)
            -> ItemInfo (ItemMasterAndPrices ((,) [Text]))
computeDiff item0 item@(ItemInfo style var master) = let
  [i0, i] = (map (impMaster . iiInfo)  [item0, item]) :: [Maybe (StockMasterF Identity)]
  [s0, s] = (map (fromMaybe mempty .impSalesPrices . iiInfo)  [item0, item]) :: [(IntMap (PriceF Identity))]
  [p0, p] = (map (fromMaybe mempty .impPurchasePrices . iiInfo)  [item0, item]) :: [(IntMap (PurchDataF Identity))]
  [ItemPriceF wp0, ItemPriceF wp] = (map (fromMaybe mempty .impWebPrices . iiInfo)  [item0, item]) :: [ItemPriceF Identity]
  [ws0, ws] = (map (impWebStatus . iiInfo)  [item0, item]) :: [Maybe (ItemWebStatusF Identity)]
  -- we don't want to compare the web status to the base item, but to the FA Status
  -- if something is running, it should be available on the website
  -- However, if FAStatus is not present (ie, product doesn't exits) should be equivalent
  -- to disabled
  ws0' = faToWebStatus style (impFAStatus master)
  diff = ItemMasterAndPrices (diffFieldStockMasterF <$>  i0 <*> i)
                             (Just $ diffPriceMap s0 s )
                             (Just $ diffPurchMap p0 p )
                             (setPureItemStatusF1 <$> impFAStatus master)
                             (diffFieldItemWebStatusF <$> Just ws0' <*> ws)
                             (Just . ItemPriceF $ diffWebPriceMap wp0 wp)

  in ItemInfo style var (diff)

-- diffMap ::((f Identity) -> (f Identity) -> (f ((,) [Text])))
        -- -> IntMap (f Identity) -> IntMap (f Identity) -> IntMap (f ((,) [Text]))
diffPriceMap a b = let
  aligned = align a b
  in these setWarnPriceF1 setInfoPriceF1 diffFieldPriceF <$> aligned
diffPurchMap a b = let
  aligned = align a b
  in these setWarnPurchDataF1 setInfoPurchDataF1 diffFieldPurchDataF <$> aligned
diffWebPriceMap a b = let
  aligned = align a b
  in these setWarn setInfo diffField <$> aligned

-- | Convert a FrontAccounting status to the expected Status on the Website  
faToWebStatus :: Text -> Maybe (ItemStatusF Identity) -> ItemWebStatusF Identity
faToWebStatus style fa = case faRunningStatus <$> fa of
  Just (Identity FARunning) -> ItemWebStatusF (pure (Just style)) (pure True)
  _ -> ItemWebStatusF (pure Nothing) (pure False)
  
-- * Join variations
-- | Computes the VariationStatus ie if variations are present
-- or not in the given map. "Missing" .i.e where the variation
-- is not present in the variation map will be created from item0
  
-- | Check for each styles if they all variations present in variations
-- variations
-- joinStyleVariations :: [ItemInfo a] -> [ItemInfo b] -> [(VariationStatus , ItemInfo a)]
joinStyleVariations :: Map Text (Text, Text)
                    -> (ItemInfo a -> Text -> ItemInfo a)
                    -> (ItemInfo a -> ItemInfo a -> ItemInfo diff)
                    -> [ItemInfo a]
                    -> [Text] -- ^ all possible variations
                    -> [( ItemInfo a
                        , [(VariationStatus, ItemInfo diff)]
                        )]
joinStyleVariations bases adjustBase computeDiff_ items vars = let
  styles = Map.fromListWith (flip (<>))  [(iiStyle item, [item]) | item <- items]

  in map (\(_, variations@(var:_)) -> let
             -- bases Map a style to the base sku. We need to find the sku then item
             varMap = mapFromList [((iiStyle v, iiVariation v), v) | v <- variations ]
             base = fromMaybe var $ Map.lookup (iiStyle var) bases
               >>= flip Map.lookup varMap
             in ( base
                , computeItemsStatus adjustBase computeDiff_
                  base
                  vars
                  variations
                )
         )
         (mapToList styles)


mergeInfoSources :: Monoid a => [[ItemInfo a]] -> [ItemInfo a]
mergeInfoSources sources = let
  maps :: [Map (ItemInfo ()) _a]
  maps = map (\source -> mapFromList [ ( fmap (const ()) info
                                       ,  iiInfo info 
                                       )
                                     | info <- source
                                     ]
             ) sources
  merged = foldl' (unionWith (Monoid.<>)) (mempty) maps
  in [ ItemInfo style var i | (ItemInfo style var (), i) <- Map.toList merged]
  

-- * Columns
pricesColumns field masters =
  let colSetFor m  = keysSet m
      cols = mapMaybe (fmap colSetFor . field ) masters
      colSet = mconcat cols
  in sort $ IntSet.toList colSet

salesPricesColumns ::  [ItemMasterAndPrices f] -> [Int]
salesPricesColumns masters = pricesColumns impSalesPrices masters

purchasePricesColumns ::  [ItemMasterAndPrices f] -> [Int]
purchasePricesColumns masters = pricesColumns impPurchasePrices masters
  

-- * Status
faRunningStatus :: Applicative f => ItemStatusF f -> f FARunningStatus
faRunningStatus ItemStatusF{..} = let
  go qoh onOrder_  allQoh  onOrder  allOnDemand  used = case () of
    _ | qoh  > 0 || onOrder_ > 0 -> FARunning
    _ | allQoh /= 0 || onOrder > 0 || allOnDemand > 0 -> FAAsleep
    _ | used == True -> FADead
    _ -> FAGhost -- nothing is using it. It can be deleted
  in go <$> isfQoh <*> isfOnOrder <*> isfAllQoh <*> isfOnOrder <*> isfAllOnDemand <*> isfUsed 
      
  

-- * Prices
-- | Computes  theoretical prices based on default price and price list info.
-- | Doesn't touch prices if given
computeTheoreticalPrices :: Int -> [Entity SalesType] -> IntMap Double -> IntMap Double
computeTheoreticalPrices baseId priceLists priceMap = let
  go base (Entity key pl) = let
    pId = unSalesTypeKey key
    in ( pId
       ,
         if pId == baseId
         then base
         else fromMaybe (fromInteger (round (base * salesTypeFactor pl * 100))/100) (lookup pId priceMap)
       )
   in case lookup baseId priceMap of
        Nothing -> priceMap -- nothing we can do, return the current mag
        Just base -> mapFromList (map (go base) priceLists)


computeTheoreticalPricesF :: Int -> [Entity SalesType] -> IntMap (PriceF Identity) -> ItemPriceF Identity
computeTheoreticalPricesF baseId priceLists priceMap = let
  priceMap' = fmap (runIdentity . pfPrice ) priceMap
  result =  computeTheoreticalPrices  baseId priceLists priceMap'
  in ItemPriceF (fmap Identity result)

computeTheoreticalPricesP :: Copointed f =>
  Int -> [Entity SalesType] -> IntMap (PriceF f) -> IntMap Double
computeTheoreticalPricesP baseId priceLists priceMap = let
  priceMap' = fmap (copoint . pfPrice) priceMap
  in computeTheoreticalPrices baseId priceLists priceMap'

masterPrice :: Copointed f => Int -> ItemMasterAndPrices f -> Maybe Double
masterPrice baseId master = do -- Maybe
  prices <- impSalesPrices master
  priceF <- lookup baseId prices
  return . copoint $ pfPrice priceF


