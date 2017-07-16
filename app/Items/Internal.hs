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


diffField :: Eq a => Identity a -> Identity a -> ((,) [Text]) a
diffField (Identity a) (Identity b) = if a == b then ([], a) else (["text-danger"], b)

-- Generates diffFieldStockMasterF 
$(mmZip "diffField" ''StockMasterF)

-- -- * MinMax
minMax :: a -> MinMax a
minMax a = MinMax a a
 
-- $(mmZip "mappend" ''StockMasterF)

  -- mappend = mappendStockMasterF


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
computeDiff item0 item@(ItemInfo style var _) = let
  [i0, i] = (map (impMaster . iiInfo)  [item0, item]) :: [Maybe (StockMasterF Identity)]
  [s0, s] = (map (impSalesPrices . iiInfo)  [item0, item]) :: [Maybe (ItemPriceF Identity)]
  diff = ItemMasterAndPrices (diffFieldStockMasterF <$>  i0 <*> i)
                             (diffPrices <$> s0 <*> s)
                             Nothing 

  in ItemInfo style var (diff)

diffPrices :: ItemPriceF Identity -> ItemPriceF Identity -> ItemPriceF ((,) [Text])
diffPrices p0 (ItemPriceF m) = ItemPriceF $ (([], ) . runIdentity) <$> m
  
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
  maps :: [Map (ItemInfo ()) _]
  maps = map (\source -> mapFromList [ ( fmap (const ()) info
                                       ,  iiInfo info 
                                       )
                                     | info <- source
                                     ]
             ) sources
  merged = foldl' (unionWith (Monoid.<>)) (mempty) maps
  in [ ItemInfo style var i | (ItemInfo style var (), i) <- Map.toList merged]
  

salesPricesColumns ::  [ItemMasterAndPrices f] -> [Text]
salesPricesColumns masters =
  let colSetFor (ItemPriceF m)  = keysSet m
      cols = mapMaybe (fmap colSetFor . impSalesPrices) masters
      colSet = mconcat cols
  in map tshow (sort $ IntSet.toList colSet)
  
