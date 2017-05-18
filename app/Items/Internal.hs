{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Main pure functions related to items ...
module Items.Internal where

import ClassyPrelude
import Items.Types
import Data.List (cycle)
import Data.These
import Data.Align(align)
import qualified Data.Map as Map
import Metamorphosis
import FA


diffField :: Eq a => Identity a -> Identity a -> ((,) [Text]) a
diffField (Identity a) (Identity b) = if a == b then ([], a) else (["text-danger"], b)

$(mmZip "diffField" ''StockMasterInfo)

-- -- * MinMax
minMax a = MinMax a a
 
-- $(mmZip "mappend" ''StockMasterInfo)

deriving instance Monoid (StockMasterInfo MinMax) -- where
  -- mappend = mappendStockMasterInfo

computeItemsStatus item0 varMap items = let
  styleMap = mapFromList [(iiVariation i, i) | i <- items ]
  varMap' = Map.mapWithKey (\var _ -> item0 { iiVariation = var }) varMap
  joineds = align varMap' styleMap
  ok _ item = (VarOk,  item)
  r = map (these (VarMissing,) (VarExtra,) ok) (Map.elems joineds)
  in map (\(s, i) -> (s, computeDiff item0 i)) r

computeDiff :: ItemInfo StockMaster -> ItemInfo StockMaster -> ItemInfo (StockMasterInfo ((,) [Text]))
computeDiff item0 item | iiStyle item0 == iiStyle item  && iiVariation item0 == iiVariation item
      = ItemInfo (iiStyle item0)
                 (iiVariation item0)
                 (runIdentity . aStockMasterToStockMasterInfo . iiInfo $ item0)
computeDiff item0 item@(ItemInfo style var _) = let
  [i0, i] = (map (runIdentity . aStockMasterToStockMasterInfo . iiInfo ) [item0, item]) :: [StockMasterInfo Identity]
  diff = diffFieldStockMasterInfo i0 i 

  in ItemInfo style var (diff)

-- | Computes the VariationStatus ie if variations are present
-- or not in the given map. "Missing" .i.e where the variation
-- is not present in the variation map will be created from item0
  
-- | Check for each styles if they all variations present in variations
-- variations
-- joinStyleVariations :: [ItemInfo a] -> [ItemInfo b] -> [(VariationStatus , ItemInfo a)]
joinStyleVariations :: [ItemInfo StockMaster] -> [ItemInfo StockMaster]
                    -> [( ItemInfo StockMaster
                        , ItemInfo (StockMasterInfo MinMax)
                        , [(VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))]
                        )]
joinStyleVariations items vars = let
  varMap = mapFromList $ map ((,()) . iiVariation) vars
  styles = Map.fromListWith (flip (<>))  [(iiStyle item, [item]) | item <- items]

  in map (\(style, variations) -> let base = headEx variations
                                      in ( base
                                         , minMaxFor base variations
                                         , computeItemsStatus (headEx variations)
                                                              varMap
                                                              variations
                                         )
         )
         (mapToList styles)

minMaxFor :: ItemInfo StockMaster -> [ItemInfo StockMaster]  -> ItemInfo (StockMasterInfo MinMax)
minMaxFor (ItemInfo st var _) infos = let
 infos' = map (runIdentity . aStockMasterToStockMasterInfo . iiInfo) infos
 in ItemInfo st var (mconcat infos')
