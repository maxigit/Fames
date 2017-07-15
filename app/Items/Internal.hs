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

computeDiff :: ItemInfo StockMaster -> ItemInfo StockMaster -> ItemInfo (StockMasterF ((,) [Text]))
computeDiff item0 item@(ItemInfo style var _) = let
  [i0, i] = (map (runIdentity . aStockMasterToStockMasterF . iiInfo ) [item0, item]) :: [StockMasterF Identity]
  diff = diffFieldStockMasterF i0 i 

  in ItemInfo style var (diff)

-- | Computes the VariationStatus ie if variations are present
-- or not in the given map. "Missing" .i.e where the variation
-- is not present in the variation map will be created from item0
  
-- | Check for each styles if they all variations present in variations
-- variations
-- joinStyleVariations :: [ItemInfo a] -> [ItemInfo b] -> [(VariationStatus , ItemInfo a)]
joinStyleVariations :: Map Text (Text, Text)
                    -> (ItemInfo a -> Text -> ItemInfo a)
                    -> (ItemInfo a -> ItemInfo a -> ItemInfo diff)
                    -> (ItemInfo a -> [ItemInfo a] -> agg)
                    -> [ItemInfo a]
                    -> [Text] -- ^ all possible variations
                    -> [( ItemInfo a
                        , agg
                        , [(VariationStatus, ItemInfo diff)]
                        )]
joinStyleVariations bases adjustBase computeDiff_ aggregateFor items vars = let
  styles = Map.fromListWith (flip (<>))  [(iiStyle item, [item]) | item <- items]

  in map (\(_, variations@(var:_)) -> let
             -- bases Map a style to the base sku. We need to find the sku then item
             varMap = mapFromList [((iiStyle v, iiVariation v), v) | v <- variations ]
             base = fromMaybe var $ Map.lookup (iiStyle var) bases
               >>= flip Map.lookup varMap
             in ( base
                , aggregateFor base variations
                , computeItemsStatus adjustBase computeDiff_
                  base
                  vars
                  variations
                )
         )
         (mapToList styles)

minMaxFor :: ItemInfo StockMaster -> [ItemInfo StockMaster]  -> ItemInfo (StockMasterF MinMax)
minMaxFor (ItemInfo st var _) infos = let
 infos' = map (runIdentity . aStockMasterToStockMasterF . iiInfo) infos
 in ItemInfo st var (mconcat infos')
