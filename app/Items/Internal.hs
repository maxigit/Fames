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

-- Generates diffFieldStockMasterInfo 
$(mmZip "diffField" ''StockMasterInfo)

-- -- * MinMax
minMax :: a -> MinMax a
minMax a = MinMax a a
 
-- $(mmZip "mappend" ''StockMasterInfo)

  -- mappend = mappendStockMasterInfo


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
computeItemsStatus :: (ItemInfo StockMaster -> Text -> ItemInfo StockMaster )
                   -> ItemInfo StockMaster
                   -> [Text]
                   -> [ItemInfo StockMaster]
                   -> [(VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))]
computeItemsStatus adjustItem0 item0 varMap items = let
  styleMap = mapFromList [(iiVariation i, i) | i <- items ]
  varMap' = mapFromList [(var, adjustItemBase var) | var <- varMap]
  joineds = align varMap' styleMap
  ok _ item = (VarOk,  item)
  r = map (these (VarMissing,) (VarExtra,) ok) (Map.elems joineds)
  adjustItemBase var = (adjustItem0 item0 var) {iiVariation = var} -- | Force variation
  in map (\(s, i) -> (s, computeDiff (adjustItemBase (iiVariation i)) i)) r

computeDiff :: ItemInfo StockMaster -> ItemInfo StockMaster -> ItemInfo (StockMasterInfo ((,) [Text]))
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
joinStyleVariations :: (ItemInfo StockMaster -> Text -> ItemInfo StockMaster)
                    -> [ItemInfo StockMaster]
                    -> [Text] -- ^ all possible variations
                    -> [( ItemInfo StockMaster
                        , ItemInfo (StockMasterInfo MinMax)
                        , [(VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))]
                        )]
joinStyleVariations adjustBase items vars = let
  styles = Map.fromListWith (flip (<>))  [(iiStyle item, [item]) | item <- items]

  in map (\(_, variations) -> let base = headEx variations
                                      in ( base
                                         , minMaxFor base variations
                                         , computeItemsStatus adjustBase
                                                              (headEx variations)
                                                              vars
                                                              variations
                                         )
         )
         (mapToList styles)

minMaxFor :: ItemInfo StockMaster -> [ItemInfo StockMaster]  -> ItemInfo (StockMasterInfo MinMax)
minMaxFor (ItemInfo st var _) infos = let
 infos' = map (runIdentity . aStockMasterToStockMasterInfo . iiInfo) infos
 in ItemInfo st var (mconcat infos')
