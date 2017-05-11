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
import Data.List (cycle)
import Data.These
import Data.Align(align)
import qualified Data.Map as Map
import Generics.SOP as SOP
import Items.SOP
import FA
  
-- | Check for each styles if they all variations present in variations
-- variations
-- joinStyleVariations :: [ItemInfo a] -> [ItemInfo b] -> [(VariationStatus , ItemInfo a)]
joinStyleVariations :: [ItemInfo StockMaster] -> [ItemInfo StockMaster]
                    -> [( ItemInfo StockMaster
                        , [(VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))]
                        )]
joinStyleVariations items vars = let
  varMap = mapFromList $ map ((,()) . iiVariation) vars
  styles = Map.fromListWith (flip (<>))  [(iiStyle item, [item]) | item <- items]

  in map (\(style, variations) -> let base = headEx variations
                                      in (base, computeItemsStatus (headEx variations)
                                                                   varMap
                                                                   variations )
         )
         (mapToList styles)

-- | Computes the VariationStatus ie if variations are present
-- or not in the given map. "Missing" .i.e where the variation
-- is not present in the variation map will be created from item0
computeItemsStatus :: ItemInfo StockMaster
                   -> Map Text ()
                   -> [ItemInfo StockMaster]
                   -> [(VariationStatus, ItemInfo (StockMasterInfo ((,) [Text])))]
computeItemsStatus item0 varMap items = let
  styleMap = mapFromList [(iiVariation i, i) | i <- items ]
  varMap' = Map.mapWithKey (\var _ -> item0 { iiVariation = var }) varMap
  joineds = align varMap' styleMap
  ok _ item = (VarOk,  item)
  r = map (these (VarMissing,) (VarExtra,) ok) (Map.elems joineds)
  in map (\(s, i) -> (s, computeDiff item0 i)) r


instance SOP.Generic (StockMasterInfo f)
computeDiff :: ItemInfo StockMaster -> ItemInfo StockMaster -> ItemInfo (StockMasterInfo ((,) [Text]))
computeDiff item0 item | iiStyle item0 == iiStyle item  && iiVariation item0 == iiVariation item
      = ItemInfo (iiStyle item0)
                 (iiVariation item0)
                 (runIdentity . aStockMasterToStockMasterInfo . iiInfo $ item0)
computeDiff item0 item@(ItemInfo style var _) = let
  [i0, i] = (map (runIdentity . aStockMasterToStockMasterInfo . iiInfo ) [item0, item]) :: [StockMasterInfo Identity]
  diff = diffInfo i0 i 

  in ItemInfo style var (diff)

diffInfo ::
  forall r xs ys zs .
  ( SOP.Generic (r Identity), SOP.Generic (r ((,) [Text]))
  , Code (r Identity) ~ '[ xs ], Code (r ((,) [Text])) ~ '[ ys ]
  , AllZip (AppEqualR Identity) xs zs  -- xs ~ Map Identity zs
  , AllZip (AppEqualL ((,) [Text])   ) zs ys  -- ys ~ Map Maybe    zs
  , All Eq zs
  ) => r Identity -> r Identity -> r ((,) [Text])
diffInfo a b= to' ( diffInfoNP (from' a) (from' b) :: NP ((,) [Text]) zs)

diffInfoNP :: All Eq xs => NP Identity xs -> NP Identity xs -> NP ((,) [Text]) xs
diffInfoNP = hczipWith (Proxy :: Proxy Eq) diffField
  
diffField :: Eq a => Identity a -> Identity a -> ((,) [Text]) a
diffField (Identity a) (Identity b) = if a == b then ([], a) else (["text-danger"], b)

  
