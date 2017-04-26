-- | Main pure functions related to items ...
module Items.Internal where

import ClassyPrelude
import Items.Types
import Data.List (cycle)
import Data.These
import Data.Align(align)
import qualified Data.Map as Map

  
-- | Check for each styles if they all variations present in variations
-- variations
joinStyleVariations :: [ItemInfo a] -> [ItemInfo b] -> [(VariationStatus , ItemInfo a)]
joinStyleVariations items vars = let
  varMap = mapFromList $ map ((,()) . iiVariation) vars
  styles = Map.fromListWith (<>)  [(iiStyle item, [item]) | item <- items]

  in concatMap (\(style, variations) -> computeItemsStatus (headEx variations) varMap variations ) (mapToList styles)

-- | Computes the VariationStatus ie if variations are present
-- or not in the given map. "Missing" .i.e where the variation
-- is not present in the variation map will be created from item0
computeItemsStatus :: ItemInfo a -> Map Text b -> [ItemInfo a] -> [(VariationStatus, ItemInfo a)]
computeItemsStatus item0 varMap items = let
  styleMap = mapFromList [(iiVariation i, i) | i <- items ]
  varMap' = Map.mapWithKey (\var _ -> item0 { iiVariation = var }) varMap
  joineds = align varMap' styleMap
  ok _ item = (VarOk,  item)
  in map (these (VarMissing,) (VarExtra,) ok) (Map.elems joineds)


  
