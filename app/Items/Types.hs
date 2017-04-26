module Items.Types where

import ClassyPrelude
-- | Holder for miscellaneous information relative to an item.
-- Allows mainly to call operation which need to group by style and or variations.
data ItemInfo a = ItemInfo
  { iiStyle :: Text
  , iiVariation:: Text
  , iiInfo :: a
  }


data VariationStatus= VarOk | VarMissing | VarExtra deriving (Eq, Show, Read)
