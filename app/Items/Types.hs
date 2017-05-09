{-# LANGUAGE DeriveGeneric #-}
module Items.Types where

import ClassyPrelude
import Metamorphosis
import Metamorphosis.Applicative
import FA
import Language.Haskell.TH.Syntax
import Lens.Micro
import qualified GHC.Generics as GHC

-- | Holder for miscellaneous information relative to an item.
-- Allows mainly to call operation which need to group by style and or variations.
data ItemInfo a = ItemInfo
  { iiStyle :: Text
  , iiVariation:: Text
  , iiInfo :: a
  }
deriving instance (Show a) => Show (ItemInfo a)
deriving instance (Eq a) => Eq (ItemInfo a)
deriving instance (Ord a) => Ord (ItemInfo a)


data VariationStatus= VarOk | VarMissing | VarExtra deriving (Eq, Show, Read, Ord)

type FieldWithClasses a = ([Text], a)
-- | Functor parameterized version of StockMaster
$(metamorphosis
 ( (return)
 . (fdTypes %~ ("f":))
 . (fdFieldName . mapped %~ ("smi"++). drop 11)
 . (fdBang .~ Bang NoSourceUnpackedness NoSourceStrictness)
 . (fdTConsName .~ "StockMasterInfo")
 )
 [''StockMaster]
 (const $ Just applicativeBCR)
 (const [''GHC.Generic])
 )

-- instance Generic (StockMasterInfo f)
