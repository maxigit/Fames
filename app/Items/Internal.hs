-- | Main pure functions related to items ...
module Items.Internal where

import Prelude
import Items.Types

  
-- | Check for each styles if they all variations present in variations
-- variations
joinStyleVariations :: [ItemInfo a] -> [ItemInfo b] -> [(VariationStatus , ItemInfo a)]
joinStyleVariations styles var = zip (cycle $ replicate 3 VarOk
                                              ++ [VarMissing]
                                              ++ replicate 5 VarOk
                                              ++ [VarExtra]
                                     )
                                     styles

  

