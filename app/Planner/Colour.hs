{-# LANGUAGE StandaloneDeriving #-}
module Planner.Colour
( readFromPalette
)
where

import Data.Colour.Palette.BrewerSet
import ClassyPrelude
import Prelude((!!))


readFromPalette :: Text -> Maybe Kolor
readFromPalette name =
  case break (== '-')  name of
    (paletteName, (uncons -> Just ('-', indexs))) | Just i <- readMay indexs -> let
                              palettem = readBrewerSet paletteName
                              in palettem <*> (Just $ i-1)
    _ -> Nothing
-- * Brewer Set
deriving instance Show ColorCat
deriving instance Enum ColorCat
deriving instance Bounded ColorCat

-- | Read brewer colour  using Palette#size-#colour
-- example "YlGn9-1"
-- We don't check that the palette size is within the boundary
readBrewerSet :: Text -> Maybe (Int -> Kolor)
readBrewerSet name =
  case [ (n, cat)
       | cat <- [minBound..maxBound] :: [ColorCat]
       , (stripPrefix  (tshow cat) -> (Just (readMay -> Just n))) <- [name]
       ] of
    [(n,  cat)] -> Just (\i -> brewerSet cat n !! (i `mod` n))
    _ -> Nothing
    




