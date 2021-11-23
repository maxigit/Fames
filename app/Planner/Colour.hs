{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Planner.Colour
( readFromPalette
)
where

import Data.Colour.Palette.BrewerSet
import Data.Colour.Palette.ColorSet
import ClassyPrelude
import Prelude((!!))


readFromPalette :: Text -> Maybe Kolor
readFromPalette name =
  case break (== '-')  name of
    (paletteName, (uncons -> Just ('-', indexs))) | Just i <- readMay indexs -> let
                              palettem = readBrewerSet paletteName <|> readColorSet paletteName
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
    [(n,  cat)] -> Just (\i -> brewerSet cat n !! (i `min` n))
    _ -> Nothing
    
-- * Read from Palette.ColorSet 
readColorSet :: Text -> Maybe (Int -> Kolor)
readColorSet "rybColor" = Just (\i -> rybColor (i `mod` 24))
readColorSet "wheel24" = Just (\i -> rybColor (i `mod` 24))
readColorSet "d3Colors1" = Just (\i -> d3Colors1 (i `mod` 10))
readColorSet "d3Colors21" = Just (\i -> d3Colors2 Light (i `mod` 10))
readColorSet "d3Colors22" = Just (\i -> d3Colors2 Dark (i `mod` 10))
readColorSet "d3Colors41" = Just (\i -> d3Colors4 Lightest (i `mod` 10))
readColorSet "d3Colors42" = Just (\i -> d3Colors4 Light (i `mod` 10))
readColorSet "d3Colors43" = Just (\i -> d3Colors4 Dark (i `mod` 10))
readColorSet "d3Colors44" = Just (\i -> d3Colors4 Darkest (i `mod` 10))
readColorSet _ = Nothing




