module Handler.WH.Dimensions (
getWHDimensionR
) where

import Handler.WH.Legacy.Box
import Import
import qualified Yesod.Media.Simple as M
import Diagrams.Prelude hiding(iso)
import Diagrams.Backend.Cairo
-- import Linear.Affine ((.-.))

-- * Types
data Facet = Facet
  { offset :: Dimension
  , background :: Maybe (Colour Double)
  , points :: [Dimension]
  }

-- * Orphan instances
instance Monoid Dimension where
  mempty = Dimension 0 0 0
  mappend (Dimension l w h) (Dimension l' w' h') =
    Dimension (l+l') (w+w') (h+h')

-- * Requests

getWHDimensionR :: Handler TypedContent
getWHDimensionR = do
  let size = mkWidth 800
  M.renderContent (M.SizedDiagram size diag)


-- * Diagrams

diag :: Diagram Cairo
diag  = displayBox (Dimension 31 34 77) (Just $ Dimension 29 23 30)


displayBox :: Dimension -> Maybe Dimension -> Diagram Cairo
displayBox outer inner   = let
  facets = outerBoxToFacets' outer <> maybe [] innerBoxToFacets inner <> outerBoxToFacets outer 
  in mconcat $ map displayFacetISO facets


outerBoxToFacets :: Dimension -> [Facet]
outerBoxToFacets (Dimension l w h) = let
  bottom = Facet mempty (Just outerColourNZ)  $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  backside = Facet (Dimension l 0 0 ) (Just outerColourNX)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet (Dimension 0 w 0 ) (Just outerColourNY) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  front = Facet mempty Nothing $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  frontside = Facet mempty Nothing  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  up = Facet (Dimension 0 0 h) Nothing  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  in  reverse [bottom, backside, back]

outerBoxToFacets' :: Dimension -> [Facet]
outerBoxToFacets' (Dimension l w h) = let
  bottom = Facet mempty (Just outerColourNZ)  $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  backside = Facet (Dimension l 0 0 ) (Just outerColourNX)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet (Dimension 0 w 0 ) (Just outerColourNY) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  front = Facet mempty Nothing $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  frontside = Facet mempty Nothing  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  up = Facet (Dimension 0 0 h) Nothing  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  in  reverse [front, frontside, up]

innerBoxToFacets :: Dimension -> [Facet]
innerBoxToFacets (Dimension l w h) = let
  front = Facet mempty (Just innerColourNY) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  frontside = Facet mempty (Just innerColourNZ)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  up = Facet (Dimension 0 0 h) (Just innerColourNX)  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  up' = Facet (Dimension 0 (w/2) h) (Just innerColourNX)  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  in  reverse [front, frontside, up, up']

toDim (x,y,z) = Dimension x y z
toDims = map toDim

-- | Display a facet with iso perspective
displayFacetISO :: Facet -> Diagram Cairo
displayFacetISO (Facet offset_ bg_ points) = let
  trail = closeLine $ lineFromVertices (map iso points)
  in stroke trail # translate (iso offset_ .-. origin) # (maybe id fc  bg_)

-- ** Colours
outerColourNX = sRGB24 158 221 225
outerColourNY = sRGB24 60 183 192
outerColourNZ = sRGB24 224 244 245

innerColourNY = sRGB24 192 148 59
innerColourNX = sRGB24 224 201 153
innerColourNZ = sRGB24 235 219 189

cos30 = cos (pi/6)
sin30 = sin (pi/6)
cos60 = cos (pi/3)
sin60 = sin (pi/3)
iso :: Dimension -> Point V2 Double 
iso (Dimension l w h) = x ^& y  where
  x = l * cos30 - w * cos30
  y = w * sin30 + l * sin30 + h
