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
diag  = displayBox (Dimension 31 34 77)


displayBox :: Dimension -> Diagram Cairo
displayBox dim = let
  facets = outerBoxToFacets dim
  in mconcat $ map displayFacetISO facets


outerBoxToFacets :: Dimension -> [Facet]
outerBoxToFacets (Dimension l w h) = let
  bottom = Facet mempty $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  backside = Facet (Dimension l 0 0 ) $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet (Dimension 0 w 0 ) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  -- up = [(0,0,h), (l,0,h), (l,w,h), (0,w,h)]
  toDim (x,y,z) = Dimension x y z
  toDims = map toDim
  in  [bottom, backside, back]

innerBoxToFacets :: Dimension -> [Facet]
innerBoxToFacets = undefined

-- | Display a facet with iso perspective
displayFacetISO :: Facet -> Diagram Cairo
displayFacetISO (Facet offset_ points) = let
  trail = closeLine $ lineFromVertices (map iso points)
  in stroke trail # translate (iso offset_ .-. origin)
  


cos30 = cos (pi/6)
sin30 = sin (pi/6)
cos60 = cos (pi/3)
sin60 = sin (pi/3)
iso :: Dimension -> Point V2 Double 
iso (Dimension l w h) = x ^& y  where
  x = l * cos30 - w * cos30
  y = w * sin30 + l * sin30 + h
