module Handler.WH.Dimensions (
getWHDimensionR
) where

import WarehousePlanner.Base hiding(rotate)
import qualified WarehousePlanner.Base as W
import Import
import qualified Yesod.Media.Simple as M
import Diagrams.Prelude hiding(iso)
import Diagrams.Backend.Cairo
import Data.Ord(comparing)
-- import Linear.Affine ((.-.))

-- * Types
data Facet = Facet
  { offset :: Dimension
  , background :: Maybe (Colour Double)
  , points :: [Dimension]
  }

-- | Positionated Dimension (+ offset and orientation)
data PDimension = PDimension
  { poffset :: Dimension
  , pDim :: Dimension
  , pOr :: Orientation 
  }

-- * Requests

getWHDimensionR :: Handler TypedContent
getWHDimensionR = do
  l0 <- lookupGetParam "length"
  w0 <- lookupGetParam "width"
  h0 <- lookupGetParam "height"
  l1 <- lookupGetParam "ilength"
  w1 <- lookupGetParam "iwidth"
  h1 <- lookupGetParam "iheight"
  imgW <- lookupGetParam "imgW"

  let [l,w,h] =  zipWith (fromMaybe) [60,40,20] (map (>>= readMay) [l0,w0,h0] )
      outer = Dimension l w h
  
  let is@[li,wi,hi] =  (map (>>= readMay) [l1,w1,h1] )
      inner = liftA3 Dimension li wi hi
  traceShowM ("INNER", inner, is )
  let size = mkWidth (fromMaybe 800 $ imgW >>= readMay)
      diag = displayBox outer inner
  M.renderContent (M.SizedDiagram size diag)


-- * Diagrams

displayBox :: Dimension -> Maybe Dimension -> Diagram Cairo
displayBox outer innerm   = let
  disp = map displayFacetISO
  (fg_ , facets) = case innerm of
             -- no inner, or in fact no outer ...
             Nothing -> ([], innerBoxToFacets up outer)
             Just inner -> let
               background = outerBoxToFacets outer
               foreground = outerBoxToFacets' outer
               inners = innerBoxes outer inner
               middle = mconcat $ map innerPToFacets inners
               in (foreground , middle <> background)
  sortF = sortBy (comparing (facetZ)) 
  in mconcat $ (disp fg_  # lc (blend 0.5 outerColourNY black) )
               <> disp facets

-- | Calculate 
innerBoxes :: Dimension -> Dimension -> [PDimension]
innerBoxes outer inner =
  let orientations = zip allOrientations (repeat 6)
      best = bestArrangement orientations [(outer, ())] inner
      (ori, nl, nw, nh, _) = traceShow ("BEST", best) best
      (Dimension l w h) = W.rotate ori inner
  in reverse $ [ PDimension (Dimension l0 w0 h0) inner ori
               | iw <- [1..nw]
               , ih <- [1..nh]
               , il <- [1..nl]
               , let [l0, w0, h0] = zipWith (\d i -> d* fromIntegral (i-1)) [l,w,h] [il, iw, ih]
               ]


outerBoxToFacets :: Dimension -> [Facet]
outerBoxToFacets (Dimension l w h) = let
  bottom = Facet mempty (Just outerColourNZ)  $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  backside = Facet mempty (Just outerColourNX)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet mempty (Just outerColourNY) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  front = Facet mempty Nothing $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  frontside = Facet mempty Nothing  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  up = Facet (Dimension 0 0 h) Nothing  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  in  reverse [bottom, backside, back]

outerBoxToFacets' :: Dimension -> [Facet]
outerBoxToFacets' (Dimension l w h) = let
  bottom = Facet mempty (Just outerColourNZ)  $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  backside = Facet (Dimension 0 0 0 ) (Just outerColourNX)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet mempty (Just outerColourNY) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  front = Facet (Dimension 0 w 0) Nothing $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  frontside = Facet (Dimension l 0 0 ) Nothing  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  up = Facet (Dimension 0 0 h) Nothing  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  in  reverse [front, frontside, up]

innerBoxToFacets :: Orientation -> Dimension -> [Facet]
innerBoxToFacets opening (Dimension l w h) = let
  top = facet (Dimension 0 0 h) (Just innerColourNX) [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  front = facet (Dimension 0 w 0) (Just innerColourNY) [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  side = facet (Dimension l 0 0) (Just innerColourNZ)  [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  facet off col points im jm km = [ Facet (off `mappend` offset)  col
                                          [ Dimension  (l'/ls) (w'/ws) (h'/hs)
                                          | (l',w',h') <- points
                                          ]
                                  | i <- [1..im]
                                  , j <- [1..jm]
                                  , k <- [1..km]
                                  , let ls = fromIntegral im
                                        ws = fromIntegral jm
                                        hs = fromIntegral km
                                        offset = ( Dimension (l/ls* fromIntegral (i-1))
                                                   (w/ws * fromIntegral (j-1))
                                                   (h/hs * fromIntegral (k -1))
                                                 )
                                  ]
 

  go (Orientation Vertical Depth) = front 1 1 1 ++  side 1 1 1  ++ top 1 2 1
  go (Orientation Vertical Horizontal) = front 1 1 1 ++  side 1 1 1 ++ top 2 1 1
  go (Orientation Horizontal Depth) = front 1 1 1 ++  side 1 1 2 ++ top 1 1 1
  go (Orientation Horizontal Vertical) = front 1 1 1 ++  side 1 2 1 ++ top 1 1 1
  go (Orientation Depth Vertical) = front 1 1 2 ++  side 1 1 1 ++ top 1 1 1
  in go opening

-- halfL f@(Dimension l w h) = let
--   f' = f { gg
  

innerPToFacets :: PDimension -> [Facet]
innerPToFacets (PDimension  off dim ori) = let
  facets = innerBoxToFacets ori dim
  in map (rotateFacet ori . translateFacet off) facets
  

 
rotateFacet ori (Facet off bg dims) = Facet (W.rotate ori off) bg (map (W.rotate ori) dims)
translateFacet off0 (Facet off1 bg dims) = Facet (off0 `mappend` off1) bg dims

facetCenter (Facet offset _ points) = let
  Dimension gl gw gh = mconcat points 
  n = fromIntegral (length points)
  in offset `mappend` (Dimension (gl/n) (gw/n) (gh/n))

facetZ f =
  let Dimension l w h = facetCenter f
  in (l+w)
  
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
  x =  0 - l * cos30 + w * cos30
  y = 0 - w * sin30 - l * sin30 + h

