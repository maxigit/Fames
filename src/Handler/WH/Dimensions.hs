module Handler.WH.Dimensions
( getWHDimensionR
, getWHDimensionOuterR
, getWHDimensionInnerR
, getWHDimensionBulkR
, postWHDimensionBulkR
) where

import Yesod.Form.Bootstrap3
import WarehousePlanner.Base hiding(rotate)
import qualified WarehousePlanner.Base as W
import Import
import qualified Yesod.Media.Simple as M
import Diagrams.Prelude hiding(iso)
import Diagrams.Backend.Cairo
import Data.Ord(comparing)
import Data.Text (splitOn, strip)
import Data.Maybe (fromJust)
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

-- * Forms
singleForm outer inner = renderBootstrap3 BootstrapBasicForm form
  where
    form = (,,,,,,) <$> (areq intField "outer length" Nothing)
              <*> (areq intField "outer width" Nothing)
              <*> (areq intField "outer height" Nothing)
              <*> (aopt intField "inner length" Nothing)
              <*> (aopt intField "inner width" Nothing)
              <*> (aopt intField "inner height" Nothing)
              <*> (aopt textField "style" (Nothing))
validateParam :: (Int64, Int64, Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Text)
              -> Either (Dimension, Maybe Dimension) Text
validateParam (l,w,h,il,iw,ih,style) =
  case style of
      Nothing -> Left ( Dimension (fromIntegral l) (fromIntegral w) (fromIntegral h)
                      , Dimension <$> (fromIntegral <$> il) <*> (fromIntegral <$> iw) <*> (fromIntegral <$> ih)
                      )
      Just style -> Right style


bulkForm text = renderBootstrap3 BootstrapBasicForm form where
  form = unTextarea <$> areq textareaField "dimensions" (fmap Textarea text)
  
-- * Requests

mkDimension l w h = Dimension (c l) (c w) (c h) where
  c = fromIntegral
getWHDimensionOuterR :: Int64 -> Int64 -> Int64 -> Handler TypedContent
getWHDimensionOuterR l w h = renderImage (mkDimension l w h) Nothing

getWHDimensionInnerR :: Int64 -> Int64 -> Int64
                    ->Int64 -> Int64 -> Int64
                    ->  Handler TypedContent
getWHDimensionInnerR l w h il iw ih = renderImage (mkDimension l w h) (Just $ mkDimension il iw ih)



renderImage :: Dimension -> Maybe Dimension -> Handler TypedContent
renderImage outer inner = do
  imgW <- lookupGetParam "width"
  -- let size = mkWidth (fromMaybe 800 $ imgW >>= readMay)
  let w = fromMaybe 800 $ imgW >>= readMay 
      size = dims2D w w
      diag = displayBox outer inner
  M.renderContent (M.SizedDiagram size diag)

getWHDimensionR :: Handler Html
getWHDimensionR = renderDimensionR Nothing Nothing

renderDimensionR outer0 inner0 = do
  ((resp, form), encType) <- runFormGet (singleForm outer0 inner0)
  boxes <- case resp of
    FormMissing -> return []
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{form}|]
    FormSuccess param -> case validateParam param of
      (Left  (outer, inner)) -> return [("" :: Text, outer, inner)]
      (Right style) -> loadBoxForStyles style


  defaultLayout [whamlet|
<div.well>
  <form#get-dimensions role=form method=get action=@{WarehouseR WHDimensionR} enctype=#{encType}>
    ^{form}
    <button.btn.btn-default type="submit" name> Process
<div.well>
  ^{renderBoxes boxes}
|]

getWHDimensionBulkR :: Handler Html
getWHDimensionBulkR = do
  renderBulk Nothing


postWHDimensionBulkR :: Handler Html
postWHDimensionBulkR = do
  ((resp, view), _encType) <- runFormPost (bulkForm Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess text -> do
      renderBulk (Just text)
     
  
renderBulk :: Maybe Text -> Handler Html
renderBulk text = do
  (form, encType) <- generateFormPost (bulkForm text)
  let boxes = maybe [] parseBoxList text
  defaultLayout [whamlet|
<div.well>
  <form#get-dimensions role=form method=POST action=@{WarehouseR WHDimensionBulkR} enctype=#{encType}>
    ^{form}
    <button.btn.btn-default type="submit" name> Process
<div.well>
  ^{renderBoxes boxes}
|]

renderBoxes :: [(Text, Dimension, Maybe Dimension)] -> Widget
renderBoxes boxes = [whamlet|
<div>
  <table.table.table-striped.table-border.table-hover>
    <thead>
      <tr>
        <th> Style
        <th> Length
        <th> Width
        <th> Length
        <th> Image
    <tbody>
      $forall (style, outer, inner) <- boxes
        <tr>
          <td> #{style}
          <td> #{formatDouble (dLength outer)}
             $maybe il <- fmap dLength inner
               <br>
               #{formatDouble il}
          <td> #{formatDouble (dWidth outer)}
             $maybe iw <- fmap dWidth inner
               <br>
               #{formatDouble iw}
          <td> #{formatDouble (dHeight outer)}
             $maybe ih <- fmap dHeight inner
               <br>
               #{formatDouble ih}
          <td><a href="@{routeFor outer inner}" ><img src=@?{(routeFor outer inner, [("width", "128")])}>
|]
  

-- * Misc
loadBoxForStyles style = return [ ("a", Dimension 31 34 77, Nothing)
                         , ("b", Dimension 60 39 25, Just (Dimension 38 23 10))
                         ]

parseBoxList :: Text -> [(Text, Dimension, Maybe Dimension)]
parseBoxList text = mapMaybe go (traceShowId $ lines text)
  where go line =
          case splitOn "," (strip line) of
            [l,w,h] -> Just ("", mkDim0 l w h, Nothing)
            [s,l,w,h] -> Just (s, mkDim0 l w h, Nothing)
            [l,w,h, il, iw, ih] -> Just ("", mkDim0 l w h, (mkDimM il iw ih))
            [s,l,w,h, il, iw, ih] -> Just (s, mkDim0 l w h, (mkDimM il iw ih))
            _ -> Nothing
        c = readMay :: Text -> Maybe Double
        mkDim0 l w h = fromJust $ mkDimM l w h
        mkDimM l w h = liftA3 Dimension (c l) (c w) (c h)

routeFor (Dimension l w h) Nothing = WarehouseR $ WHDimensionOuterR ((fromIntegral.round) l) (round w) (round h)
routeFor (Dimension l w h) (Just (Dimension il iw ih))
       =WarehouseR $ WHDimensionInnerR (round l) (round w) (round h) (round il) (round iw) (round ih)
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
  in mconcat $ (disp fg_  # lc (blend 0.5 outerColourNY black) # dashing [13, 5] 0 )
               <> disp facets

-- | Calculate 
innerBoxes :: Dimension -> Dimension -> [PDimension]
innerBoxes outer inner =
  let orientations = zip allOrientations (repeat 6)
      best = bestArrangement orientations [(outer, ())] inner
      (ori, nl, nw, nh, _) = traceShow ("BEST-new", outer, inner, best) best
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
  backside = Facet mempty (Just outerColourNY)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet mempty (Just outerColourNX) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
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
  top = facet (Dimension 0 0 h) (Just innerColourNZ) [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  front = facet (Dimension 0 w 0) (Just innerColourNX) [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  side = facet (Dimension l 0 0) (Just innerColourNY)  [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
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
  go (Orientation Horizontal Depth) = front 1 1 1 ++  side 1 2 1 ++ top 1 1 1
  go (Orientation Horizontal Vertical) = front 1 1 1 ++  side 1 1 2 ++ top 1 1 1
  go (Orientation Depth Vertical) = front 1 1 2 ++  side 1 1 1 ++ top 1 1 1
  in go $ traceShowId opening

-- halfL f@(Dimension l w h) = let
--   f' = f { gg
  

innerPToFacets :: PDimension -> [Facet]
innerPToFacets (PDimension  off dim ori) = let
  facets = innerBoxToFacets ori (W.rotate ori dim)
  in map (translateFacet off) facets
  

 
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
-- outerColourNX = sRGB24 158 221 225
outerColourNY = sRGB24 60 183 192
outerColourNX = blend 0.5 outerColourNY outerColourNZ -- sRGB24 224 201 153
outerColourNZ = sRGB24 224 244 245

innerColourNY = sRGB24 192 148 59
innerColourNX = blend 0.5 innerColourNY innerColourNZ -- sRGB24 224 201 153
innerColourNZ = sRGB24 235 219 189

cos30 = cos (pi/6)
sin30 = sin (pi/6)
cos60 = cos (pi/3)
sin60 = sin (pi/3)
iso :: Dimension -> Point V2 Double 
iso (Dimension l w h) = x ^& y  where
  x =  0 - l * cos30 + w * cos30
  y = 0 - w * sin30 - l * sin30 + h

