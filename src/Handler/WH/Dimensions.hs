{-# LANGUAGE ViewPatterns #-}
module Handler.WH.Dimensions
( getWHDimensionR
, getWHDimensionOuterR
, getWHDimensionInnerR
, getWHDimensionBulkR
, postWHDimensionBulkR
) where

import Yesod.Form.Bootstrap3
import Database.Persist.MySQL
import WarehousePlanner.Base hiding(rotate,up)
import WarehousePlanner.Move 
import qualified WarehousePlanner.Base as W
import Import
import qualified Yesod.Media.Simple as M
import Diagrams.Prelude hiding(iso, offset,dims,outer,pre,view,text)
import Diagrams.Backend.Cairo
import Data.Text (strip, split)
import Data.Maybe (fromJust)
-- import Linear.Affine ((.-.))

-- * Types 
data Facet = Facet
  { _unused_offset :: Dimension
  , _unused_background :: Maybe (Colour Double)
  , _unused_points :: [Dimension]
  }

-- | Positionated Dimension (+ offset and orientation)
data PDimension = PDimension
  { _unused_poffset :: Dimension
  , _unused_pDim :: Dimension
  , _unused_pOr :: Orientation 
  }


-- * Forms 
singleForm = renderBootstrap3 BootstrapBasicForm form
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
      Just style_ -> Right style_


bulkForm text = renderBootstrap3 BootstrapBasicForm form where
  form = unTextarea <$> areq textareaField (bfs ("dimensions" :: Text)) (fmap Textarea text)
  
-- * Requests 

mkDimension l w h = Dimension (c l) (c w) (c h) where
  c = fromIntegral
{-# NOINLINE getWHDimensionOuterR #-}
getWHDimensionOuterR :: Int64 -> Int64 -> Int64 -> Handler TypedContent
getWHDimensionOuterR l w h = renderImage (mkDimension l w h) Nothing

{-# NOINLINE getWHDimensionInnerR #-}
getWHDimensionInnerR :: Int64 -> Int64 -> Int64
                    ->Int64 -> Int64 -> Int64
                    ->  Handler TypedContent
getWHDimensionInnerR l w h il iw ih = renderImage (mkDimension l w h) (Just $ mkDimension il iw ih)



renderImage :: Dimension -> Maybe Dimension -> Handler TypedContent
renderImage outer inner = do
  imgW <- lookupGetParam "width"
  -- let size_ = mkWidth (fromMaybe 800 $ imgW >>= readMay)
  let w = fromMaybe 800 $ imgW >>= readMay 
      size_ = dims2D w w
      diag = displayBox outer inner
  M.renderContent (M.SizedDiagram size_ diag)

{-# NOINLINE getWHDimensionR #-}
getWHDimensionR :: Handler Html
getWHDimensionR = renderDimensionR 

renderDimensionR = do
  setInfo ("Start style with  ! or < (inner box volume) to use inner boxes")
  ((resp, form), encType) <- runFormGet singleForm
  boxes <- case resp of
    FormMissing -> return []
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{form}|]
    FormSuccess param -> case validateParam param of
      (Left  (outer, inner)) -> return [("" :: Text, outer, inner)]
      (Right style) -> loadBoxForStyles style


  defaultLayout [whamlet|
<form#get-dimensions role=form method=get action=@{WarehouseR WHDimensionR} enctype=#{encType}>
  <div.well>
    ^{form}
    <button.btn.btn-default type="submit" name> Process
  <div.well>
    ^{renderBoxes boxes}
|]

{-# NOINLINE getWHDimensionBulkR #-}
getWHDimensionBulkR :: Handler Html
getWHDimensionBulkR = do
  renderBulk []


{-# NOINLINE postWHDimensionBulkR #-}
postWHDimensionBulkR :: Handler Html
postWHDimensionBulkR = do
  ((resp, view), _encType) <- runFormPost (bulkForm Nothing)
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> do
      setError $ "FormFailure:" >> mapM_ toHtml a
      sendResponseStatus (toEnum 400) =<< defaultLayout [whamlet|^{view}|]
    FormSuccess text -> do
      actionM <- lookupPostParam "action"
      let boxes = rotateBox actionM $ parseBoxList text
      renderBulk boxes
     
  
renderBulk :: [(Text, Dimension, Maybe Dimension)] -> Handler Html
renderBulk boxes = do
  (form, encType) <- generateFormPost (bulkForm $ Just $ boxesToText boxes)
  defaultLayout [whamlet|
<form#get-dimensions role=form method=POST action=@{WarehouseR WHDimensionBulkR} enctype=#{encType}>
  <div.well>
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
        <th> Height
        <th> Volume
        <th> Image
    <tbody>
      $forall (index, (style, outer, inner)) <- zip ns boxes
        $with  gapm <- fmap (snd . innerBoxes outer) inner
            <tr>
              <td> #{style}
              <td> 
                 <button.btn.btn-transparent type=submit name=action value="ol/#{index}"> #{formatDouble (dLength outer)}
                 $maybe il <- fmap dLength inner
                   <br>
                   <button.btn.btn-transparent type=submit name=action value="il/#{index}"> #{formatDouble il}
                 $maybe gap <- gapm
                   <br>
                   (+#{formatDouble (dLength gap)})
              <td>
                 <button.btn.btn-transparent type=submit name=action value="ow/#{index}">#{formatDouble (dWidth outer)}
                 $maybe iw <- fmap dWidth inner
                   <br>
                   <button.btn.btn-transparent type=submit name=action value="iw/#{index}"> #{formatDouble iw}
                 $maybe gap <- gapm
                   <br>
                   (+#{formatDouble (dWidth gap)})
              <td>
                 <button.btn.btn-transparent type=submit name=action value="oh/#{index}">#{formatDouble (dHeight outer)}
                 $maybe ih <- fmap dHeight inner
                   <br>
                   <button.btn.btn-transparent type=submit name=action value="ih/#{index}"> #{formatDouble ih}
                 $maybe gap <- gapm
                   <br>
                   (+#{formatDouble (dHeight gap)})
              <td> #{formatVolume (volume outer / 1000000)}
                 $maybe i <- inner
                   <br> #{formatVolume (volume i / 1000000)}
              <td><a href="@{routeFor outer inner}" ><img src=@?{(routeFor outer inner, [("width", "128")])}>
|] where ns = [0 :: Int ..]
  

-- * Misc 
-- TODO  remove quick hack 
loadBoxForStyles :: Text -> Handler [(Text, Dimension, Maybe Dimension)]
loadBoxForStyles (stripPrefix "!" -> Just style) = do
  let sql = "SELECT style, count(owidth) n, `oLength` l, oWidth w , oHeight h"
          <> " , iLength il, iWidth iw, iHeight ih"
          <> " FROM box_dimensions"
          <> " WHERE style like ?"
          <> " GROUP BY style, l, w, h, il, iw, ih "
          <> " ORDER by n DESC "
          <> " LIMIT 200" :: Text
      go (Single s, Single c, Single l, Single w, Single h, Single il, Single iw, Single ih)
        = (s <> " ("  <> tshow (c::Int) <> ")" , Dimension l w h, liftA3 Dimension il iw ih )
  takes <- runDB $ rawSql sql [PersistText style]
  return $ map go takes
loadBoxForStyles (stripPrefix "<" -> Just vol) = do
  let sql = "SELECT style, count(owidth) n, `oLength` l, oWidth w , oHeight h"
          <> " , iLength il, iWidth iw, iHeight ih"
          <> " FROM box_dimensions"
          <> " WHERE IF(iLength=0, oLength*oWidth*oHeight,iLength*iWidth*iHeight)/1000000 <= ?"
          <> " GROUP BY style, l, w, h, il, iw, ih "
          <> " ORDER by (itemPerBox*volume) DESC "
          <> " LIMIT 200" :: Text
      go (Single s, Single c, Single l, Single w, Single h, Single il, Single iw, Single ih)
        = (s <> " ("  <> tshow (c::Int) <> ")" , Dimension l w h, liftA3 Dimension il iw ih )
  takes <- runDB $ rawSql sql [PersistDouble (fromJust $ readMay vol)]
  return $ map go takes
loadBoxForStyles style =  do
  let sql = "SELECT LEFT(description,8) as style, count(width) n, `length` l, width w , height h"
          <> " FROM fames_boxtake"
          <> " WHERE description like ?"
          <> " GROUP BY style, l, w, h "
          <> " ORDER by n DESC "
          <> " LIMIT 200" :: Text
      go (Single s, Single c, Single l, Single w, Single h)
        = (s <> " ("  <> tshow (c::Int) <> ")" , Dimension l w h, Nothing)
  takes <- runDB $ rawSql sql [PersistText style]
  return $ map go takes
  

parseBoxList :: Text -> [(Text, Dimension, Maybe Dimension)]
parseBoxList text = mapMaybe go (lines text)
  where go line =
          chooseInner <$> case split (`elem` (",x\t" :: [Char])) (strip line) of
            [l,w,h] -> Just ("", mkDim0 line l w h, Nothing)
            [s,l,w,h] -> Just (s, mkDim0 line l w h, Nothing)
            [s,l,w,h,""] -> Just (s, mkDim0 line l w h, Nothing)
            ["","","", il, iw, ih] -> Just ("", (mkDim0 line il iw ih), Nothing)
            [l,w,h, il, iw, ih] -> Just ("", mkDim0 line l w h, (mkDimM il iw ih))
            [s, "","","", il, iw, ih] -> Just (s, (mkDim0 line il iw ih), Nothing)
            [s,l,w,h, il, iw, ih] -> Just (s, mkDim0 line l w h, (mkDimM il iw ih))
            _ -> Nothing
        c = readMay . (stripSuffix' "cm") . toLower :: Text -> Maybe Double
        stripSuffix' pre s = fromMaybe s $ stripSuffix pre s
        mkDim0 line l w h = fromMaybe (error $ "Can't parse dimension for " ++ unpack line) $ mkDimM l w h
        mkDimM l w h = liftA3 Dimension (c l) (c w) (c h)
        -- swap inner and outer boxes if necessary
        chooseInner (style, b1, Just b2) | volume b1 < volume b2 = (style, b2, Just b1)
        chooseInner x                                            = x
                    
boxesToText :: [(Text, Dimension, Maybe Dimension)] -> Text
boxesToText = unlines . map go where
  go (style, outer, innerm) = intercalate "\t" 
                                          $ style : dimToTexts outer ++ maybe [] dimToTexts innerm
  dimToTexts (Dimension l w h) = map (pack . formatDouble) [l, w, h]


rotateBox :: Maybe Text -> [(Text, Dimension, Maybe Dimension)] -> [(Text, Dimension, Maybe Dimension)] 
rotateBox Nothing boxes = boxes
rotateBox (Just action) boxes = let
  (what, lineS) = splitAt 3 action
  process (style, outer@(Dimension l w h), innerm) =
      case what of
        "ol/" -> -- turn 
          (style, dimension w h l, innerm)
        "ow/" -> -- set widh to top
          (style, dimension h l w, innerm)
        _ | Just (Dimension li wi hi) <- innerm ->
          case what of
          "il/" -> (style, outer, Just $ dimension hi wi li)
          "iw/" -> (style, outer, Just $ dimension hi li wi)
          _ -> (style, outer, innerm)
        _ -> (style, outer, innerm)
  -- create a Dimension where length is bigger that width
  dimension l0 w0 h = Dimension l w h where [w,l] = sort [l0, w0]
  in case readMay lineS of
      Just line | (before, box:after) <- splitAt line boxes -> before ++ [process box] ++ after
      _ -> boxes


routeFor (Dimension l w h) Nothing = WarehouseR $ WHDimensionOuterR ((fromIntegral.round) l) (round w) (round h)
routeFor (Dimension l w h) (Just (Dimension il iw ih))
       =WarehouseR $ WHDimensionInnerR (round l) (round w) (round h) (round il) (round iw) (round ih)
-- * Diagrams 

displayBox :: Dimension -> Maybe Dimension -> Diagram Cairo
displayBox outer (Just (Dimension 0 0 0))   = displayBox outer Nothing
displayBox outer innerm   = let
  disp = map displayFacetISO
  (fg_ , facets) = case innerm of
             -- no inner, or in fact no outer ...
             Nothing -> ([], innerBoxToFacets W.up outer)
             Just inner -> let
               background = outerBoxToFacets outer
               foreground = outerBoxToFacets' outer
               (inners, _) = innerBoxes outer inner
               middle = mconcat $ map innerPToFacets inners
               in (foreground , middle <> background)
  d = mconcat $ (disp fg_  # lc (blend 0.5 outerColourNY black) # dashing [13, 5] 0 )
               <> disp facets
  z = [ lineFromVertices (map iso [(Dimension 0 0 0), o])
      | o <- [Dimension 100 0 0, Dimension 0 100 0, Dimension 0 0 100 ]
      ]
  in d <> (mconcat $ map stroke z) # lc red

-- | Calculate 
innerBoxes :: Dimension -> Dimension -> ([PDimension], Dimension) -- ^ (dimension+offset, gaps)
innerBoxes outer@(Dimension lo wo ho) inner =
  let orientations = zipWith3 (\o minW maxW -> OrientationStrategy o minW maxW Nothing Nothing False)
                              allOrientations
                              (repeat 0)
                              (repeat 6)
      -- try with shrunk box by 1 cm first
      best0 = bestArrangement orientations [(outer, outer, ())] inner
      shrink (Dimension x y z) = Dimension (x-1) (y-1) (z-1)
      best@(_,tilingMode,_) = bestArrangement orientations [(shrink outer, shrink outer, ())] inner
      (ori, (Regular (HowMany _ nl nw nh)), _) = if tmTotal tilingMode > 0
                                then best
                                else best0
      (Dimension l w h) = W.rotate ori inner
      gap = Dimension (lo - fromIntegral nl*l) (wo - fromIntegral nw * w) ( ho - fromIntegral nh * h)
  in (reverse $ [ PDimension (Dimension l0 w0 h0) inner ori
               | iw <- [1..nw]
               , ih <- [1..nh]
               , il <- [1..nl]
               , let [l0, w0, h0] = zipWith (\d i -> d* fromIntegral (i-1)) [l,w,h] [il, iw, ih]
               ]
     , gap
     )


outerBoxToFacets :: Dimension -> [Facet]
outerBoxToFacets (Dimension l w h) = let
  bottom = Facet mempty (Just outerColourNZ)  $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  backside = Facet mempty (Just outerColourNY)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  back = Facet mempty (Just outerColourNX) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  _unused_front = Facet mempty Nothing $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
  _unused_frontside = Facet mempty Nothing  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  _unused_up = Facet (Dimension 0 0 h) Nothing  $ toDims [(0,0,0), (l,0,0), (l,w/2,0), (0,w/2,0)]
  in  reverse [bottom, backside, back]

outerBoxToFacets' :: Dimension -> [Facet]
outerBoxToFacets' (Dimension l w h) = let
  _unused_bottom = Facet mempty (Just outerColourNZ)  $ toDims [(0,0,0), (l,0,0), (l,w,0), (0,w,0)]
  _unused_backside = Facet (Dimension 0 0 0 ) (Just outerColourNX)  $ toDims [(l,0,0), (l,w,0), (l,w,h), (l,0,h)]
  _unused_back = Facet mempty (Just outerColourNY) $ toDims [(0,w,0), (l,w,0), (l,w,h), (0,w,h)]
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
  go (Orientation Depth Horizontal) = front 2 1 1 ++  side 1 1 1 ++ top 1 1 1
  go (Orientation Vertical Vertical) = error "should not happen"
  go (Orientation Depth Depth ) = error "should not happen"
  go (Orientation Horizontal Horizontal ) = error "should not happen"
  in go opening

-- halfL f@(Dimension l w h) = let
--   f' = f { gg
  

innerPToFacets :: PDimension -> [Facet]
innerPToFacets (PDimension  off dim ori) = let
  facets = innerBoxToFacets ori (W.rotate ori dim)
  in map (translateFacet off) facets
  

 
_unused_rotateFacet ori (Facet off bg_ dims) = Facet (W.rotate ori off) bg_ (map (W.rotate ori) dims)
translateFacet off0 (Facet off1 bg_ dims) = Facet (off0 `mappend` off1) bg_ dims

facetCenter (Facet offset _ points) = let
  Dimension gl gw gh = mconcat points 
  n = fromIntegral (length points)
  in offset `mappend` (Dimension (gl/n) (gw/n) (gh/n))

_unused_facetZ f =
  let Dimension l w _h = facetCenter f
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
_unused_cos60 = cos (pi/3)
_unused_sin60 = sin (pi/3)
iso :: Dimension -> Point V2 Double 
iso (Dimension l w h) = x ^& y  where
  x =  0 - l * cos30 + w * cos30
  y = 0 - w * sin30 - l * sin30 + h

