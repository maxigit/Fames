{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module WarehousePlanner.Display where

import ClassyPrelude
import WarehousePlanner.Base hiding(up)
import Diagrams.Prelude hiding(Box,offset,direction)
import Control.Monad.State(get, gets)
import Diagrams.Backend.Cairo 
-- import Data.Maybe
import qualified Data.Map as Map

display :: WH (Diagram B) s
display = do
    warehouse <- get
    g <- renderGroup (shelfGroup warehouse)
    s0 <- defaultShelf
    base <- renderGroup (ShelfProxy (s0))
    return $ g === base
    

renderGroup :: ShelfGroup s -> WH (Diagram B) s
renderGroup (ShelfGroup gs direction) = do
    rendered <- mapM renderGroup gs
    return $ pad (1.05) $ cat_ direction (map alignB rendered)
    where cat_  Vertical = vcat 
          cat_  Horizontal = hcat
          cat_  Depth = hcat -- should not happen
    
renderGroup (ShelfProxy i) = do
    shelf <- findShelf i
    renderShelf shelf

renderSlices :: ShelfGroup s -> WH [Diagram B] s
renderSlices (ShelfGroup gs __direction) = mapM renderGroup gs
renderSlices shelf@(ShelfProxy _) = do
    diag <- renderGroup shelf
    return [diag]

renderShelf :: Shelf s -> WH (Diagram B) s
renderShelf shelf = do            
    (_, used) <- usedDepth shelf
    styling@ShelfStyling{..} <- gets shelfStyling <*> return shelf
    let (Dimension l __w h) = maxDim shelf
        (Dimension ln wn hn) = minDim shelf
        rmax = rect l h # lc border # lwL 2 -- # fc white
        rmin = rect ln hn # fc background
                          # lwL 0
                          # translate (r2 (- (l-ln)/2, -(h-hn)/2))
        -- r = rmin `atop` rmax
        r = t' `atop` rmax`atop` rmin  
        shelfTitle = case title of
          [] -> shelfName shelf
          _ -> intercalate "\n" title
        barTitle_ = fromMaybe (shelfName shelf) barTitle
        t = scaledText 50 20 barTitle_  #fc barForeground `atop` rect l 15 # fc (barBackground) # lc border # lwL 2
        -- display the depth bar relative to the full length,as that's what we are losing
        wn' = ln
        bar = if displayBarGauge then depthBar styling wn' (used*wn'/wn) else mempty
        bar' = (alignL bar # translateX 5) `atop` alignL t
        t' = scaledText ln hn shelfTitle #fc foreground 
        diagram = r -- t `atop` r
        align_ = case flow shelf of
                    LeftToRight -> alignBL
                    RightToLeft -> alignBR

    boxes <- renderBoxes shelf
    

    return $ alignBL $ centerX ((align_ boxes) `atop` (align_ diagram)) === (centerX bar')

renderBoxes :: Shelf s -> WH (Diagram B) s
renderBoxes shelf = let 
    in do
        boxes <- findBoxByShelf shelf
        z'diags <-  mapM (renderBox shelf) boxes
        let zMap = Map.fromListWith atop (concat z'diags)
        return $ foldl' atop (rect 0 0 ) (Map.elems zMap)
        --                   ^ necessary so to force the envelope to include (0,0)
        --                   so that the offset of boxes is not cancelled
        --                   when align diagrams

depthBar :: ShelfStyling -> Double -> Double -> Diagram B
depthBar ShelfStyling{..} w used = let
  shelfBar = depthBar'' (lc black . lwL 1) w 0 background
  usedBar = depthBar'' (lc black . lwL 1)  used 0 barBackground
  -- threshold1 = depthBar' (w*0.70) 0 red 
  -- threshold2 = depthBar' (w*0.85) 0 orange 
  in mconcat ( map alignL [ usedBar
                          -- , threshold1 
                          -- , threshold2 
                          , shelfBar 
                          ]
             )

gaugeBar :: Double -> Diagram B
gaugeBar w = let
  shelfBar = depthBar' w 0 green
  threshold1 = depthBar' (w*0.70) 0 red 
  threshold2 = depthBar' (w*0.85) 0 orange 
  in mconcat ( map alignL [ threshold1 
                          , threshold2 
                          , shelfBar 
                          ]
             )
depthBar' :: Double -> Double -> Colour Double -> Diagram B
depthBar' width_ offset colour = depthBar'' (lwL 1 . lc colour) width_ offset colour
-- depthBar' = depthBar'' (lwL 1) --  . lc colour) width_ offset colour
depthBar'' :: (Diagram B -> Diagram B) -> Double -> Double -> Colour Double -> Diagram B
depthBar'' up l l0 colour = translate (r2 (scale_ l0, 0)) . alignBL $ rect (scale_ l) 4 # fc colour # up
  where scale_ x = x /3

-- | render a Box within a shelf. 
-- Boxes are stack vertically until they reach the maximum.
-- to do so, we need to keep track of the postion of the last 
-- box

offsetBox :: Bool -> Shelf s -> Box s -> Diagram B -> Diagram B
offsetBox fromBoxCenter shelf box diagram  = let
    Dimension xn _yn _zn = minDim shelf
    -- Dimension xx yx zx = maxDim shelf
    Dimension l _w h = if fromBoxCenter then boxDim box else Dimension 0 0 0
    Dimension ox _oy oz = boxOffset box

    offset LeftToRight =  (ox+l/2, oz+h/2)
    offset RightToLeft = (xn -ox+l/2, oz+h/2) 

          
    in diagram # translate (r2 (offset (flow shelf)))

renderBox :: Shelf s -> Box s -> WH [(Int, Diagram B)] s
renderBox shelf box = do
    BoxStyling{..} <- gets boxStyling `ap` return box
    let   Dimension l __w h = boxDim box
          border' = fromMaybe foreground border
          titles = case title of
                     [] -> (boxStyle box <> "\n" <> showOrientation (orientation box) <> " " <> boxContent box)
                     _ -> unlines title  
                        
    let   r = (r' $ rect l h  # lc border' # fc background # lwL 2) #scale 0.95 # pad 1.05
          r' = case background2 of
                 Nothing -> id
                 Just bg2 -> \r0 -> circle (min l h / 3) # fc bg2 #lwL 0 `atop` r0
          t = scaledText l h titles
              # fc foreground
          diagram_ = t `atop` r
          diagram = offsetBox True shelf box diagram_

          boxBar =  renderBoxBar box barTitle foreground background border' background2
          backBag = if displayBarGauge
                    then renderBoxBarBg shelf
                    else mempty

          -- offset and scale to 
          Dimension _ ow' _ = boxCoordinate box
          oy = fromMaybe 0 offsetBarGaugeY * (ow' -1)
          ox = fromMaybe 0 offsetBarGaugeX * (ow' -1)
               

          offsetBar bar = offsetBox False shelf box $ bar # translate (r2 (5-ox,5-oy) ) 
    return $ [(3, diagram),
              (2, offsetBar backBag),
              (1, offsetBar boxBar)]

renderBoxBar :: Box s -> Maybe Text
             -> Colour Double -> Colour Double -> Colour Double -> Maybe (Colour Double)
             -> Diagram B
renderBoxBar box titlem foreground background border circlem =
  let Dimension _l w _h = boxDim box
      Dimension _ox oy _oz = boxOffset box
      c = maybe mempty (\col -> circle (3/2) # lwL 0 # fc col ) circlem
      t = maybe mempty (fc foreground . scaledText w w)  titlem  `atop` c
  in depthBar'' ((atop t) . (lwL 1 . lc $ blend 0.5  border background )) w oy (background)
  -- in depthBar'  w oy (background)

    
renderBoxBarBg :: Shelf s -> Diagram B
renderBoxBarBg shelf = gaugeBar yn
    where   Dimension _xn yn _zn = minDim shelf
    
-- | draw text scaled to fit given rectangle
scaledText :: Double -> Double-> Text -> Diagram B
scaledText x y s =  let
    (x0, y0) = (10,3) 
    (sX, sY) =  (x/x0, y/y0)
    in text (unpack s) & scale (min sX sY)


