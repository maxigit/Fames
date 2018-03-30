{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module WarehousePlanner.Display where

import Prelude
import WarehousePlanner.Base
import Diagrams.Prelude hiding(Box)
import Control.Monad.State
import Diagrams.Backend.Cairo 
import Data.Maybe
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
    return $ pad (1.05) $ cat direction (map alignB rendered)
    where cat  Vertical = vcat 
          cat  Horizontal = hcat
          cat  Depth = hcat -- should not happen
    
renderGroup (ShelfProxy i) = do
    shelf <- findShelf i
    renderShelf shelf

renderSlices :: ShelfGroup s -> WH [Diagram B] s
renderSlices (ShelfGroup gs direction) = mapM renderGroup gs
renderSlices shelf@(ShelfProxy _) = do
    diag <- renderGroup shelf
    return [diag]

renderShelf :: Shelf s -> WH (Diagram B) s
renderShelf shelf = do            
    (_, used) <- usedDepth shelf
    (fgM, bgM) <- gets shelfColors `ap` return shelf 
    let (Dimension l _w h) = maxDim shelf
        (Dimension ln wn hn) = minDim shelf
        rmax = rect l h # lc royalblue # lwL 2 -- # fc white
        isSeparator = wn < 10
        rmin = rect ln hn # fc (fromMaybe (if isSeparator then white else lightsteelblue) fgM)
                          # lwL 0
                          # translate (r2 (- (l-ln)/2, -(h-hn)/2))
        -- r = rmin `atop` rmax
        r = t' `atop` rmax`atop` rmin  
        t = scaledText 50 20 (shelfName shelf) `atop` rect l 15 # fc (fromMaybe darkorange bgM) # lc royalblue # lwL 2
        -- display the depth bar relative to the full length,as that's what we are losing
        wn' = ln
        bar = if not isSeparator then depthBar wn' (used*wn'/wn) else mempty
        bar' = (alignL bar # translateX 5) `atop` alignL t
        t' = scaledText ln hn (shelfName shelf) #lc darkblue
        diagram = r -- t `atop` r
        align = case flow shelf of
                    LeftToRight -> alignBL
                    RightToLeft -> alignBR

    boxes <- renderBoxes shelf
    

    return $ alignBL $ centerX ((align boxes) `atop` (align diagram)) === (centerX bar')

renderBoxes :: Shelf s -> WH (Diagram B) s
renderBoxes shelf = let 
    in do
        boxes <- findBoxByShelf shelf
        z'diags <-  mapM (renderBox shelf) boxes
        let zMap = Map.fromListWith atop (concat z'diags)
        return $ foldl atop mempty (Map.elems zMap)

depthBar :: Double -> Double -> Diagram B
depthBar w used = let
  shelfBar = depthBar'' (lc black . lwL 1) w 0 lightsteelblue
  usedBar = depthBar'' (lc black . lwL 1)  used 0 darkorange
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
depthBar' width offset colour = depthBar'' (lwL 1 . lc colour) width offset colour
-- depthBar' = depthBar'' (lwL 1) --  . lc colour) width offset colour
depthBar'' :: (Diagram B -> Diagram B) -> Double -> Double -> Colour Double -> Diagram B
depthBar'' up l l0 colour = translate (r2 (scale l0, 0)) . alignBL $ rect (scale l) 4 # fc colour # up
  where scale x = x /3

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
    let   Dimension xn yn zn = minDim shelf
          -- Dimension xx yx zx = maxDim shelf
          Dimension l w h = boxDim box
          Dimension ox oy oz = boxOffset box
          foreground = if (ox+l) > xn || (oy+w) > yn || (oz+h) > zn
                  then firebrick -- stick out
                  else black
    background <-  gets colors `ap` return box
    let   r = rect l h  # lc foreground # fc background # lwL 2 #scale 0.95 # pad 1.05
          t = scaledText l h (boxStyle box ++ "\n" ++ showOrientation (orientation box) ++ " " ++ boxContent box)
          diagram_ = t `atop` r
          diagram = offsetBox True shelf box diagram_

          boxBar =  renderBoxBar box background
          backBag = renderBoxBarBg shelf

          -- offset and scale to 
          offsetBar bar = offsetBox False shelf box $ bar # translate (r2 (5,5) ) 
    return $ [(3, diagram),
              (2, offsetBar backBag),
              (1, offsetBar boxBar)]

renderBoxBar :: Box s -> Colour Double -> Diagram B
renderBoxBar box background =
  let Dimension _l w _h = boxDim box
      Dimension _ox oy _oz = boxOffset box
  in depthBar'' (lwL 1 . lc $ blend 0.5  black background ) w oy (background)
  -- in depthBar'  w oy (background)

    
renderBoxBarBg :: Shelf s -> Diagram B
renderBoxBarBg shelf = gaugeBar yn
    where   Dimension _xn yn _zn = minDim shelf
    
-- | draw text scaled to fit given rectangle
-- scaledText :: Double -> Double-> String -> Diagram b
scaledText x y s =  let
    (x0, y0) = (10,3) 
    (sX, sY) =  (x/x0, y/y0)
    in text s & scale (min sX sY)


