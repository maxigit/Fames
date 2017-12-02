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
    let (Dimension l w h) = maxDim shelf
        (Dimension ln wn hn) = minDim shelf
        rmax = rect l h # lc royalblue # lwL 2 -- # fc white
        rmin = rect ln hn # fc (fromMaybe (if wn < 10 then white else lightsteelblue) fgM)
                          # lwL 0
                          # translate (r2 (- (l-ln)/2, -(h-hn)/2))
        -- r = rmin `atop` rmax
        r = t' `atop` rmax`atop` rmin  
        t = scaledText 50 20 (shelfName shelf) `atop` rect l 15 # fc (fromMaybe darkorange bgM) # lc royalblue # lwL 2
        bar = depthBar wn used
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
  shelfBar = depthBar' w 0 green -- rect (w/3) 5 # fc green # lwL 0
  usedBar = depthBar' used 0 lightsteelblue -- rect (used/3) 5 # fc lightsteelblue # lwL 0
  threshold1 = depthBar' (w*0.70) 0 red -- rect (w/3*0.70) 5 # fc red # lwL 0
  threshold2 = depthBar' (w*0.85) 0 orange -- rect (w/3*0.85) 5 # fc orange # lwL 0
  in mconcat ( map alignL [ usedBar
                          , threshold1 
                          , threshold2 
                          , shelfBar 
                          ]
             )

depthBar' :: Double -> Double -> Colour Double -> Diagram B
depthBar' width offset colour = depthBar'' (lwL 1 . lc colour) width offset colour
depthBar'' :: (Diagram B -> Diagram B) -> Double -> Double -> Colour Double -> Diagram B
depthBar'' up l l0 colour = translate (r2 (scale l0, 0)) . alignBL $ rect (scale l) 4 # fc colour # up
  where scale x = x /3

-- | render a Box within a shelf. 
-- Boxes are stack vertically until they reach the maximum.
-- to do so, we need to keep track of the postion of the last 
-- box

offsetBox :: Shelf s -> Box s -> Diagram B -> Diagram B
offsetBox shelf box diagram  = let
    Dimension xn yn zn = minDim shelf
    Dimension xx yx zx = maxDim shelf
    Dimension l w h = boxDim box
    Dimension ox oy oz = boxOffset box

    offset LeftToRight =  (ox+l/2, oz+h/2)
    offset RightToLeft = (xn -ox-l/2, oz+h/2) 

          
    in diagram # translate (r2 (offset (flow shelf)))

renderBox :: Shelf s -> Box s -> WH [(Int, Diagram B)] s
renderBox shelf box = do
    let   Dimension xn yn zn = minDim shelf
          Dimension xx yx zx = maxDim shelf
          Dimension l w h = boxDim box
          Dimension ox oy oz = boxOffset box
          foreground = if (ox+l) > xn || (oy+w) > yn || (oz+h) > zn
                  then firebrick
                  else black
    background <-  gets colors `ap` return box
    let   r = rect l h  # lc foreground # fc background # lwL 2 #scale 0.95 # pad 1.05
          t = scaledText l h (showOrientation (orientation box) ++ boxSku box)
          diagram_ = t `atop` r
          diagram = offsetBox shelf box diagram_

          boxBar =  renderBoxBar box background
          backBag = renderBoxBarBg shelf

          offsetBar bar = bar # translate (r2 (5,5) ) # translate (r2 (ox, oz))
    return $ [(3, diagram),
              (2, offsetBar backBag),
              (1, offsetBar boxBar)]

renderBoxBar :: Box s -> Colour Double -> Diagram B
renderBoxBar box background =
  let Dimension l w h = boxDim box
      Dimension ox oy oz = boxOffset box
  in depthBar'' (lwL 1 . lc $ blend 0.5  black background ) w oy (blend 0.5 white background)

    
renderBoxBarBg :: Shelf s -> Diagram B
renderBoxBarBg shelf = depthBar yn 0
    where   Dimension xn yn zn = minDim shelf
    
-- | draw text scaled to fit given rectangle
-- scaledText :: Double -> Double-> String -> Diagram b
scaledText x y s =  let
    (x0, y0) = (10,3) 
    (sX, sY) =  (x/x0, y/y0)
    in text s & scale (min sX sY)


