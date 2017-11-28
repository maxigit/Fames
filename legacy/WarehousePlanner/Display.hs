{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module WarehousePlanner.Display where

import Prelude
import WarehousePlanner.Base
import Diagrams.Prelude hiding(Box)
import Control.Monad.State
import Diagrams.Backend.Cairo 
import Data.Maybe

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
        diags <-  mapM (renderBox shelf) boxes
        return $ foldl atop mempty diags


depthBar :: Double -> Double -> Diagram B
depthBar w used = let
  shelfBar = rect (w/3) 5 # fc green # lwL 0
  usedBar = rect (used/3) 5 # fc lightsteelblue # lwL 0
  threshold1 = rect (w/3*0.70) 5 # fc red # lwL 0
  threshold2 = rect (w/3*0.85) 5 # fc orange # lwL 0
  in mconcat ( map alignL [ usedBar
                          , threshold1 
                          , threshold2 
                          , shelfBar 
                          ]
             )

-- | render a Box within a shelf. 
-- Boxes are stack vertically until they reach the maximum.
-- to do so, we need to keep track of the postion of the last 
-- box
renderBox :: Shelf s -> Box s -> WH (Diagram B) s
renderBox shelf box = do
    let   Dimension xn yn zn = minDim shelf
          Dimension xx yx zx = maxDim shelf
          Dimension l w h = boxDim box
          Dimension ox oy oz = boxOffset box
          foreground = if (ox+l) > xn || (oy+w) > yn || (oz+h) > zn
                  then firebrick
                  else black
          bar = depthBar (yn) w  

    
    background <-  gets colors `ap` return box


    let   r = rect l h  # lc foreground # fc background # lwL 2 #scale 0.95 # pad 1.05
          t = scaledText l h (showOrientation (orientation box) ++ boxSku box)
          diagram_ = t `atop` r
          diagram = center $ (alignBL bar # translate (r2 (5, 5))) `atop` (alignBL diagram_)
          offset LeftToRight =  (ox+l/2, oz+h/2)
          offset RightToLeft = (xn -ox-l/2, oz+h/2) 

          
    return $ diagram # translate (r2 (offset (flow shelf)))

    
    
    
-- | draw text scaled to fit given rectangle
-- scaledText :: Double -> Double-> String -> Diagram b
scaledText x y s =  let
    (x0, y0) = (10,3) 
    (sX, sY) =  (x/x0, y/y0)
    in text s & scale (min sX sY)


