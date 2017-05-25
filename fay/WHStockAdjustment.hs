{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module WHStockAdjustment where

import FFIExample hiding(onChange)

import DOM hiding(hasClass)
import Fay.Text (fromString) 
import qualified Fay.Text as FT
-- import Fay.Text
import FFI
import qualified Data.Text as T
import           Fay.Yesod
import           JQuery as JQ
import           Prelude
import           SharedTypes
import SharedStockAdjustment

main = do
  unsures <- select"#stock-adjustment table tr.unsure"
  jQueryMap installC unsures
  return ()

installC :: Double -> JQ.Element -> Fay JQuery
installC index el = do
  row <- select el
  Defined sku <- JQ.getAttr "data-sku" row
  moveRows <- select ("tr.move.sku-" `FT.append` sku)
  inputs <- findSelector "select" moveRows
  jQueryMap (updateQohOnChange row) inputs
  onClick (\e -> do
    jToggleBase row moveRows
    return True) row

updateQohOnChange :: JQuery -> Double -> JQ.Element -> Fay JQuery
updateQohOnChange row index input = do
  orig <- getOriginalQuantities row
  jSelect <- select input
  spanQoh <- findSelector "span.qoh" row
  -- find options
  let quantityBefore = do
        jOptions <- findSelector "option:selected" jSelect
        options' <- jToList jOptions
        options <- (mapM select options')
        qtys <- mapM (\option -> do
                         value <- getVal option
                         parseInt' value
             ) options

        return $ sum qtys
        
  qBefores <- quantityBefore
  let q0 = qoh orig - qBefores -- Quantity in stock before ANY moves without any 
  
  onChange ( do
               q <- quantityBefore
               setText (FT.pack . show $ q0+q) spanQoh
               return ()
    ) jSelect
    
  return jSelect
    


getOriginalQuantities :: JQuery -> Fay OriginalQuantities
getOriginalQuantities row = do
  let getFrom selector = do
        j <- findSelector selector row
        Defined attr <- JQ.getAttr "data-original" j
        parseInt' attr
        
  qtake' <- getFrom "td.quantity"
  qoh' <- getFrom "td.qoh" 
  qlost' <- getFrom "td.lost" 

  return $ OriginalQuantities qtake' qoh' qlost' Nothing

-- | Moves which have been done BEFORE the stocktake
-- needs to be taken into account when calculating the stock adjustment.
-- Moves which have been done AFTER  don't.
-- As the QOH is on the day of the stock take, it should already include
-- all moves from that day and before. Moving those move to after, should
-- result in the QOH to be updated accordingly. This is the same for
-- moves after which are moved before ...
-- updateQOH = 
-- updateQOH _ = undefined
