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

main = do
  unsures <- select"#stock-adjustment table tr.unsure"
  jQueryMap installC unsures
  return ()

installC :: Double -> JQ.Element -> Fay JQuery
installC index el = do
  row <- select el
  Defined sku <- JQ.getAttr "data-sku" row
  moveRows <- select ("tr.move.sku-" `FT.append` sku)
  options <- findSelector "option" moveRows
  jQueryMap (updateQohOnChange row) options
  onClick (\e -> do
    jToggleBase row moveRows
    return True) row


updateQohOnChange :: JQuery -> Double -> JQ.Element -> Fay JQuery
updateQohOnChange row index option = do
  jOption <- select option
  spanVal <- findSelector "td.qoh > span.qoh" row
  qohS <- getVal spanVal
  qtyS <- getVal =<< select option
  alert' (qohS `FT.append` "," `FT.append` qtyS)
  qoh <- parseInt (T.pack $ FT.unpack qohS)
  qty <- parseInt (T.pack $ FT.unpack qtyS)
  
  onChange ( do
    qohS <- getVal spanVal
    qoh <- parseInt (T.pack $ FT.unpack qohS)
    setVal (FT.pack . show $ qoh + qty) spanVal
    return ()
    ) jOption
    
  return jOption
    



  
-- | Moves which have been done BEFORE the stocktake
-- needs to be taken into account when calculating the stock adjustment.
-- Moves which have been done AFTER  don't.
-- As the QOH is on the day of the stock take, it should already include
-- all moves from that day and before. Moving those move to after, should
-- result in the QOH to be updated accordingly. This is the same for
-- moves after which are moved before ...
-- updateQOH = 
-- updateQOH _ = undefined
