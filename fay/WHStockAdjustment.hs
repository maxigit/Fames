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

selectModuloInput :: Fay JQuery
selectModuloInput = select "#hident8" -- @TODO change

getModuloValue :: Fay (Maybe Int)
getModuloValue = do
  jMod <- selectModuloInput
  val <- getVal jMod
  case val of
    "" -> return Nothing
    "0" -> return Nothing
    _ -> parseInt' val >>= (\v -> return (Just v))
    


main = do
  unsures <- select"#stock-adjustment table tr.unsure"
  jQueryMap installC unsures
  -- update badge on modulo change, for all row
  rows <- select "#stock-adjustment table tr[data-sku]"
  moduloInput <- selectModuloInput
  onChange ( (jQueryMap (\_ e -> select e >>= updateBadges ) rows) >> return ()
           ) moduloInput
  installActiveCallback

installC :: Double -> JQ.Element -> Fay JQuery
installC index el = do
  row <- select el
  moves <- findMoves row
  inputs <- findSelectMoves row
  jQueryMap (updateQohOnChange row) inputs
  onClick (\e -> do
    jToggleBase row moves
    return True) row

findMoves :: JQuery -> Fay JQuery
findMoves row = do
  Defined sku <- JQ.getAttr "data-sku" row
  select ("tr.move.sku-" `FT.append` sku)
  
findSelectMoves :: JQuery -> Fay JQuery
findSelectMoves row = do
  moveRows <- findMoves row
  findSelector "select" moveRows

  
updateQohOnChange :: JQuery -> Double -> JQ.Element -> Fay JQuery
updateQohOnChange row index input = do
  jSelect <- select input
  -- find options
  onChange ( updateBadges row >> return ()
    ) jSelect
    
  return jSelect
    
-- | Return the sum of moves select as before
-- for a give row
quantityBefore :: JQuery -> JQuery ->  Fay Int
quantityBefore row jSelect = do
  jOptions <- findSelector "option:selected" jSelect
  options' <- jToList jOptions
  options <- (mapM select options')
  qtys <- mapM (\option -> do
                   value <- getVal option
                   parseInt' value
               ) options


  return $ sum qtys
  

updateBadges :: JQuery -> Fay JQuery
updateBadges row = do
  oqties <- getOriginalQuantities row
  jSelect <- findSelectMoves row
  spanQoh <- findSelector "span.qoh" row

  -- computes the new qoh depending on the selected moves 
  qBefore <- quantityBefore row jSelect
  let newQoh = qoh oqties + qBefore
  -- update QOH cell
  setText (FT.pack . show $ newQoh) spanQoh
  -- update bagde sizes
  modulo <- getModuloValue
  let nqties = oqties {qoh = newQoh, qModulo = modulo}
      badges = computeBadges nqties

      updateBadge (accessor, klass) = do
        let q = accessor badges
        badge <- findSelector ("span.badge." `FT.append` klass) row
        case badgeWidth q of
          Nothing -> jHide badge
          Just w -> do
            jShow badge
            setText (FT.pack $ show q ) badge
            setCss "width" (FT.pack (show w) `FT.append` "em") badge >> return ()


  mapM_ updateBadge [(bMissing, "missing"), (bMissingMod, "missing-mod")
                        , (bFound, "found") , (bFoundMod , "found-mod")
                        , (bNew, "new")]
  return row



  

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

-- * Toggle All
installActiveCallback :: Fay JQuery
installActiveCallback = do
  master <- select "#stock-adj-active-all"
  boxes <- getAllActiveBoxes

  onChange (do
               
              checked <- JQ.getProp "checked" master
              JQ.setProp "checked" checked boxes
              setRowActiveStyle master checked boxes
              return ()
           ) master

  jQueryMap (installUpdateActive master) boxes
  return boxes


getAllActiveBoxes :: Fay JQuery
getAllActiveBoxes = do
  select "#stock-adjustment table tr td.active input"

installUpdateActive :: JQuery -> Double -> JQ.Element -> Fay  JQuery
installUpdateActive master _ el = do
  j <- select el
  onChange (do
               checked <- JQ.getProp "checked" j
               setRowActiveStyle master checked j
               updateActiveMaster master
               return ()
           ) j
  return j
  

setRowActiveStyle :: JQuery -> FT.Text -> JQuery -> Fay JQuery 
setRowActiveStyle master checkedStr boxes = do
  checked <- jIsTrue checkedStr
  let opacity = if checked then "1" else "0.5"
  rows <- parent =<< parent boxes
  setCss "opacity" opacity rows

-- If one more of the active boxes are checked, the master should be checked
updateActiveMaster :: JQuery -> Fay ()
updateActiveMaster master = do
  boxes <- getAllActiveBoxes
  -- checkedBoxes <- findSelector ":checked" boxes
  checkedBoxes <- select "#stock-adjustment table tr td.active input:checked"
  size <- jsize checkedBoxes
  if size == 0
    then jUncheck master
    else setProp "checked" "true" master
  return ()



  

