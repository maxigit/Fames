{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module ItemsIndex where

import FFIExample

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

partition f xs = (Prelude.filter f xs, Prelude.filter (Prelude.not . f) xs)
main :: Fay ()
main = do
  main'
  installNav
  return ()
main' :: Fay ()
main' = do
  installCallbacks
  installLabelCallbacks
  addCheckAll
  return ()
installCallbacks = do
    rows <- select "#items-index table > tbody > tr"
    jQueryMap installC rows
    return  ()



installC :: Double -> JQ.Element -> Fay JQuery
installC index el = do
  row <- select el
  onclickBase row
  onSelectBase row
  onCheckRow row


-- | install onclick event to expand/collapse base
onclickBase base = do
  dklasses <- JQ.getAttr "class" base
  case dklasses of
    Undefined -> return base
    Defined cs -> do
      let classes = FT.splitOn (" ") cs
          styles = findInClasses "style-" classes
          hideShow = if "base" `elem` classes then jToggleBase base else jHide

      case styles of
            [style] ->  do
                cell <- findSelector "td.description" base
                onClick (\ev -> do
                            hideShow =<< select ("#items-index table > tbody > tr.variation.style-" `FT.append` style )
                            return False
                        ) cell
                JQ.addClass "clickable" cell
            _ -> return base

-- | install  :Change base based on radio
onSelectBase' base = do
  radio <- findSelector "input[type=radio]" base
  Defined name <- JQ.getAttr "name" radio
  JQ.onChange ( do

    let selector = ("tr.base input[type=radio][name='"`FT.append` name `FT.append` "']")
    oldRadio <- select selector
    oldBase <- JQ.closestSelector "tr.base" oldRadio 
    JQ.removeClass "base" oldBase
    JQ.addClass "base" base
    return ()
    ) radio
  return base
-- | ajax call to handle base change
onSelectBase base = do
  radio <- findSelector "input[type=radio]" base
  table <- select "#items-table"
  form <- select "#items-form"
  JQ.onChange (do
                  -- ajax call
                  updateWithAjax form (\html -> do
                                          JQ.setHtml (FT.pack $ T.unpack html) table
                                          main'
                                      )
               ) radio
  return base

updateWithAjax :: JQuery -- ^ form
               -> (a -> Fay()) -- ^ success handler
               -> Fay ()
updateWithAjax =
  ffi "$.ajax({url:%1[0].action, data:%1.serialize(), dataType:'json', type:'POST',success:%2})"
updateWithAjax' :: JQuery -- ^ form
               -> (a -> Fay()) -- ^ success handler
               -> Fay ()
updateWithAjax' =
  ffi "alert(JSON.stringify({url:%1[0].action, data:%1.serialize(), dataType:'json', type:'POST',success:%2}))"

installNav = do
  navs <- select "a.view-mode[data-url]"
  jQueryMap (\_ el -> do
               nav <- select el
               onClick (\ev -> do
                   url' <- JQ.getAttr "data-url" nav
                   let Defined url = url'
                   ajaxReload url
                   -- update nav tab state manually
                   -- because or ajax doesn't render the navbar
                   JQ.removeClass "active" =<< closestSelector "li" navs
                   JQ.addClass "active" =<< closestSelector "li" nav
                   -- update the url on the form, so that next form submit
                   -- keep the actual tab
                   form <- select "#items-form"
                   JQ.setAttr "action" url form
                   -- setLocation (T.pack $ FT.unpack url)
                   return False
                           ) nav
            ) navs



ajaxReload url = do
  form <- select "#items-form"
  table <- select "#items-table"
  JQ.setHtml ("<p>Loading...</p>") table
  ajaxReloadFFI url form (\html -> do
                            JQ.setHtml (FT.pack $ T.unpack html) table
                            main'
                            )
  return ()

ajaxReloadFFI :: FT.Text -> JQuery -> (a -> Fay ()) ->  Fay ()
ajaxReloadFFI = ffi "$.ajax({url:%1, data:%2.serialize(), dataType:'json', type:'POST',success:%3})"

findInClasses prefix [] = []
findInClasses prefix (c:cs) =
  let cs' = findInClasses prefix cs
  in case stripPrefix prefix c of 
       Just suffix -> suffix : cs'
       Nothing -> cs'
       
stripPrefix pre s = let
  l = FT.length pre
  in if FT.take l s  == pre
     then Just (FT.drop l s)
     else Nothing
                                

    

                            
-- | Install callback so that unchecked row are unchecked
onCheckRow :: JQuery -> Fay JQuery
onCheckRow row = do
  checkbox <- findSelector "input[type=checkbox]" row
  JQ.onChange (do
               checkedStr <- JQ.getProp "checked" checkbox
               checked <- jIsTrue checkedStr
               if checked
                 then JQ.removeClass "unchecked" row
                 else JQ.addClass "unchecked" row
               updateCheckAllStatus
               return ()
               ) checkbox
  return row


addCheckAll = do
  td <- select "#items-index table th.checkall"
  JQ.setHtml "<input type=checkbox id=items-checkall>" td
  checkall <- select "#items-checkall"
  JQ.onChange (do
              boxes <- select "#items-index .stock-master-CheckColumn input"
              checked <- JQ.getProp "checked" checkall
              JQ.setProp "checked" checked boxes
              -- update manually unchecked status
              checked' <- jIsTrue checked
              rows <- select "#items-index table tbody tr"
              if checked'
                 then JQ.removeClass "unchecked" rows
                 else JQ.addClass "unchecked" rows
              return ()
           ) checkall
  updateCheckAllStatus

updateCheckAllStatus = do
  checked <- select "#items-index .stock-master-CheckColumn :checked"
  size <- jsize checked
  checkall <- select "#items-checkall"
  JQ.setProp "checked" (if size /= 0 then "true" else "false") checkall



installLabelCallbacks = do
  labels <- select "[data-label]"
  jQueryMap installL labels
  return ()

-- | Install label callback to select or deselect row with similar label
installL :: Double -> JQ.Element -> Fay JQuery
installL index el = do
  label <- select el
  parentRow <- JQ.closestSelector "tr" label
  checkBox <- findSelector "input[type=checkbox]" parentRow
  valueDef <- JQ.getAttr "data-label" label
  case valueDef of
    Defined value ->  do
        JQ.addClass "clickable" label -- change mouse pointer
      -- find rows matching the same label
        others <- select ("[data-label=" `FT.append` value  `FT.append` "]")
        targets <- JQ.closestSelector "tr" others

        onClick (\ev -> do
                    -- depending on the value of the current row
                    -- we either select or unselect all the targets
                    checkedStr <- JQ.getProp "checked" checkBox
                    checked <- jIsTrue checkedStr
                    checkBoxes <- findSelector "input[type=checkbox]" targets
                    JQ.setProp "checked" (if checked then "" else "checked") checkBoxes
                    if checked -- need unchecking
                      then JQ.addClass "unchecked" targets
                      else JQ.removeClass "unchecked" targets
                    updateCheckAllStatus
                    return False
                  ) label

    _ -> return label


