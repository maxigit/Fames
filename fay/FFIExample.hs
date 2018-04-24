{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example of defining FFI functions.
--
-- The `ffi' method is currently incompatible with 'RebindableSyntax',
-- so these are defined in another module.

module FFIExample where


import Fay.Text (fromString) 
import qualified Fay.Text as FT
import           Prelude
import Data.Text (Text, pack)
import DOM
import FFI
import           JQuery hiding(Element)
import qualified JQuery as JQ
  
onKeyUp :: Element -> Fay () -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

onChange :: Element -> Fay () -> Fay ()
onChange = ffi "%1.onchange=%2"

onSelect :: Element -> Fay () -> Fay ()
onSelect = ffi "%1.onselect=%2"

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

alert :: Text -> Fay ()
alert = ffi "alert(%1)"

alertS :: String -> Fay ()
alertS = ffi "alert(%1)"

alert' :: FT.Text -> Fay ()
alert' = ffi "alert(%1)"

-- | Hide a JQuery
jHide :: JQuery -> Fay ()
jHide = ffi "%1.hide()"

-- | Show a JQuery
jShow :: JQuery -> Fay ()
jShow = ffi "%1.show()"

-- | Collapse or expand the elemetns of base
-- Base is the "header" and elements are the element
-- to hide/show.
-- We the data-hidden attribute to remember
-- the state of the elements. It's quicker than
-- having to check each time or use JQuery toggle
jToggleBase :: JQuery -> JQuery -> Fay ()
jToggleBase base elements = do
  let hiddenAttr = "data-hidden"
  hidden <- JQ.getAttr hiddenAttr base
  let toHide = case hidden of
        Defined "" -> True
        Undefined -> True
        _ -> False

  if toHide
    then
        do
          JQ.setAttr hiddenAttr "true" base
          jHide elements
    else
       do
           JQ.setAttr hiddenAttr "" base
           jShow elements

jsize :: JQuery -> Fay Int
jsize = ffi "%1.size()"


jToList :: JQuery -> Fay [JQ.Element]
jToList = ffi "%1.toArray()"


parseInt' :: FT.Text -> Fay Int
parseInt' = parseInt . pack . FT.unpack

jIsTrue :: FT.Text -> Fay Bool
jIsTrue = ffi "%1 === true"


jUncheck :: JQuery -> Fay JQuery
jUncheck = ffi "%1.prop('checked', false)"
  

-- * Install navigation callback

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
