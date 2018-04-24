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
main :: Fay ()
main = do
  main' -- redone after ajax update
  installNav'
  return ()

main :: Fay ()
main' = return ()

installNav' = do
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
                   form <- select "#planner-view-form"
                   JQ.setAttr "action" url form
                   -- setLocation (T.pack $ FT.unpack url)
                   return False
                           ) nav
            ) navs

ajaxReload url = do
  form <- select "#planner-view-form"
  view <- select "#planner-view-view"
  JQ.setHtml ("<p>Loading...</p>") view
  ajaxReloadFFI url form (\html -> do
                            JQ.setHtml (FT.pack $ T.unpack html) view
                            main'
                            )
  return ()
