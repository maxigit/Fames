{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module ItemsReport where

import FFIExample

import DOM hiding(hasClass)
import Data.Text (fromString) 
import FFI
import qualified Data.Text as T
import           Fay.Yesod
import           JQuery as JQ
import           Prelude
import           SharedTypes


main :: Fay ()
main = do
  installNav "#items-report-form" ajaxReload
  return ()

ajaxReload url = do
  form <- select "#items-report-form"
  table <- select "#items-report-result"
  JQ.setHtml ("<p>Loading...</p>") table
  ajaxReloadFFI url form (\html -> do
                            JQ.setHtml html table
                            -- update url in address bar
                            replaceUrlInBar url
                            -- and in form action
                            -- done in installNav JQ.setAttr form "action" url
                            return ()
                            )
  return ()
