{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module ItemsReport where

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
  installNav ajaxReload

ajaxReload url = do
  form <- select "#items-report-form"
  table <- select "#items-report-result"
  JQ.setHtml ("<p>Loading...</p>") table
  ajaxReloadFFI url form (\html -> do
                            JQ.setHtml (FT.pack $ T.unpack html) table
                            )
  return ()
