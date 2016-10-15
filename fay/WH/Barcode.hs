{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module WH.Barcode where

import DOM
import Fay.Yesod
import Prelude
import SharedTypes
import FFIExample
import Data.Text
import FFI

alert :: Text -> Fay ()
alert = ffi "window.alert(%1)"

main :: Fay ()
main = do
  template <- getElementsByName "ftemplate"
  mapM_ (\t -> onChange t $ do
     alert "ftemplate modified"
	  ) template 
  prefix <- getElementById "hident2"
  onChange prefix $ do
     alert "prefix modified"


