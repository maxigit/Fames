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
    rows <- select "#items-index table > tbody > tr"
    jQueryMap installC rows
    return  ()



installC :: Double -> JQ.Element -> Fay JQuery
installC index el = do
  row <- select el
  isBase <- hasClass "base" row
  alert (T.pack $ show isBase)
  case isBase of
    True -> onclickBase row
    False -> onclickRow row


onclickBase base = do
  dklasses <- JQ.getAttr "class" base
  alert (T.pack $ show dklasses)
  case dklasses of
    Undefined -> return base
    Defined cs -> do
      let classes = FT.splitOn (" ") cs
      alert (T.pack $ show classes)
      let styles = findInClasses "base-" classes
      alert (T.pack $ show styles)
      case styles of
            [style] -> 
                onClick (\ev -> do
                            alert (T.pack $ show style)
                            return True
                        ) base
            _ -> return base

onclickRow = onclickBase

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
                                
                            
