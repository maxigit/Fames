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
  onclickBase row


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
                onClick (\ev -> do
                            hideShow =<< select ("#items-index table > tbody > tr.variation.style-" `FT.append` style )
                            return True
                        ) base
            _ -> return base


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
                                

    

                            
