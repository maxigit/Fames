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
  onSelectBase row


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
                                          main
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
                                

    

                            
