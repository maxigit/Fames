module Import
( module Import
) where

import Foundation                 as Import
import Import.NoFoundation        as Import
import Handler.Util               as Import
import Language.Haskell.TH.Syntax (Exp (ConE))
import Yesod.Fay                  (FayFile)
import Model.Boxtake              as Import
import Model.DocumentKey              as Import

fayFile :: FayFile
fayFile = fayFile' (ConE 'StaticR)
