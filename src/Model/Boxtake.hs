module Model.Boxtake where

import Foundation
import Import.NoFoundation



-- * Util 
-- * DB 
-- | Update the location of a boxtake and reactivate if needed
-- We don't update the document key to keep the original one.
-- In fact we can't, as there is a unique (reference, document_key).
-- Changing the document key will require to change the reference as well
-- to avoid collision.
updateBoxtakeLocation :: Text -> OperatorId -> Day -> Entity Boxtake -> SqlHandler ()
updateBoxtakeLocation location operatorId date (Entity key Boxtake{..}) = do
     update key [ BoxtakeLocation =. location
                , BoxtakeDate =. date
                , BoxtakeOperator =. operatorId
                , BoxtakeLocationHistory =. (boxtakeDate
                                    , boxtakeLocation
                                    ) : boxtakeLocationHistory
                , BoxtakeActive =. True
                ]

-- | Deactivate the boxtake and the corresponding stocktake. (or not yet)
-- Doesn't update the location, so we can query easily where it was last
-- but update the history. 
-- without that, we will lose either the date when it was deactivated or the
-- initial date when the last scan has been done.
-- The date is not changed neither because the box hasn't actually
-- been scanned and therefore shouldn't have priority when deciding
-- how to reactivate/deactiave boxes using automatic box adjustmente
setActivateBoxtake :: Bool -> Day -> Entity Boxtake -> SqlHandler ()
setActivateBoxtake active date (Entity key Boxtake{..}) = do
  update key [ BoxtakeActive =. active
             , BoxtakeLocationHistory =. (date
                                         , if active then "<Reactivated>" else "<Deactivated>"
                                         ) : boxtakeLocationHistory
             -- , BoxtakeDate =. date
             ]

deactivateBoxtakeByKey :: Day -> Key Boxtake -> SqlHandler ()
deactivateBoxtakeByKey day key = do
  boxtakeM <- get key 
  case boxtakeM of
    Nothing -> setWarning . toHtml $ "Can't deactivate box #" <> tshow key <> ". Box doesn't exist."
    Just boxtake -> setActivateBoxtake False day (Entity key boxtake)
      


reactivateBoxtakeByKey :: Day -> Key Boxtake -> SqlHandler ()
reactivateBoxtakeByKey day key = do
  boxtakeM <- get key 
  case boxtakeM of
    Nothing -> setWarning . toHtml $ "Can't reactivate box #" <> tshow key <> ". Box doesn't exist."
    Just boxtake -> setActivateBoxtake True day (Entity key boxtake)
