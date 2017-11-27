module Model.Boxtake where

import Foundation
import Import.NoFoundation
import Handler.Util


-- * Util
-- * DB
-- | Update the location of a boxtake and reactivate if needed
updateBoxtakeLocation :: DocumentKeyId -> Text -> OperatorId -> Day -> Entity Boxtake -> SqlHandler ()
updateBoxtakeLocation docKey location operatorId date (Entity key Boxtake{..}) = do
     update key [ BoxtakeLocation =. location
                , BoxtakeDate =. date
                , BoxtakeOperator =. operatorId
                , BoxtakeDocumentKey =. docKey
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
deactivateBoxtake :: Day -> Entity Boxtake -> SqlHandler ()
deactivateBoxtake date (Entity key Boxtake{..}) = do
  update key [ BoxtakeActive =. False
             , BoxtakeLocationHistory =. (boxtakeDate
                                         , boxtakeLocation
                                         ) : boxtakeLocationHistory
             , BoxtakeDate =. date
             ]
