-- | Miscellaneous functions to help rendering
-- | and/or accessing the database
module Handler.Util where

-- import Foundation
import Import.NoFoundation
import Database.Persist


-- renderEntities :: PersistEntity a => (FieldDef -> Text) -> [Entity a] -> Html
renderEntities getColumn entities = do
  let eDef = entityDef (map entityVal entities)
-- <table class="#{unHaskellName . entityHaskell $ eDef}">
  [shamlet|
<table class="#{unHaskellName $ entityHaskell eDef}">
  <tr>
    $forall field <- entityFields eDef
      <th> #{getColumn field}
  $forall Entity eit entity  <- entities
    <tr>
      $forall (pfield, fieldDef) <- zip (toPersistFields entity) (entityFields eDef)
        <td class="#{getHaskellName fieldDef}" > #{show $ toPersistValue pfield}
|]

getDBName :: FieldDef -> Text
getDBName = unDBName . fieldDB

getHaskellName :: FieldDef -> Text
getHaskellName = unHaskellName . fieldHaskell



