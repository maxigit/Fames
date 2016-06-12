-- | Miscellaneous functions to help rendering
-- | and/or accessing the database
module Handler.Util
( entitiesToTable
, getDBName
, getHaskellName
) where

-- import Foundation
import Import.NoFoundation
import Database.Persist


entitiesToTable :: PersistEntity a => (FieldDef -> Text) -> [Entity a] -> Html
entitiesToTable getColumn entities = do
  let eDef = entityDef (map entityVal entities)
-- <table class="#{unHaskellName . entityHaskell $ eDef}">
  [shamlet|
<table.table.table-bordered class="#{unHaskellName $ entityHaskell eDef}">
  <tr>
    <th> Id
    $forall field <- entityFields eDef
      <th> #{getColumn field}
  $forall Entity eit entity  <- entities
    <tr>
      <td.id> #{renderPersistValue $ toPersistValue eit}
      $forall (pfield, fieldDef) <- zip (toPersistFields entity) (entityFields eDef)
        <td class="#{getHaskellName fieldDef}" > #{renderPersistValue $ toPersistValue pfield}
|]

getDBName :: FieldDef -> Text
getDBName = unDBName . fieldDB

getHaskellName :: FieldDef -> Text
getHaskellName = unHaskellName . fieldHaskell


renderPersistValue :: PersistValue -> Text
renderPersistValue (PersistList _) = "<>"
renderPersistValue pvalue = case (fromPersistValueText pvalue) of
  Left _ -> tshow pvalue
  Right text -> text




