-- | Render table
module Handler.Table where

import Import
import Lens.Micro.Extras(view)
import Data.Text(toTitle)

displayTable :: [col] --  ^ index of columns to display
             -> (col -> (Html, [Text])) --  ^ column index to header and class
             -> [ (col -> Maybe (Html, [Text])
                  , [Text]
                  )] --  ^ rows, given column index, return a value and classes
             -> Widget
displayTable columns colDisplay rows = do
  [whamlet|
<table *{datatable}>
  ^{displayTableRowsAndHeader columns colDisplay rows}
|]
 
displayTable200 :: [col] --  ^ index of columns to display
                -> (col -> (Html, [Text])) --  ^ column index to header and class
                -> [ (col -> Maybe (Html, [Text])
                     , [Text]
                     )] --  ^ rows, given column index, return a value and classes
                -> Widget
displayTable200 columns colDisplay rows = do
  [whamlet|
<table data-page-length=200 *{datatable}>
  ^{displayTableRowsAndHeader columns colDisplay rows}
|]

displayTableRows :: [col] --  ^ index of columns to display
             -> (col -> (Html, [Text])) --  ^ column index to header and class
             -> [(col -> Maybe (Html, [Text]), [Text])] --  ^ rows, given column index, return a value and classes
             -> Widget
displayTableRows  columns colDisplay rows = do
  [whamlet|
  $forall (rowF, rowA) <- rows
    <tr class="#{intercalate " " rowA}">
      $forall col <- columns
        $case rowF col
          $of Nothing
            <td.empty class="#{intercalate " " (snd (colDisplay col))}">
          $of Just (cellH, cellA)
            <td class="#{intercalate " " ((snd (colDisplay col)) <> cellA)}"> #{cellH}
  |]

 
displayTableRowsAndHeader :: [col] --  ^ index of columns to display
             -> (col -> (Html, [Text])) --  ^ column index to header and class
             -> [(col -> Maybe (Html, [Text]), [Text])] --  ^ rows, given column index, return a value and classes
             -> Widget
displayTableRowsAndHeader  columns colDisplay rows = do
  [whamlet|
  <thead>
    <tr>
      $forall (colH, colA) <- map colDisplay columns
        <th class="#{intercalate " " colA}">#{colH}
  <tbody>
    ^{displayTableRows columns colDisplay rows}
|]


entityFieldToColumn  :: (PersistEntity e, PersistField typ) => EntityField e typ -> (Text, Entity e  -> Either Html PersistValue )
entityFieldToColumn field = (fieldName, getter) where
  fieldDef = persistFieldDef field
  fieldName = toTitle $ getDBName fieldDef
  getter = Right . toPersistValue . view (fieldLens field)


entityColumnToColDisplay :: e -> (Text, e -> Either Html PersistValue) -> Maybe (Html, [Text])
entityColumnToColDisplay entity (name, getter) =
  Just ( either id (toHtml . renderPersistValueEraseNull) $ getter entity, [name] )

renderPersistValueEraseNull :: PersistValue -> Text
renderPersistValueEraseNull PersistNull = ""
renderPersistValueEraseNull pv = renderPersistValue pv

entityKeyToColumn :: (PersistField typ, PersistEntity e)
  => (Route App -> [(Text, Text)] -> Text)
  -> (Int64 -> Route App)
  -> EntityField e typ
  -> (Text,
      Entity e -> Either Html PersistValue)
entityKeyToColumn renderUrl route field = addRoute <$$> entityFieldToColumn field where
  addRoute v = case v of
    Right (PersistInt64 int64) -> Left $ [hamlet|<a href="@{route int64}">##{tshow int64}|] renderUrl
    _ -> v


