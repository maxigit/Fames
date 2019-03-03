-- | Render table
module Handler.Table where

import Import
import Lens.Micro.Extras(view)
import Data.Text(toTitle)
import Handler.Util

displayTable :: [col] -- ^ index of columns to display
             -> (col -> (Html, [Text])) -- ^ column index to header and class
             -> [ (col -> Maybe (Html, [Text])
                  , [Text]
                  )] -- ^ rows, given column index, return a value and classes
             -> Widget
displayTable columns colDisplay rows = do
  [whamlet|
<table.table.table-bordered>
  ^{displayTableRowsAndHeader columns colDisplay rows}
|]
 

displayTableRows :: [col] -- ^ index of columns to display
             -> (col -> (Html, [Text])) -- ^ column index to header and class
             -> [(col -> Maybe (Html, [Text]), [Text])] -- ^ rows, given column index, return a value and classes
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

 
displayTableRowsAndHeader :: [col] -- ^ index of columns to display
             -> (col -> (Html, [Text])) -- ^ column index to header and class
             -> [(col -> Maybe (Html, [Text]), [Text])] -- ^ rows, given column index, return a value and classes
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


entityFieldToColumn  :: (PersistEntity e, PersistField typ) => EntityField e typ -> (Text, Entity e  -> PersistValue )
entityFieldToColumn field = (fieldName, getter) where
  fieldDef = persistFieldDef field
  fieldName = toTitle $ getDBName fieldDef
  getter = toPersistValue . view (fieldLens field)


entityColumnToColDisplay :: e -> (Text, e -> PersistValue) -> Maybe (Html, [Text])
entityColumnToColDisplay entity (name, getter) =
  Just ( toHtml . renderPersistValue$ getter entity, [name] )
