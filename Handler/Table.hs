-- | Render table
module Handler.Table where

import Import

displayTable :: [col] -> (col -> (Html, Text)) -> [(col -> Maybe (Html, Text), Text)] -> Widget
displayTable columns colDisplay rows = do
  [whamlet|
<table.table.table-striped.table-bordered>
  <thead>
    <tr>
      $forall (colH, colA) <- map colDisplay columns
        <th class="#colA}">#{colH}
  <tbody>
    $forall (rowF, rowA) <- rows
      <tr class="#{rowA}">
        $forall col <- columns
          $case rowF col
            $of Nothing
              <td.empty class="#{fst (colDisplay col)}">
            $of Just (cellH, cellA)
              <td class="#{fst (colDisplay col)} #{cellA}"> #{cellH}
|]
 
 

