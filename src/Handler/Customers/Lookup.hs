module Handler.Customers.Lookup
( getDPDLookupR
) where

import Import
import Database.Persist.Sql (fromSqlKey)

-- | Displays a datatable allowing
-- to search dpd contact
-- and provide a link to
-- to go back to DPD invoice
-- filled with the chosen contact
getDPDLookupR :: Int64 -> Handler Html
getDPDLookupR invoiceNo = do
  detailEs <- runDB $ selectList [ ShippingDetailsCourrier ==. "DPD" ]
                                 [ Asc ShippingDetailsShortName ]

  let eDef = entityDef (map entityVal detailEs)
      formTo dId = [whamlet|
        <form.form method=GET action="@{CustomersR $ CustInvoiceCCodesR invoiceNo (Just $ fromSqlKey dId)}">
          <button.btn.btn-success.btn-small> Go
       |] 
  let widget = [whamlet|
    <table *{datatable}>
      <thead>
        <tr>
          <th>
          $forall field <- entityFields eDef
            <th> #{getDBName field}
      <tbody>
        $forall Entity dId detail <- detailEs
          <tr>
            <td> ^{formTo dId} 
            $forall (pfield, _) <- zip (toPersistFields detail) (entityFields eDef)
              <td> #{renderPersistValue $ toPersistValue pfield}
  |]
  defaultLayout widget

