module Handler.Customers.Lookup
( getDPDLookupR
) where

import Import
import Database.Persist.Sql (fromSqlKey)
import qualified Data.Map as Map
import Handler.Customers.ShippingDetails

-- | Displays a datatable allowing
-- to search dpd contact
-- and provide a link to
-- to go back to DPD invoice
-- filled with the chosen contact
getDPDLookupR :: Int64 -> Handler Html
getDPDLookupR invoiceNo = do
  detailEs0 <- runDB $ selectList [ ShippingDetailsCourrier ==. "DPD" ]
                                 [ Asc ShippingDetailsShortName 
                                 , Desc ShippingDetailsLastUsed
                                 , Desc ShippingDetailsId -- (1)
                                 ]
  -- (1) Trick so that the record with the full key appears
  --     last. When creating the Map the last item overwrite
  --     the existing one. That way the record with
  --     the full key should be the one appearing on the list.
  -- remove duplicates, ie details which corresponds to the same key
  let byKey = Map.fromList $ map (fanl $ computeKey . entityVal) detailEs0
      detailEs :: [Entity ShippingDetails]
      detailEs = sortOn (\(Entity _ ShippingDetails{..}) -> ( shippingDetailsShortName
                                                          , Down shippingDetailsLastUsed
                                                          )
                        )
                        ( toList byKey )
  

  let eDef = entityDef (map entityVal detailEs)
      keep e = getDBName e `notElem` ["key", "source", "last_used", "courrier"]
      formTo dId = [whamlet|
        <form.form method=GET action="@{CustomersR $ CustInvoiceCCodesR invoiceNo (Just $ fromSqlKey dId)}">
          <button.btn.btn-success.btn-small> Go
       |] 
  let widget = [whamlet|
    <table *{datatable}>
      <thead>
        <tr>
          <th>
          <th> last_used
          $forall field <- filter keep (getEntityFields eDef)
            <th> #{getDBName field}
          <th> source
          <th> courrier
          <th> key
      <tbody>
        $forall Entity dId detail <- detailEs
          <tr>
            <td> ^{formTo dId} 
            <td> #{tshowM (shippingDetailsLastUsed detail)}
            $forall (pfield, _) <- filter (keep . snd) (zip (toPersistFields detail) (getEntityFields eDef))
              <td> #{renderPersistValue $ toPersistValue pfield}
            <td> #{shippingDetailsSource detail}
            <td> #{shippingDetailsCourrier detail}
            <td> #{shippingDetailsKey detail}
  |]
  defaultLayout widget

