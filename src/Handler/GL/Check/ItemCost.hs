module Handler.GL.Check.ItemCost
( getGLCheckItemCostR
, getGLCheckItemCostAccountViewR
, getGLCheckItemCostItemViewR
)
where

import Import
import Handler.GL.Check.ItemCost.Common
import Formatting as F

getGLCheckItemCostR :: Handler Html
getGLCheckItemCostR = do
  accounts <- getStockAccounts
  summaries <- mapM getAccountSummary accounts
  defaultLayout 
    [whamlet|
      <table *{datatable}>
        <thead>
          <tr>
            <th> Account
            <th data-class-name="text-right"> GL Balance
            <th data-class-name="text-right"> Correct Balance
            <th data-class-name="text-right"> Stock Valuation
            <th data-class-name="text-right"> Difference
        <tbody>
          $forall AccountSummary{..} <- summaries
           <tr>
            <td> 
               <a href=@{GLR $ GLCheckItemCostAccountViewR (fromAccount asAccount)}>
                 #{fromAccount asAccount} - #{asAccountName}
            <td> #{formatDouble asGLAmount}
            <td> #{maybe "" formatDouble' asCorrectAmount}
            <td> #{formatDouble asStockValuation}
            $if equal' 0.01 asGLAmount asStockValuation
              <td>
              
            $else
              <td class="#{classFor 100 asGLAmount asStockValuation}"> #{formatDouble $ asGLAmount - asStockValuation}
    |]


getGLCheckItemCostAccountViewR :: Text -> Handler Html
getGLCheckItemCostAccountViewR account = do
  sku'counts <- loadPendingTransactionCountFor (Account account)
  defaultLayout 
    [whamlet|
     <table *{datatable}>
      <thead>
        <th> Stock Id
        <th> Unchecked moves
      <tbody>
        $forall (sku, count) <- sku'counts
          <tr>
            <td>
              <a href="@{GLR $ GLCheckItemCostItemViewR account sku}">
                #{fromMaybe "<unknow sku>" sku}
            <td> #{tshow count}
    |]


getGLCheckItemCostItemViewR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewR account item = do
  trans0 <- loadMovesAndTransactions (Account account) item
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let trans = computeItemCostTransactions (Account account) trans0
      urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
  defaultLayout $
    infoPanel (fromMaybe account item)  [whamlet|

      <table *{datatable}>
        <thead>
              <th> Date
              <th> No
              <th> Type
              <th data-class-name="text-right"> Amount
              <th data-class-name="text-right"> Quantity
              <th data-class-name="text-right"> Cost
              <th data-class-name="text-right"> Stock Value
              <th data-class-name="text-right"> QOHBefore
              <th data-class-name="text-right"> QOHAfter
              <th data-class-name="text-right"> CostBefore
              <th data-class-name="text-right"> CostAfter
              <th> CostValidation
              <th data-class-name="text-right"> MovedId
              <th data-class-name="text-right"> GlDetail
        <tbody>
          $forall ItemCostTransaction{..} <- trans
            $with _unused <- (itemCostTransactionAccount, itemCostTransactionSku)
            <tr>
              <td> #{tshow itemCostTransactionDate}
              <td> #{transNoWithLink urlFn ""  itemCostTransactionFaTransType itemCostTransactionFaTransNo}
              <td> #{transIconWithLink glUrlFn "" itemCostTransactionFaTransType itemCostTransactionFaTransNo}
              $if equal itemCostTransactionFaAmount itemCostTransactionCorrectAmount
                <td.bg-success.text-success>
                  #{formatDouble' itemCostTransactionFaAmount}
              $else
                <td class="#{classFor 0.5 itemCostTransactionFaAmount itemCostTransactionCorrectAmount}" data-tople="tooltip"
                  title="diff: #{formatDouble' $ itemCostTransactionFaAmount - itemCostTransactionCorrectAmount}">
                  <div> FA: #{formatDouble' itemCostTransactionFaAmount}
                  <div> SB: #{formatDouble' itemCostTransactionCorrectAmount}
              <td> #{formatDouble' itemCostTransactionQuantity}
              $if equal itemCostTransactionCost itemCostTransactionMoveCost
                <td.bg-success.text-success>
                  #{formatDouble' itemCostTransactionCost}
              $else
                <td class="#{classFor 0.01 itemCostTransactionCost itemCostTransactionMoveCost}" data-toggle="tooltip"
                  title="#{formatDouble' $ itemCostTransactionCost - itemCostTransactionMoveCost}"
                  >
                  <div> Move: #{formatDouble' itemCostTransactionMoveCost}
                  <div> SB: #{formatDouble' itemCostTransactionCost}
              $if equal itemCostTransactionStockValue itemCostTransactionFaStockValue
                <td.bg-success.text-success> #{formatDouble' itemCostTransactionStockValue}
              $else
                <td class="#{classFor 0.5 itemCostTransactionFaStockValue itemCostTransactionStockValue}" data-toggle="tooltip"
                title="diff: #{formatDouble' $ itemCostTransactionFaStockValue - itemCostTransactionStockValue}" >
                  <div> FA: #{formatDouble' itemCostTransactionFaStockValue}
                  <div> SB: #{formatDouble' itemCostTransactionStockValue}
              <td> #{formatDouble' itemCostTransactionQohBefore}
              <td> #{formatDouble' itemCostTransactionQohAfter}
              <td> #{formatDouble' itemCostTransactionCostBefore}
              <td> #{formatDouble' itemCostTransactionCostAfter}
              <td> #{tshowM itemCostTransactionItemCostValidation}
              <td> #{tshowM itemCostTransactionMoveId}
              <td> #{tshowM itemCostTransactionGlDetail}
    |]

formatDouble' :: Double -> Text
formatDouble' = F.sformat (commasFixedWith' round 6)

equal = equal' 1e-6
equal' e a b = abs (a - b) < e
classFor e a b = if equal' e a b
                 then "bg-warning text-warning" :: Text
                 else "bg-danger text-danger"
