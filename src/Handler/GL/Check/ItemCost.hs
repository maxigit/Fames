module Handler.GL.Check.ItemCost
( getGLCheckItemCostR
, getGLCheckItemCostAccountViewR
, getGLCheckItemCostItemViewR
)
where

import Import
import Handler.GL.Check.ItemCost.Common

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
            <th> GL Balance
            <th> Correct Balance
            <th> Stock Valuation
        <tbody>
          $forall AccountSummary{..} <- summaries
           <tr>
            <td> 
               <a href=@{GLR $ GLCheckItemCostAccountViewR (fromAccount asAccount)}>
                 #{fromAccount asAccount} - #{asAccountName}
            <td> #{formatDouble asGLAmount}
            <td> #{maybe "" formatDouble asCorrectAmount}
            <td> #{formatDouble asStockValuation}
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
                #{fromMaybe "" sku}
            <td> #{tshow count}
    |]


getGLCheckItemCostItemViewR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewR account item = do
  trans0 <- loadMovesAndTransactions (Account account) item
  let trans = computeItemCostTransactions (Account account) trans0
  defaultLayout $
    infoPanel (fromMaybe account item)  [whamlet|

      <table *{datatable}>
        <thead>
              <th> Date
              <th> FaTransNo
              <th> FaTransType
              <th> MovedId
              <th> GlDetail
              <th> FaAmount
              <th> CorrectAmount
              <th> QOHBefore
              <th> Quantity
              <th> QOHAfter
              <th> CostBefore
              <th> CostAfter
              <th> CostValidation
        <tbody>
          $forall ItemCostTransaction{..} <- trans
            $with _unused <- (itemCostTransactionAccount, itemCostTransactionSku)
            <tr>
              <td> #{tshow itemCostTransactionDate}
              <td> #{tshow itemCostTransactionFaTransNo}
              <td> #{tshow itemCostTransactionFaTransType}
              <td> #{tshow itemCostTransactionMoveId}
              <td> #{tshow itemCostTransactionGlDetail}
              <td> #{formatDouble itemCostTransactionFaAmount}
              <td> #{formatDouble itemCostTransactionCorrectAmount}
              <td> #{formatDouble itemCostTransactionQohBefore}
              <td> #{formatDouble itemCostTransactionQuantity}
              <td> #{formatDouble itemCostTransactionQohAfter}
              <td> #{formatDouble itemCostTransactionCostBefore}
              <td> #{formatDouble itemCostTransactionCostAfter}
              <td> #{tshowM itemCostTransactionItemCostValidation}
    |]

