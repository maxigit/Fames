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
                #{fromMaybe "<unknow sku>" sku}
            <td> #{tshow count}
    |]


getGLCheckItemCostItemViewR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewR account item = do
  trans0 <- loadMovesAndTransactions (Account account) item
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let trans = computeItemCostTransactions (Account account) trans0
      urlFn = urlForFA faURL
  defaultLayout $
    infoPanel (fromMaybe account item)  [whamlet|

      <table *{datatable}>
        <thead>
              <th> Date
              <th> FaTransNo
              <th> FaTransType
              <th> MovedId
              <th> GlDetail
              <th> FA Amount
              <th> Quantity
              <th> Cost
              <th> Move Cost
              <th> CorrectAmount
              <th> QOHBefore
              <th> QOHAfter
              <th> CostBefore
              <th> CostAfter
              <th> Stock Value
              <th> FA Stock Value
              <th> CostValidation
        <tbody>
          $forall ItemCostTransaction{..} <- trans
            $with _unused <- (itemCostTransactionAccount, itemCostTransactionSku)
            <tr>
              <td> #{tshow itemCostTransactionDate}
              <td> #{transNoWithLink urlFn ""  itemCostTransactionFaTransType itemCostTransactionFaTransNo}
              <td> #{transactionIcon itemCostTransactionFaTransType}
              <td> #{tshowM itemCostTransactionMoveId}
              <td> #{tshowM itemCostTransactionGlDetail}
              <td> #{formatDouble itemCostTransactionFaAmount}
              <td> #{formatDouble itemCostTransactionQuantity}
              <td> #{formatDouble itemCostTransactionCost}
              <td> #{formatDouble itemCostTransactionMoveCost}
              <td> #{formatDouble itemCostTransactionCorrectAmount}
              <td> #{formatDouble itemCostTransactionQohBefore}
              <td> #{formatDouble itemCostTransactionQohAfter}
              <td> #{formatDouble itemCostTransactionCostBefore}
              <td> #{formatDouble itemCostTransactionCostAfter}
              <td> #{formatDouble itemCostTransactionStockValue}
              <td> #{formatDouble itemCostTransactionFaStockValue}
              <td> #{tshowM itemCostTransactionItemCostValidation}
    |]

