module Handler.GL.Check.ItemCost
( getGLCheckItemCostR
, getGLCheckItemCostAccountViewR
, getGLCheckItemCostItemViewR
, getGLCheckItemCostItemViewSavedR
, postGLCheckItemCostAccountCollectR
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
      <table data-page-length=200 *{datatable}>
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
            $case asCorrectAmount
              $of (Just correct)
                $if equal correct asGLAmount
                  <td.bg-success.text-success>
                      #{formatDouble correct}
                $else
                  <td class="#{classFor 100 asGLAmount correct}">
                      <div>SB: #{formatDouble correct}
                      <div>Diff:  #{formatDouble $ correct - asGLAmount}
              $of Nothing
                <td>
            <td> #{formatDouble asStockValuation}
            $if equal' 0.01 asGLAmount asStockValuation
              <td>
              
            $else
              <td class="#{classFor 100 asGLAmount asStockValuation}"> #{formatDouble $ asGLAmount - asStockValuation}
    |]


getGLCheckItemCostAccountViewR :: Text -> Handler Html
getGLCheckItemCostAccountViewR account = do
  sku'count'lasts0 <- loadPendingTransactionCountFor (Account account)
  let totalCount = sum $ [count | (_,count,_) <- sku'count'lasts] 
      faStockValue = sum $ [ itemCostSummaryFaStockValue last | (_,_,Just last) <- sku'count'lasts] 
      stockValue = sum $ [ itemCostSummaryStockValue last | (_,_,Just last) <- sku'count'lasts] 
      -- filter items with no transaction neither summary (so not used at all)
      sku'count'lasts = filter (\(_, count, lastm) -> count /= 0 || isJust lastm) sku'count'lasts0
  defaultLayout 
    [whamlet|
     <table *{datatable} data-page-length=200>
      <thead>
        <th> Stock Id
        <th> Unchecked moves
        <th> Checked GL
        <th> Checked Correct
      <tbody>
        $forall (sku, count, lastm) <- sku'count'lasts
          <tr>
            <td>
              <a href="@{GLR $ GLCheckItemCostItemViewSavedR account sku}">
                #{fromMaybe "<unknow sku>" sku}
            <td> 
              <a href="@{GLR $ GLCheckItemCostItemViewR account sku}">
                #{tshow count}
            $case lastm
              $of (Just last)
                $if equal (itemCostSummaryFaStockValue last) (itemCostSummaryStockValue last)
                  <td.bg-success.text-success> #{formatDouble (itemCostSummaryStockValue last)}
                $else
                  <td class="#{classFor 0.5 (itemCostSummaryFaStockValue last) (itemCostSummaryStockValue last)}" data-toggle="tooltip"
                  title="diff: #{formatDouble $ (itemCostSummaryFaStockValue last) - (itemCostSummaryStockValue last)}" >
                    #{formatDouble (itemCostSummaryFaStockValue last)}
                <td> #{formatDouble (itemCostSummaryStockValue last)}
              $of Nothing
                <td>
                <td>
      <tfoot>
        <th> Total
        <th> #{tshow totalCount}
        <th> #{formatDouble faStockValue }
        <th> #{formatDouble stockValue}
     <form method=POST action="@{GLR $ GLCheckItemCostAccountCollectR account}">  
       <button.btn.btn-danger type="sumbit"> Collect
    |]


getGLCheckItemCostItemViewR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewR account item = do
  lastm <- loadCostSummary (Account account) item
  trans0 <- loadMovesAndTransactions (entityVal <$> lastm) (Account account) item
  let trans = computeItemCostTransactions (entityVal <$> lastm) (Account account) trans0
  renderTransactions (fromMaybe account item) trans

getGLCheckItemCostItemViewSavedR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewSavedR account item = do
    trans <- runDB $ selectList [ItemCostTransactionAccount ==. account, ItemCostTransactionSku ==. item] []
    renderTransactions (fromMaybe account item) (map entityVal trans)

renderTransactions :: Text -> [ItemCostTransaction] -> Handler Html 
renderTransactions title trans = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
  defaultLayout $
    infoPanel title  [whamlet|

      <table data-page-length=200 *{datatable}>
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

-- * Saving
postGLCheckItemCostAccountCollectR :: Text -> Handler Html
postGLCheckItemCostAccountCollectR account = do
  sku'counts <- loadPendingTransactionCountFor (Account account)
  let skus = [sku | (sku,count,_) <- sku'counts, count /= 0]
  mapM_ (collectCostTransactions (Account account)) skus
  getGLCheckItemCostAccountViewR account



formatDouble' :: Double -> Text
formatDouble' = F.sformat (commasFixedWith' round 6)

equal = equal' 1e-6
equal' e a b = abs (a - b) < e
classFor e a b = if equal' e a b
                 then "bg-warning text-warning" :: Text
                 else "bg-danger text-danger"
