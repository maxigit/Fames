module Handler.GL.Check.ItemCost
( getGLCheckItemCostR
, getGLCheckItemCostAccountViewR
, getGLCheckItemCostItemViewR
, getGLCheckItemCostItemViewSavedR
, postGLCheckItemCostAccountCollectR
, postGLCheckItemCostCollectAllR
, getGLCheckItemCostCheckR
, postGLCheckItemCostPurgeR
, postGLCheckItemCostPurgeAccountR
, postGLCheckItemCostPurgeAccountItemR
, postGLCheckItemCostUpdateGLR
, postGLCheckItemCostUpdateGLAccountR
, postGLCheckItemCostUpdateGLAccountItemR
, getGLCheckItemCostValidationsViewR
, getGLCheckItemCostValidationViewR
, postGLCheckItemCostVoidValidationR
)
where

import Import
import Handler.GL.Check.ItemCost.Common
import Formatting as F
import qualified FA as FA
import GL.Check.ItemCostSettings
import Database.Persist.Sql  (fromSqlKey, toSqlKey)

import Yesod.Form.Bootstrap3

-- * Rendering
getGLCheckItemCostR :: Handler Html
getGLCheckItemCostR = do
  accounts <- getStockAccounts
  (date, form, encType) <- extractDateFromUrl
  summaries <- mapM (getAccountSummary date) accounts
  let glBalance = sum (map asGLAmount summaries)
      stockValue = sum (map asStockValuation summaries)
      qoh = sum (map asQuantity summaries)
      correctValue = sum (mapMaybe asCorrectAmount summaries)
      correctQoh = sum (mapMaybe asCorrectQoh summaries)
  defaultLayout  $ do
    setTitle  . toHtml  $ "Check Item cost - " <> tshow date
    [whamlet|
      <h2> Check Item cost #{tshow date}
      <table data-page-length=10 *{datatable}>
        <thead>
          <tr>
            <th> Account
            <th> Collect Date
            <th data-class-name="text-right"> GL Balance
            <th data-class-name="text-right"> Correct Balance
            <th data-class-name="text-right"> GL Balance - Correct 
            <th data-class-name="text-right"> Stock Valuation - Correct
            <th data-class-name="text-right"> Stock Valuation
            <th data-class-name="text-right"> Stock Valuation - GL
            <th data-class-name="text-right"> QOH (FA)
            <th data-class-name="text-right"> QOH - Correct
        <tbody>
          $forall AccountSummary{..} <- summaries
           <tr>
            <td> 
               <a href=@{GLR $ GLCheckItemCostAccountViewR (fromAccount asAccount)}>
                 #{fromAccount asAccount} - #{asAccountName}
            <td> #{tshowM asCollectDate} 
                <form.form-inline method=POST action="@{GLR $ GLCheckItemCostAccountCollectR (fromAccount asAccount)}">  
                   <span.hidden> ^{form}
                   <button.btn.btn-danger type="sumbit"> Collect
            <td> #{formatDouble asGLAmount}
            $case asCorrectAmount
              $of (Just correct)
                $if equal correct asGLAmount
                  <td.bg-success.text-success>
                      #{formatDouble correct}
                  <td.bg-success.text-success>
                      #{formatAbs asGLAmount correct}
                $else
                  <td class="#{classFor 100 asGLAmount correct}">
                      #{formatDouble correct}
                  <td class="#{classFor 100 asGLAmount correct}">
                      #{formatAbs correct asGLAmount}
                $if equal' 0.01 asGLAmount asStockValuation
                  <td.bg-success.text-sucess>
                    #{formatAbs correct asStockValuation}
                $else
                  <td class="#{classFor 100 asGLAmount asStockValuation}">
                    #{formatAbs correct asStockValuation}
              $of Nothing
                <td>
                <td>
                <td>
            <td> #{formatDouble asStockValuation}
            $if equal' 0.01 asGLAmount asStockValuation
              <td.bg-success.text-sucess>
                #{formatAbs asGLAmount asStockValuation}
            $else
              <td class="#{classFor 100 asGLAmount asStockValuation}">
                #{formatAbs asGLAmount asStockValuation}
            <td> #{formatDouble' asQuantity}
            <td> #{maybe "-" (formatAbs asQuantity) asCorrectQoh}
        <tfoot>
          <tr>
            <th> Total
            <th> #{formatDouble glBalance}
            <th> #{formatDouble correctValue}
            <th> #{formatDouble $ glBalance - correctValue}
            <th> #{formatDouble $ stockValue - correctValue}
            <th> #{formatDouble $ stockValue}
            <th> #{formatDouble $ stockValue - glBalance}
            <th> #{formatDouble' $ qoh}
            <th> #{formatAbs qoh correctQoh}
     <form.form-inline method=GET action="@{GLR $ GLCheckItemCostR}" enctype="#{UrlEncoded}">  
       ^{form}
       <button.btn.btn-primary type="sumbit"> Refresh
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostCollectAllR}" enctype="#{encType}">  
       ^{form}
       <button.btn.btn-warning type="sumbit"> Collect All
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostPurgeR}">  
       <button.btn.btn-danger type="sumbit"> Purge All
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateGLR}">  
       ^{form}
       <button.btn.btn-warning type="sumbit"> Fix Gl
    |]


getGLCheckItemCostAccountViewR :: Text -> Handler Html
getGLCheckItemCostAccountViewR account = do
  (date, form, encType) <- extractDateFromUrl
  sku'count'lasts0 <- loadPendingTransactionCountFor date (Account account)
  let totalCount = sum $ [count | (_,count,_) <- sku'count'lasts] 
      faStockValue = sum $ [ itemCostSummaryFaStockValue last | (_,_,Just last) <- sku'count'lasts] 
      stockValue = sum $ [ itemCostSummaryStockValue last | (_,_,Just last) <- sku'count'lasts] 
      -- filter items with no transaction neither summary (so not used at all)
      sku'count'lasts = filter (\(_, count, lastm) -> count /= 0 || isJust lastm) sku'count'lasts0
  defaultLayout $ do
    setTitle . toHtml $ "Item Cost - View Account " <> account
    [whamlet|
     <table data-page-length=10 *{datatable}>
      <thead>
        <th> Stock Id
        <th> Unchecked moves
        <th data-class-name="text-right"> Gl - Correct
        <th data-class-name="text-right"> Checked GL
        <th data-class-name="text-right"> Checked Correct
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
                  <td.bg-success.text-success> #{formatDouble (abs $ itemCostSummaryStockValue last - itemCostSummaryFaStockValue last)}
                  <td.bg-success.text-success> #{formatDouble (itemCostSummaryStockValue last)}
                $else
                  <td."#{classFor 0.5 (itemCostSummaryFaStockValue last) (itemCostSummaryStockValue last)}" >
                       #{formatAbs (itemCostSummaryStockValue last) (itemCostSummaryFaStockValue last)}
                  <td class="#{classFor 0.5 (itemCostSummaryFaStockValue last) (itemCostSummaryStockValue last)}">
                    #{formatDouble (itemCostSummaryFaStockValue last)}
                <td> #{formatDouble (itemCostSummaryStockValue last)}
              $of Nothing
                <td>
                <td>
                <td>
      <tfoot>
        <th> Total
        <th> #{tshow totalCount}
        <th> #{formatDouble $ faStockValue - stockValue }
        <th> #{formatDouble faStockValue }
        <th> #{formatDouble stockValue}
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostAccountCollectR account}" enctype="#{encType}">  
       ^{form}
       <button.btn.btn-warning type="sumbit"> Collect
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostPurgeAccountR account}">
       <button.btn.btn-danger type="submit"> Purge
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateGLAccountR account}">
       ^{form}
       <button.btn.btn-warning type="submit"> Fix Gl
    |]


getGLCheckItemCostItemViewR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewR account item = do
  lastm <- loadCostSummary (Account account) item
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let endDatem =  item >>= itemSettings settingsm (Account account) >>= closingDate
  trans0 <- loadMovesAndTransactions (entityVal <$> lastm) endDatem (Account account) item
  let transE = computeItemCostTransactions (entityVal <$> lastm) (Account account) trans0
  w <- either (renderDuplicates account) (renderTransactions (fromMaybe account item)) transE
  defaultLayout $ do
    setTitle . toHtml $ "Item Cost - pending " <> account <> maybe " (All)" ("/" <>) item
    w

getGLCheckItemCostItemViewSavedR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewSavedR account item = do
    trans <- runDB $ selectList [ItemCostTransactionAccount ==. account, ItemCostTransactionSku ==. item] []
    w <-  renderTransactions (fromMaybe account item) (map entityVal trans)
    (_, form, encType) <-  extractDateFromUrl
    defaultLayout $ do
      setTitle . toHtml $ "Item Cost Item - done " <> account <> maybe " (All)" ("/" <>) item
      w
      [whamlet|
      <form.form-inline method=POST action="@{GLR $ GLCheckItemCostPurgeAccountItemR account item}">
        <button.btn.btn-danger type="submit"> Purge
      <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateGLAccountItemR account item}" encType=#{encType}>
        ^{form}
        <button.btn.btn-warning type="submit"> Fix Gl
      |]

renderTransactions :: Text -> [ItemCostTransaction] -> Handler Widget 
renderTransactions title trans = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
  return $
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
              <th> Comment
              <th> CostValidation
              <th data-class-name="text-right"> MovedId
              <th data-class-name="text-right"> GlDetail
        <tbody>
          $forall (ItemCostTransaction{..},previousDate) <- zip trans (Nothing : map (Just . itemCostTransactionDate) trans)
            $with _unused <- (itemCostTransactionAccount, itemCostTransactionSku)
            <tr>
              $with inFuture <- Just itemCostTransactionDate < previousDate
                <td :inFuture:.bg-danger :inFuture:.text-danger> #{tshow itemCostTransactionDate}
              <td> #{transNoWithLink urlFn ""  itemCostTransactionFaTransType itemCostTransactionFaTransNo}
              <td> #{transIconWithLink glUrlFn "" itemCostTransactionFaTransType itemCostTransactionFaTransNo}
              $if equal itemCostTransactionFaAmount itemCostTransactionCorrectAmount
                <td.bg-success.text-success>
                  #{formatDouble itemCostTransactionFaAmount}
              $else
                <td class="#{classFor 0.5 itemCostTransactionFaAmount itemCostTransactionCorrectAmount}" data-tople="tooltip"
                  title="diff: #{formatDouble $ itemCostTransactionFaAmount - itemCostTransactionCorrectAmount}">
                  <div> FA: #{formatDouble itemCostTransactionFaAmount}
                  <div> SB: #{formatDouble itemCostTransactionCorrectAmount}
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
              $if equal' 0.01 itemCostTransactionStockValue itemCostTransactionFaStockValue
                <td.bg-success.text-success data-toggle="tooltip"
                 title="#{formatDouble' itemCostTransactionFaStockValue}">
                  #{formatDouble' itemCostTransactionStockValue}
              $else
                <td class="#{classFor 0.5 itemCostTransactionFaStockValue itemCostTransactionStockValue}" data-toggle="tooltip"
                title="diff: #{formatDouble' $ itemCostTransactionFaStockValue - itemCostTransactionStockValue}" >
                  <div> FA: #{formatDouble' itemCostTransactionFaStockValue}
                  <div> SB: #{formatDouble' itemCostTransactionStockValue}
              $if equal itemCostTransactionQohBefore itemCostTransactionQohAfter
                <td>
              $else
                <td> #{formatDouble' itemCostTransactionQohBefore}
              $with isNeg <- itemCostTransactionQohAfter < 0
                <td :isNeg:.bg-danger :isNeg:.text-danger > #{formatDouble' itemCostTransactionQohAfter}
              $if equal itemCostTransactionCostBefore itemCostTransactionCostAfter
                <td> 
              $else
                <td> #{formatDouble' itemCostTransactionCostBefore}
              <td class="#{classForRel 0.25 itemCostTransactionCostBefore itemCostTransactionCostAfter}"> #{formatDouble' itemCostTransactionCostAfter}
              <td> #{itemCostTransactionComment}
              <td> #{maybe "" (tshow . fromSqlKey) itemCostTransactionItemCostValidation}
              <td> #{tshowM itemCostTransactionMoveId}
              <td> #{tshowM itemCostTransactionGlDetail}
    |]

-- | Displays stockmoves and gl trans resulting of a "duplicate"
-- : we can' not untangle their cartesian product. (see fixDuplicates)
renderDuplicates :: Text -> (Text, [Matched]) -> Handler Widget
renderDuplicates account (err,move'gls) = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
  return $
    dangerPanel (account) [whamlet|
      <div.bg-danger.text-danger>
        #{err}
      <table *{datatable}>
        <thead>
          <th> Date
          <th> Quantity
          <th> Cost/Amount
          <th> Sku
          <th> Location
          <th>
          <th>
          <th> Id
        <tbody>
          $forall (m'g, _) <- move'gls
            <tr>
              $case m'g
                $of (This (Entity moveId move))
                  <td> #{tshow $ FA.stockMoveTranDate  move}
                  <td> #{formatDouble' $ FA.stockMoveQty  move}
                  <td> #{formatDouble' $ FA.stockMoveStandardCost  move}
                  <td> #{tshow $ FA.stockMoveStockId  move}
                  <td> #{tshow $ FA.stockMoveLocCode  move}
                  <td> #{transNoWithLink urlFn "" (toEnum $ FA.stockMoveType move) (FA.stockMoveTransNo move)}
                  <td> #{transIconWithLink glUrlFn "" (toEnum $ FA.stockMoveType move) (FA.stockMoveTransNo move)}
                  <td> move: #{tshow $ FA.unStockMoveKey moveId}
                $of (That (Entity detailId gl))
                  <td> #{tshow $ FA.glTranTranDate gl}
                  <td> 
                  <td> #{formatDouble' $ FA.glTranAmount  gl}
                  <td> #{tshowM $ FA.glTranStockId  gl}
                  <td> 
                  <td> #{transNoWithLink urlFn "" (toEnum $ FA.glTranType gl) (FA.glTranTypeNo gl)}
                  <td> #{transIconWithLink glUrlFn "" (toEnum $ FA.glTranType gl) (FA.glTranTypeNo gl)}
                  <td> gl: #{tshow $ FA.unGlTranKey detailId}
                $of (These (Entity moveId move) (Entity detailId gl))
                  <td> #{tshow $ FA.stockMoveTranDate  move}
                  <td> #{formatDouble' $ FA.stockMoveQty  move}
                  <td>
                    <div> #{formatDouble' $ FA.stockMoveStandardCost  move}
                    <div> #{formatDouble' $ FA.glTranAmount  gl}
                  <td> #{tshow $ FA.stockMoveStockId  move}
                  <td> #{tshow $ FA.stockMoveLocCode  move}
                  <td> #{transNoWithLink urlFn "" (toEnum $ FA.stockMoveType move) (FA.stockMoveTransNo move)}
                  <td> #{transIconWithLink glUrlFn "" (toEnum $ FA.stockMoveType move) (FA.stockMoveTransNo move)}
                  <td> 
                    <div>move: #{tshow $ FA.unStockMoveKey moveId} 
                    <div>gl: #{tshow $ FA.unGlTranKey detailId}

    |]
    
getGLCheckItemCostCheckR :: Handler Html
getGLCheckItemCostCheckR = do
  checks <- loadCheckInfo
  defaultLayout $ do
    setTitle "Item Cost Check all"
    [whamlet|
    <table *{datatable}>
      <thead>
        <th> Sku
        <th> Account
        <th> <span>Amount Discrepency
        <th> <span>Cost Discrepency
             <span> FA vs Correct
        <th> Negative QOH
        <th> <span>Cost Variation
             <span> Before vs After
        <th> Empty Stock (FA)
        <th> Empty Stock 
        <th> summary Stock 
      <tbody>
        $forall CheckInfo{..} <- checks
          <tr>
            <td> <a href="@{GLR $ GLCheckItemCostItemViewSavedR (fromAccount icAccount) icSku}">
               #{fromMaybe "" icSku}
            <td> <a href="@{GLR $ GLCheckItemCostAccountViewR (fromAccount icAccount)}">
               #{fromAccount icAccount}
            $if abs icAmountDiscrepency > 1
              <td.bg-danger.text-danger>
                #{formatAbs 0 icAmountDiscrepency}
            $else
              <td>
            $if abs icCostDiscrepency > 0.5
              <td.bg-danger.text-danger>
                #{formatAbs 0 icCostDiscrepency}
            $else
              <td>
            $if icNegativeQOH
              <td.bg-danger.text-danger>
                Negative QOH
            $else
              <td>
            $if abs icCostVariation > 0.5
              <td.bg-warning.text-warning>
                #{formatAbs 0 icCostVariation}
            $else
              <td>
            $if abs icNullFAStockDiscrepency > 0
              <td.bg-danger.text-danger>
                #{formatAbs 0 icNullFAStockDiscrepency}
            $else
              <td>
            $if abs icNullStockDiscrepency > 0
              <td.bg-danger.text-danger>
                #{formatAbs 0 icNullStockDiscrepency}
            $else
              <td>
            <td> 
                $if abs icAmountDiscrepency > 1
                  <span>badamount
                $else
                  <span.hidden>amountok
                $if abs icCostDiscrepency > 0.5
                  <span>badcost
                $else
                  <span.hidden>costok
                $if icNegativeQOH
                  <span>badqoh
                $else
                  <span.hidden>qohok
                $if abs icCostVariation > 0.5
                  <span>badvariation
                $else
                  <span.hidden>variationok
                $if abs icNullFAStockDiscrepency > 0
                  <span>badfa
                $else
                  <span.hidden>faok
                $if abs icNullStockDiscrepency > 0
                  <span>badstock
                $else
                  <span.hidden>stockok
  |]

getGLCheckItemCostValidationsViewR :: Handler Html
getGLCheckItemCostValidationsViewR = do
  validations <- runDB $ selectList [] [Desc ItemCostValidationValidationDate, Desc ItemCostValidationLastTransaction]
  defaultLayout [whamlet|
  <table *{datatable}>
    <thead>
      <th> Id
      <th> Date
      <th> Last 
      <th> Comment
      <th> User
      <th> Voided
    <tbody>
      $forall (Entity key ItemCostValidation{..}) <- validations
        <tr>
          <td> <a href="@{GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)}" > ##{tshow $ fromSqlKey key}
          <td> #{tshow itemCostValidationValidationDate}
          <td> #{tshow itemCostValidationLastTransaction}
          <td> #{tshow itemCostValidationComment}
          <td> #{tshow $ fromSqlKey itemCostValidationUserId}
          <td> #{tshow itemCostValidationVoided}
  |]

getGLCheckItemCostValidationViewR :: Int64 -> Handler Html
getGLCheckItemCostValidationViewR vId = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
  (ItemCostValidation{..}, trans) <- runDB $
    liftA2 (,) (getJust (toSqlKey vId) )
               (selectList [ TransactionMapEventType ==. ItemCostValidationE
                        , TransactionMapEventNo ==. fromIntegral vId
                        ] []
                        )
  defaultLayout [whamlet|
      <table.table>
        <tr>
          <th> Validation date
          <td> #{tshow itemCostValidationValidationDate}
          <th> Last Transaction
          <td> #{tshow itemCostValidationLastTransaction}
          <th> User Id
          <td> #{tshow $ fromSqlKey itemCostValidationUserId }
        <tr>
          <th> comment
          <td colspan=4> #{itemCostValidationComment}
      <table *{datatable}>
        <thead>
            <th> Trans Type
            <th> Trans No
            <th> Voided
        $forall (Entity _tId TransactionMap{..}) <- trans
            $with _unused <- (transactionMapEventNo, transactionMapEventType)
            <tr :transactionMapVoided:.text-muted>
              <td> #{transNoWithLink urlFn ""  transactionMapFaTransType transactionMapFaTransNo}
              <td> #{transIconWithLink glUrlFn "" transactionMapFaTransType transactionMapFaTransNo}
              <td>
                $if transactionMapVoided
                     Voided
          
      <form.form-inline method=POST action="@{GLR $ GLCheckItemCostVoidValidationR vId}">  
             <button.btn.btn-danger type="sumbit"> Void
    |]

-- * Saving
postGLCheckItemCostAccountCollectR :: Text -> Handler Html
postGLCheckItemCostAccountCollectR account = do
  (date, _form, _encType) <- extractDateFromUrl
  sku'counts <- loadPendingTransactionCountFor date (Account account)
  let skus = [sku | (sku,count,_) <- sku'counts, count /= 0]
  transE <- mapM (collectCostTransactions date (Account account)) skus
  case partitionEithers transE of
    ([], _ ) -> getGLCheckItemCostAccountViewR account
    (duplicatess, _ ) -> do
      w <- mconcat <$> mapM (renderDuplicates account) duplicatess
      defaultLayout $ do
        setTitle . toHtml $ "Item Cost - collect error " <> account 
        w

postGLCheckItemCostCollectAllR :: Handler Html
postGLCheckItemCostCollectAllR = do
  accounts <- getStockAccounts
  mapM (postGLCheckItemCostAccountCollectR . fromAccount) accounts
  redirect $ GLR GLCheckItemCostR

-- ** Purge

postGLCheckItemCostPurgeR :: Handler Html
postGLCheckItemCostPurgeR = do
  runDB $ do
    deleteWhere [ItemCostTransactionItemCostValidation ==. Nothing]
    deleteWhere ( [] :: [Filter ItemCostSummary])
  redirect $ GLR GLCheckItemCostR

postGLCheckItemCostPurgeAccountR :: Text -> Handler Html
postGLCheckItemCostPurgeAccountR account = do
  runDB $ do
    deleteWhere [ItemCostTransactionAccount ==. account, ItemCostTransactionItemCostValidation ==. Nothing]
    deleteWhere ( [ItemCostSummaryAccount ==. account ] :: [Filter ItemCostSummary])
  redirect $ GLR GLCheckItemCostR

postGLCheckItemCostPurgeAccountItemR :: Text -> Maybe Text -> Handler Html
postGLCheckItemCostPurgeAccountItemR account sku = do
  runDB $ do
    deleteWhere [ ItemCostTransactionAccount ==. account
                , ItemCostTransactionSku ==. sku
                , ItemCostTransactionItemCostValidation ==. Nothing
                ]
    deleteWhere ( [ItemCostSummaryAccount ==. account, ItemCostSummarySku ==. sku ] :: [Filter ItemCostSummary])
  redirect $ GLR GLCheckItemCostR
-- ** Upate GL
--
postGLCheckItemCostUpdateGLR :: Handler Html
postGLCheckItemCostUpdateGLR = do
  (date, _, _) <- extractDateFromUrl
  summaries <- runDB $ selectList [] []
  faId <- fixGLBalance date summaries
  setSuccess [shamlet|
     Journaly entry ##{tshow faId} created.
             |]
  redirect $ GLR $ GLCheckItemCostValidationsViewR

postGLCheckItemCostUpdateGLAccountR :: Text -> Handler Html
postGLCheckItemCostUpdateGLAccountR account = do
  (date, _, _) <- extractDateFromUrl
  summaries <- runDB $ selectList [ItemCostSummaryAccount ==. account] []
  faId <- fixGLBalance date summaries
  setSuccess [shamlet|
     Journaly entry ##{tshow faId} created.
             |]
  redirect $ GLR $ GLCheckItemCostValidationsViewR

postGLCheckItemCostUpdateGLAccountItemR :: Text -> Maybe Text -> Handler Html
postGLCheckItemCostUpdateGLAccountItemR account skum = do
  (date, _, _) <- extractDateFromUrl
  summary <- loadCostSummary (Account account) skum
  faId <- fixGLBalance date (maybeToList summary)
  setSuccess [shamlet|
     Journaly entry ##{tshow faId} created.
             |]
  redirect $ GLR $ GLCheckItemCostValidationsViewR

-- * Voiding
postGLCheckItemCostVoidValidationR :: Int64 -> Handler Html
postGLCheckItemCostVoidValidationR vId = do
  n <- voidValidation (toSqlKey vId)
  setInfo [shamlet|<h2> #{tshow n} transactions have voided|]
  redirect . GLR $ GLCheckItemCostValidationViewR vId
-- * Util

dateForm datem = renderBootstrap3 BootstrapInlineForm form where
  form = areq dayField "date" datem 
extractDateFromUrl :: Handler (Day, Widget, Enctype) 
extractDateFromUrl  = do
  today <- todayH
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let date = fromMaybe today (defaultDate =<< settingsm)

  let form' = dateForm (Just date)
  ((resp, form), encType) <- runFormPostNoToken form'
  case resp of
    FormSuccess date -> return (date, form, encType)
    _ -> do
      ((resp, form), encType) <- runFormGet form'
      case resp of
        FormSuccess date -> return (date, form, encType)
        _ -> return (date, form, encType)


formatDouble' :: Double -> Text
formatDouble' = F.sformat (commasFixedWith' round 6)

formatAbs :: Double -> Double -> Html
formatAbs  a b = [shamlet|
     <span.bracketed :isNegative:.negative data-toggle="tooltip" 
            title="#{ formatDouble diff }"
     > #{formatDouble (abs diff)}|]
      where diff = a - b
            isNegative = diff < 0
equal = equal' 1e-6
equal' e a b = abs (a - b) < e
classFor e a b = if equal' e a b
                 then "bg-warning text-warning" :: Text
                 else "bg-danger text-danger"
classForRel r a b = if a /= 0 && b/= 0 && (1- min a b / max a b) > r
                 then "bg-warning text-warning" :: Text
                 else ""
