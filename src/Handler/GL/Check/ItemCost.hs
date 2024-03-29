{-# LANGUAGE ImplicitParams #-}
module Handler.GL.Check.ItemCost
( getGLCheckItemCostR
, getGLCheckItemCostAccountViewR
, getGLCheckItemCostItemViewR
, getGLCheckItemCostItemViewSavedR
, postGLCheckItemCostAccountCollectR
, postGLCheckItemCostItemCollectR
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
, postGLCheckItemCostUpdateCostR
, postGLCheckItemCostUpdateCostAccountR
, postGLCheckItemCostUpdateCostAccountItemR
, getGLCheckItemCostCheckPastR
, postGLCheckItemCostCheckPastRefreshR
, postGLCheckItemCostCheckPastPurgeR
)
where

import Import
import Handler.GL.Check.ItemCost.Common
import Formatting as F
import qualified FA as FA
import GL.Check.ItemCostSettings
import Database.Persist.Sql  (fromSqlKey, toSqlKey, Single(..), rawSql)
import Control.Monad (zipWithM_, zipWithM)
import Data.List(scanl)

import Yesod.Form.Bootstrap3

-- * Rendering 
getGLCheckItemCostR :: Handler Html
getGLCheckItemCostR = do
  accounts <- getStockAccounts
  (date, form, encType) <- extractDateFromUrl
  today <- todayH
  summaries0 <- mapM (getAccountSummary date) accounts
  let glBalance = sum (map asGLAmount summaries)
      stockValue = sum (map asStockValuation summaries)
      qoh = sum (map asQuantity summaries)
      correctValue = sum (mapMaybe asCorrectAmount summaries)
      correctQoh = sum (mapMaybe asCorrectQoh summaries)
    -- move account collected in the future first
      summaries = sortOn (\s -> (Down (asCollectDate s > Just date))) summaries0
  defaultLayout  $ do
    setTitle  . toHtml  $ "Check Item cost - " <> tshow date
    [whamlet|
      <h2> Check Item cost #{tshow date}
      <table data-page-length=10 *{datatable}>
        <thead>
          <tr>
            <th> Account
            <th> Collect Date
            <th data-class-name="text-right"
                data-toggle="tooltip"
                title="balance of the GL account on given date as it is on Front Accounting. Doesn't take item and checked status into consideration."
                > GL Balance
            <th data-class-name="text-right"
                data-toggle="toolitp"
                title="balasce of GL trans which seems to not be related to (configured) items. Should be  null ideally."> Out of Scope
            <th data-class-name="text-right"
                data-toggle="tooltip"
                title="How much the balance should be according to the checked moves. Doesn't take into account unchecked moves."> Correct Balance
            <th data-class-name="text-right"> GL Balance - Correct 
            <th data-class-name="text-right"> Stock Valuation - Correct
            <th data-class-name="text-right"> Stock Valuation
            <th data-class-name="text-right"> Stock Valuation - GL
            <th data-class-name="text-right"> QOH (FA)
            <th data-class-name="text-right"> QOH - Correct
        <tbody>
          $forall AccountSummary{..} <- summaries
            $with danger <- asCollectDate > Just date
             <tr :danger:.bg-danger>
              <td > 
                 <a href=@{GLR $ GLCheckItemCostAccountViewR (fromAccount asAccount)}>
                   #{fromAccount asAccount} - #{asAccountName}
              <td :danger:.text-danger> #{tshowM asCollectDate} 
                  <form.form-inline method=POST action="@{GLR $ GLCheckItemCostAccountCollectR (fromAccount asAccount)}">  
                     <span.hidden> ^{form}
                     <button.btn.btn-danger type="sumbit"> Collect
              <td> #{formatDouble asGLAmount}
              $if equal asGLAmountOutOfScope  0
                <td>
              $else
                <td.bg-danger.text-danger> #{formatDouble asGLAmountOutOfScope}
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
                  $if equal' 0.09 correct asStockValuation
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
              $if equal' 0.09 asGLAmount asStockValuation
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
            <th>
            <th> #{formatDouble glBalance}
            <th> 
            <th> #{formatDouble correctValue}
            <th> #{formatAbs glBalance correctValue}
            <th> #{formatAbs stockValue correctValue}
            <th> #{formatDouble $ stockValue}
            <th> #{formatAbs stockValue glBalance}
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
     $if date == today
       <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateCostR}">  
         <button.btn.btn-warning type="sumbit"> Update Cost
    |]


getGLCheckItemCostAccountViewR :: Text -> Handler Html
getGLCheckItemCostAccountViewR account = do
  (date, form, encType) <- extractDateFromUrl
  today <- todayH
  sku'count'lasts0 <- loadPendingTransactionCountFor date (Account account)

  itemInfos <- if date == today
               then itemTodayInfo (Account account)
               else return mempty
  let totalCount = sum $ [count | (_,count,_) <- sku'count'lasts] 
      faStockValue = sum $ [ itemCostSummaryFaStockValue last | (_,_,Just last) <- sku'count'lasts] 
      stockValue = sum $ [ round2 (cost*qoh) | (cost, qoh) <-  toList itemInfos ]
      stockValueRounded = sum $ [ itemCostSummaryStockValueRounded last | (_,_,Just last) <- sku'count'lasts] 
      -- filter items with no transaction neither summary (so not used at all)
      sku'count'lasts = filter (\(_, count, lastm) -> count /= 0 || isJust lastm) 
                      . filter (\(_,count, lastm) -> fmap (isSummaryValid count) lastm /= Just True)
                     $ sku'count'lasts0
      
      -- don't display summary if they have been collected in the future
      -- or are valid without new transaction
      isSummaryValid count s@ItemCostSummary{..} =
        (count == 0 
        && itemCostSummaryValidated
        && equal (itemCostSummaryStockValueRounded s) itemCostSummaryFaStockValue
        && equal itemCostSummaryStockValue
                 (maybe 0 (\(c,q) -> c*q) (itemCostSummarySku >>= flip lookup itemInfos))
        )
        || itemCostSummaryDate > date
        -- && equal' 1e-4 itemCostSummaryStockValue itemCostSummaryFaStockValue
  defaultLayout $ do
    setTitle . toHtml $ "Item Cost - View Account " <> account <> " - " <> tshow date
    [whamlet|
     <table data-page-length=10 *{datatable}>
      <thead>
        <th> Stock Id
        <th> Unchecked moves
        <th> Collect Date
        <th> Validated
        <th data-class-name="text-right"
            data-toggle="tooltip"
            title="Checked GL - Checked Correct "> Gl - Correct
        <th data-class-name="text-right"
            data-toggle="tooltip"
            title="Balance of GL transaction which have been checked so far.  Does'nt take into account unchecked moves." > Checked GL
        <th data-class-name="text-right"
            data-toggle="tooltip"
            title="Expected value (GL balance and stock value) so far.  Doesn't take into account unchecked moves."> Checked Correct
        <th data-class-name="text-right"
            data-toggle="tooltip"
            title="Correct cost ignorning unchecked moves."> Correct Cost
        <th data-class-name="text-right"> Current Cost
        <th data-class-name="text-right"
            data-toggle="tooltip"
            title="Difference between checked stock valuation and current stock valuation."> Cur stock - Checked
        <th data-class-name="text-right"> QOH
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
                <td> #{tshow $ itemCostSummaryDate last}
                <td>#{tshow $ itemCostSummaryValidated last}
                $if equal (itemCostSummaryFaStockValue last) (itemCostSummaryStockValueRounded last)
                  <td.bg-success.text-success> #{formatDouble (abs $ itemCostSummaryStockValueRounded last - itemCostSummaryFaStockValue last)}
                  <td.bg-success.text-success> #{formatDouble (itemCostSummaryStockValueRounded last)}
                $else
                  <td."#{classFor 0.5 (itemCostSummaryFaStockValue last) (itemCostSummaryStockValueRounded last)}" >
                       #{formatAbs (itemCostSummaryStockValueRounded last) (itemCostSummaryFaStockValue last)}
                  <td class="#{classFor 0.5 (itemCostSummaryFaStockValue last) (itemCostSummaryStockValueRounded last)}">
                    #{formatDouble (itemCostSummaryFaStockValue last)}
                <td> #{formatDouble (itemCostSummaryStockValueRounded last)}
              $of Nothing
                <td>
                <td>
                <td>
                <td>
                <td>
            $case (sku >>= flip lookup itemInfos, lastm)
              $of (Just (cost, qoh), Just last) 
                $with stock <- cost * qoh
                  <td> #{formatDouble' (itemCostSummaryCostAfter last)}
                  $with ok <- equal cost (itemCostSummaryCostAfter last)
                    $if ok
                      <td.text-success.bg-success> #{formatDouble' cost}
                    $else
                      <td."#{classFor 0.01 cost (itemCostSummaryCostAfter last)}"> #{formatDouble' cost}
                  $with ok <- equal (itemCostSummaryStockValue last) stock
                    $if ok
                      <td.text-success.bg-success> #{formatAbs stock (itemCostSummaryStockValue last)}
                    $else
                      <td."#{classFor 0.01 stock (itemCostSummaryStockValue last)}"> #{formatAbs stock (itemCostSummaryStockValue last)}
                    <td> #{formatDouble qoh}
              $of (Just (cost, qoh), Nothing ) 
                <td>
                <td> #{formatDouble' cost}
                <td> 
                <td> #{formatDouble' qoh}
              $of _
                <td>
                <td>
                <td>
                <td>
      <tfoot>
        <th> Total
        <th> #{tshow totalCount}
        <th>
        <th>
        <th> #{formatAbs faStockValue stockValueRounded }
        <th> #{formatDouble faStockValue }
        <th> #{formatDouble stockValueRounded}
        <th>
        <th>
        <th> #{formatDouble $ stockValue }
     <form.form-inline method=GET action="@{GLR $ GLCheckItemCostAccountViewR account}" enctype="#{encType}">  
       ^{form}
       <button.btn.btn-primary type="sumbit"> Refresh
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostAccountCollectR account}" enctype="#{encType}">  
       ^{form}
       <button.btn.btn-warning type="sumbit"> Collect
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostPurgeAccountR account}">
       <button.btn.btn-danger type="submit"> Purge
     <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateGLAccountR account}">
       ^{form}
       <button.btn.btn-warning type="submit"> Fix Gl
     $if date == today
       <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateCostAccountR account}">
         <button.btn.btn-warning type="submit"> Update cost
    |]


getGLCheckItemCostItemViewR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewR account item = do
  let onOldAccount oldAccount = error $ unpack  $ "Can't load old account summary for " <> fromAccount oldAccount <> " " <>  fromMaybe "" item
  lastm <- either id entityVal <$$> loadInitialSummary (Account account) onOldAccount item
  (_date, form, encType) <-  extractDateFromUrl
  settingsm <- appCheckItemCostSetting . appSettings <$> getYesod
  let endDatem =  item >>= itemSettings settingsm (Account account) >>= closingDate
      behaviors_ = fromMaybe mempty (settingsm >>= behaviors)
  let ?collectMode = Collectables
  trans0 <- loadMovesAndTransactions lastm endDatem (Account account) item
  let transE = computeItemCostTransactions behaviors_ Nothing lastm (Account account) trans0
  w <- either (renderDuplicates account) (renderTransactions (fromMaybe account item) Nothing) transE
  defaultLayout $ do
    setTitle . toHtml $ "Item Cost - pending " <> account <> maybe " (All)" ("/" <>) item
    w
    [whamlet|
    <form.form-inline method=POST action="@{GLR $GLCheckItemCostItemCollectR account item}" encType=#{encType}>
       ^{form}
      <button.btn.btn-warning type="submit"> Collect

    |]

getGLCheckItemCostItemViewSavedR :: Text -> Maybe Text -> Handler Html
getGLCheckItemCostItemViewSavedR account item = do
    trans <- runDB $ selectList [ItemCostTransactionAccount ==. account, ItemCostTransactionSku ==. item] []

    info <- case item of
        Nothing -> return Nothing
        Just sku -> do 
          info <- itemTodayInfo (Account account)
          return $ lookup sku info
    w <-  renderTransactions (fromMaybe account item) (fmap fst info) (map entityVal trans)
    today <- todayH
    (date, form, encType) <-  extractDateFromUrl
    defaultLayout $ do
      setTitle . toHtml $ "Item Cost Item - done " <> account <> maybe " (All)" ("/" <>) item
      w
      [whamlet|
      <form.form-inline method=POST action="@{GLR $ GLCheckItemCostPurgeAccountItemR account item}">
        <button.btn.btn-danger type="submit"> Purge
      <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateGLAccountItemR account item}" encType=#{encType}>
        ^{form}
        <button.btn.btn-warning type="submit"> Fix Gl
      $if date == today
        <form.form-inline method=POST action="@{GLR $ GLCheckItemCostUpdateCostAccountItemR account item}" encType=#{encType}>
          <button.btn.btn-warning type="submit"> Update Cost
      |]

renderTransactions :: Text -> Maybe Double ->  [ItemCostTransaction] -> Handler Widget 

renderTransactions title costm trans = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
      maxMoveIds = drop 1 $ scanl max Nothing $ map itemCostTransactionMoveId trans
      maxGlIds = drop 1 $ scanl max Nothing $ map itemCostTransactionGlDetail trans
      lesserM am bm = case (am, bm) of
                       (Nothing, _) -> False
                       (_, Nothing) -> True
                       (a, b) -> a < b
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
          $forall (t@ItemCostTransaction{..},previousDate,lastMoveId, lastGlId) <- zip4 trans (Nothing : map (Just . itemCostTransactionDate) trans) maxMoveIds maxGlIds
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
                <td class="#{classFor 0.5 itemCostTransactionFaAmount itemCostTransactionCorrectAmount}" data-toggle="tooltip"
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
              $if equal (itemCostTransactionStockValueRounded t) itemCostTransactionFaStockValue
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
              $with inFuture <- lesserM itemCostTransactionMoveId lastMoveId && not (Just itemCostTransactionDate == previousDate)
                <td :inFuture:.bg-danger :inFuture:.text-danger data-toggle="tooltip"
                 title="Move id should be greater than previous move ##{tshowM lastMoveId}."
                > #{tshowM itemCostTransactionMoveId}
              $with inFuture <- lesserM itemCostTransactionGlDetail lastGlId 
                <td :inFuture:.bg-danger :inFuture:.text-danger data-toggle="tooltip"
                 title="GL detail id should be greater than previous detail ##{tshowM lastGlId}."
                > #{tshowM itemCostTransactionGlDetail}
        <tfoot>
          <th>
          <th>
          <th>
          <th>
          <th>
          $case (lastMay trans, costm)
            $of (Nothing, Just cost)
              <th>  #{formatDouble' cost}
            $of (Just last, Just cost)
              $if equal cost (itemCostTransactionCostAfter last)
                <th.bg-success.text-success>
                  #{formatDouble' cost}
              $else
                <th class="#{classFor 0.01 cost (itemCostTransactionCostAfter last)}" data-toggle="tooltip"
                  title="#{formatDouble' $ cost - (itemCostTransactionCostAfter last)}"
                  > #{formatDouble' cost }
            $of _
              <th>
          $case (lastMay trans)
            $of (Just last)
              $with (stock , fa) <- (itemCostTransactionStockValueRounded last, itemCostTransactionFaStockValue last)
                $if equal stock fa
                  <th.bg-success.text-success> [0.00]
                $else
                  <th."#{classFor 0.5 stock fa}">
                    #{formatAbs stock fa}
            $of Nothing
              <th>
          <th>
          <th>
          <th>
          <th> #{maybe "" (formatDouble' . itemCostTransactionCostAfter) (lastMay trans)}
          $case (lastMay trans, costm)
            $of (Just last, Just cost)
              <th> #{formatAbs (cost * itemCostTransactionQohAfter last) (itemCostTransactionStockValue last)}
            $of _ 
              <th>
          <th>
          <th>
          <th>

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
                  <td> #{FA.stockMoveStockId  move}
                  <td> #{FA.stockMoveLocCode  move}
                  <td> #{transNoWithLink urlFn "" (toEnum $ FA.stockMoveType move) (FA.stockMoveTransNo move)}
                  <td> #{transIconWithLink glUrlFn "" (toEnum $ FA.stockMoveType move) (FA.stockMoveTransNo move)}
                  <td> move: #{tshow $ FA.unStockMoveKey moveId}
                $of (That (Entity detailId gl))
                  <td> #{tshow $ FA.glTranTranDate gl}
                  <td> 
                  <td> #{formatDouble' $ FA.glTranAmount  gl}
                  <td> #{fromMaybe "" $ FA.glTranStockId  gl}
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
                  <td> #{FA.stockMoveStockId  move}
                  <td> #{FA.stockMoveLocCode  move}
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
        <th data-toggle="tooltip"
            title="Maximum difference between the FA amount and the correct amount across all transactions."
            > <span>Amount Discrepency
        <th data-toggle="tooltip"
            title="Maximium difference between the stock move cost and the correct cost across all transactions."
              > <span>Cost Discrepency
                <span> FA vs Correct
        <th data-toggle="tootlip"
            title="Wether the item encountered a negative quantity on hand at some point."
            > Negative QOH
        <th data-toggle="tooltip"
            title="Maximum variation of the correct cost between 2 consecutive transaction."
            > <span>Cost Variation
               <span> Before vs After
        <th data-toggle="tooltip"
            title="Wether there is no stock left but a residual value in GL stock in FA."
            > Empty Stock (FA)
        <th data-toggle="tooltip"
            title="Wether there is no stock left but a residual value in correct stock value."
            >
            Empty Stock 
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
  let total = sum [ itemCostValidationTotal | (Entity _ ItemCostValidation{..}) <- validations, itemCostValidationVoided == False ]
  defaultLayout [whamlet|
  <table *{datatable}>
    <thead>
      <th> Id
      <th> Date
      <th> Last 
      <th> Total
      <th> Comment
      <th> User
      <th> Voided
    <tbody>
      $forall (Entity key ItemCostValidation{..}) <- validations
        <tr>
          <td> <a href="@{GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)}" > ##{tshow $ fromSqlKey key}
          <td> #{tshow itemCostValidationValidationDate}
          <td> #{tshow itemCostValidationLastTransaction}
          <td> #{formatAbs 0 itemCostValidationTotal}
          <td> #{itemCostValidationComment}
          <td> #{tshow $ fromSqlKey itemCostValidationUserId}
          <td> #{tshow itemCostValidationVoided}
    <tfoot>
          <td>
          <td>
          <td>
          <td> #{formatAbs 0 total}
          <td>
          <td>
          <td>
  |]

getGLCheckItemCostValidationViewR :: Int64 -> Handler Html
getGLCheckItemCostValidationViewR vId = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let urlFn = urlForFA faURL
      glUrlFn = glViewUrlForFA faURL
      sql = "SELECT ??, memo_"
           <> " FROM fames_transaction_map "
           <> " LEFT JOIN 0_comments ON (fa_trans_type = 0_comments.type AND 0_comments.id = fa_trans_no) "
           <> " WHERE event_type = ? AND event_no = ? "
      unMaybeText = fromMaybe ( "" :: Text )
  (ItemCostValidation{..}, trans) <- runDB $
    liftA2 (,) (getJust (toSqlKey vId) )
               (rawSql sql [toPersistValue (fromEnum ItemCostValidationE) , toPersistValue vId])
  glTable <- renderValidationGL vId
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
          <th> Total
          <th> #{formatDouble itemCostValidationTotal }
          <th> comment
          <td colspan=2> #{itemCostValidationComment}
      <table *{datatable}>
        <thead>
            <th> Trans Type
            <th> Trans No
            <th> Memo
            <th> Voided
        $forall (Entity _tId TransactionMap{..}, Single memom) <- trans
            $with _unused <- (transactionMapEventNo, transactionMapEventType)
            <tr :transactionMapVoided:.text-muted>
              <td> #{transNoWithLink urlFn ""  transactionMapFaTransType transactionMapFaTransNo}
              <td> #{transIconWithLink glUrlFn "" transactionMapFaTransType transactionMapFaTransNo}
              <td> #{unMaybeText memom}
              <td>
                $if transactionMapVoided
                     Voided
          
      ^{glTable}
      <form.form-inline method=POST action="@{GLR $ GLCheckItemCostVoidValidationR vId}">  
             <button.btn.btn-danger type="sumbit"> Void
    |]

renderValidationGL :: Int64 -> Handler Widget
renderValidationGL vId = do
  let sql = "SELECT account, account_name, sum(GREATEST(amount,0)), SUM(GREATEST(-amount,0)) "
           <> " FROM 0_gl_trans "
           <> " JOIN 0_chart_master ON (account = account_code) "
           <> " JOIN fames_transaction_map ON( fa_trans_type = 0_gl_trans.type AND fa_trans_no =  type_no) "
           <> " WHERE event_type = ? AND event_no = ? "
           <> " GROUP BY account "
  rows <- runDB $ rawSql sql [toPersistValue (fromEnum ItemCostValidationE) , toPersistValue vId]
  let _types = rows:: [(Single Text, Single Text, Single Double, Single Double)]
  return [whamlet|
   <table *{datatable}>
    <thead>
      <th> Account
      <th> Debit
      <th> Credit
      <th> Balance
    <tbody>
      $forall (Single account, Single accountName, Single debit, Single credit) <- rows
        <tr>
          <td> #{account} - #{accountName}
          <td> #{formatDouble debit}
          <td> #{formatDouble credit}
          <td> #{formatAbs debit credit}
  |]

-- | Check whether new transactions have been entered
-- in the past (i.e. before the last collection date)
-- and are therefore unreachable
getGLCheckItemCostCheckPastR :: Handler Html
getGLCheckItemCostCheckPastR = do
  accounts <- getStockAccounts
  uncollectables <- mapM loadUncollectables accounts
  let renderDuplicates_ _ [] = return ""
      renderDuplicates_ (Account account) duplicates =
        renderDuplicates account ("Uncollectables", duplicates)

  w <- mconcat <$> zipWithM renderDuplicates_ accounts uncollectables
  defaultLayout $ do
    setTitle "Item Cost - Check uncollectable transactions"
    w
    [whamlet|
    <form.from-inline method=POST action="@{GLR $ GLCheckItemCostCheckPastRefreshR}">
      <button.btn.btn-warning> Refresh
    <form.from-inline method=POST action="@{GLR $ GLCheckItemCostCheckPastPurgeR}">
      <button.btn.btn-danger> Purge
      |]
      
  

postGLCheckItemCostCheckPastPurgeR :: Handler Html
postGLCheckItemCostCheckPastPurgeR = do
  accounts <- getStockAccounts
  uncollectables <- mapM loadUncollectables accounts
  
  let toCriteria (Account account) (th, _) =
           [ ItemCostTransactionSku ==. gett (Just . FA.stockMoveStockId)  FA.glTranStockId th
           , ItemCostTransactionAccount ==. account
           , ItemCostTransactionDate >=. gett FA.stockMoveTranDate FA.glTranTranDate th
           ]

      purge account = mapM_ $ purgeTransactions . toCriteria account
  zipWithM_ purge accounts uncollectables
  setSuccess "Uncollectables items purged succesfully"
  getGLCheckItemCostR
  

postGLCheckItemCostCheckPastRefreshR :: Handler Html
postGLCheckItemCostCheckPastRefreshR = do
  accounts <- getStockAccounts
  uncollectables <- mapM loadUncollectables accounts
  let refreshAccount ths account = do
        mapM_ ( refreshSummaryFrom account
              . gett (Just . FA.stockMoveStockId)  FA.glTranStockId
              . fst
              ) ths
  runDB $ zipWithM_ refreshAccount uncollectables accounts
  getGLCheckItemCostCheckPastR

-- * Saving 
loadAndCollectAccount account = do
  (date, _form, _encType) <- extractDateFromUrl
  sku'counts <- loadPendingTransactionCountFor date account
  let skus = [sku | (sku,count,_) <- sku'counts, count /= 0]
  mapM (collectCostTransactions date account) skus

postGLCheckItemCostAccountCollectR :: Text -> Handler Html
postGLCheckItemCostAccountCollectR account = do
  transE <- loadAndCollectAccount (Account account)
  renderWithDuplicate (redirect $ GLR (GLCheckItemCostAccountViewR account)) account transE

renderWithDuplicate :: Handler Html -> Text ->  [Either (Text, [Matched]) ()] -> Handler Html
renderWithDuplicate onSuccess account transE =
  case partitionEithers transE of
    ([], _ ) -> onSuccess
    (duplicatess, _ ) -> do
      w <- mconcat <$> mapM (renderDuplicates account) duplicatess
      defaultLayout $ do
        setTitle . toHtml $ "Item Cost - collect error " <> account 
        w

postGLCheckItemCostItemCollectR :: Text -> Maybe Text -> Handler Html
postGLCheckItemCostItemCollectR account skum = do
  (date, _form, _encType) <- extractDateFromUrl
  sku'counts <- loadPendingTransactionCountFor date (Account account)
  let skus = [sku | (sku,count,_) <- sku'counts, count /= 0, sku == skum]
  transE <- mapM (collectCostTransactions date (Account account)) skus
  renderWithDuplicate (redirect $ GLR (GLCheckItemCostItemViewSavedR account skum)) account transE

postGLCheckItemCostCollectAllR :: Handler Html
postGLCheckItemCostCollectAllR = do
  accounts <- getStockAccounts
  transEs <- mapM loadAndCollectAccount accounts
  renderWithDuplicate (redirect $ GLR (GLCheckItemCostR)) "Any"  (concat transEs)

-- ** Purge 

postGLCheckItemCostPurgeR :: Handler Html
postGLCheckItemCostPurgeR = do
  purgeTransactions [ItemCostTransactionItemCostValidation ==. Nothing]
  redirect $ GLR GLCheckItemCostR

postGLCheckItemCostPurgeAccountR :: Text -> Handler Html
postGLCheckItemCostPurgeAccountR account = do
  purgeTransactions [ItemCostTransactionAccount ==. account, ItemCostTransactionItemCostValidation ==. Nothing]
  redirect $ GLR $ GLCheckItemCostAccountViewR account

postGLCheckItemCostPurgeAccountItemR :: Text -> Maybe Text -> Handler Html
postGLCheckItemCostPurgeAccountItemR account sku = do
  purgeTransactions [ ItemCostTransactionAccount ==. account
                    , ItemCostTransactionSku ==. sku
                    , ItemCostTransactionItemCostValidation ==. Nothing
                    ]
  redirect $ GLR $ GLCheckItemCostItemViewR account sku
-- ** Upate GL 
--
postGLCheckItemCostUpdateGLR :: Handler Html
postGLCheckItemCostUpdateGLR = do
  (date, _, _) <- extractDateFromUrl
  summaries <- runDB $ selectList [] []
  validationm <- fixGLBalance date summaries
  case validationm of
    Nothing ->  do
      setWarning [shamlet| Not validation saved|]
      redirect $ GLR $ GLCheckItemCostValidationsViewR
    Just (Entity key _ ) -> do
        setSuccess [shamlet|
           Validation ##{tshow (fromSqlKey key)} created.
                   |]
        redirect $ GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)

postGLCheckItemCostUpdateGLAccountR :: Text -> Handler Html
postGLCheckItemCostUpdateGLAccountR account = do
  (date, _, _) <- extractDateFromUrl
  summaries <- runDB $ selectList [ItemCostSummaryAccount ==. account] []
  validationm <- fixGLBalance date summaries
  case validationm of
    Nothing ->  do
      setWarning [shamlet| Not validation saved|]
      redirect $ GLR $ GLCheckItemCostValidationsViewR
    Just (Entity key _ ) -> do
        setSuccess [shamlet|
           Validation ##{tshow (fromSqlKey key)} created.
                   |]
        redirect $ GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)

postGLCheckItemCostUpdateGLAccountItemR :: Text -> Maybe Text -> Handler Html
postGLCheckItemCostUpdateGLAccountItemR account skum = do
  (date, _, _) <- extractDateFromUrl
  summary <- loadCostSummary (Account account) skum
  validationm <- fixGLBalance date (maybeToList summary)
  case validationm of
    Nothing ->  do
      setWarning [shamlet| Not validation saved|]
      redirect $ GLR $ GLCheckItemCostValidationsViewR
    Just (Entity key _ ) -> do
        setSuccess [shamlet|
           Validation ##{tshow (fromSqlKey key)} created.
                   |]
        redirect $ GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)
--
-- ** Upate Cost 
--
postGLCheckItemCostUpdateCostR :: Handler Html
postGLCheckItemCostUpdateCostR = do
  summaries <- runDB $ selectList [] []
  validationm <- updateCosts summaries
  case validationm of
    Nothing ->  do
      setWarning [shamlet| Not validation saved|]
      redirect $ GLR $ GLCheckItemCostValidationsViewR
    Just (Entity key _ ) -> do
        setSuccess [shamlet|
           Validation ##{tshow (fromSqlKey key)} created.
                   |]
        redirect $ GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)

postGLCheckItemCostUpdateCostAccountR :: Text -> Handler Html
postGLCheckItemCostUpdateCostAccountR account = do
  summaries <- runDB $ selectList [ItemCostSummaryAccount ==. account] []
  validationm <- updateCosts summaries
  case validationm of
    Nothing ->  do
      setWarning [shamlet| Not validation saved|]
      redirect $ GLR $ GLCheckItemCostValidationsViewR
    Just (Entity key _ ) -> do
        setSuccess [shamlet|
           Validation ##{tshow (fromSqlKey key)} created.
                   |]
        redirect $ GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)

postGLCheckItemCostUpdateCostAccountItemR :: Text -> Maybe Text -> Handler Html
postGLCheckItemCostUpdateCostAccountItemR account skum = do
  summary <- loadCostSummary (Account account) skum
  validationm <- updateCosts (maybeToList summary)
  case validationm of
    Nothing ->  do
      setWarning [shamlet| Not validation saved|]
      redirect $ GLR $ GLCheckItemCostValidationsViewR
    Just (Entity key _ ) -> do
        setSuccess [shamlet|
           Validation ##{tshow (fromSqlKey key)} created.
                   |]
        redirect $ GLR $ GLCheckItemCostValidationViewR (fromSqlKey key)

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
