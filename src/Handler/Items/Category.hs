module Handler.Items.Category where
import Import
import Database.Persist.MySQL
import Yesod.Form.Bootstrap3
import Handler.Util
import Handler.Table
import Handler.Items.Category.Cache
import Data.Yaml(decodeEither, encode)
import qualified FA as FA

-- * List of categories
-- | Displays the list of categories
getItemsCategoryR :: Handler Html
getItemsCategoryR = do
  cats <- categoriesH
  defaultLayout [whamlet|
   <div.well>
     <ul>
       $forall cat <- cats
         <li>
           <a href=@{ItemsR (ItemsCategoryTermsR cat)}>
             #{cat}
                 |]

getItemsCategoryTermsR :: Text -> Handler Html
getItemsCategoryTermsR name = do
  let sql = "select value, count(*) FROM fames_item_category_cache WHERE category = ? GROUP BY value"
  terms <- runDB $ rawSql sql [PersistText name]
  let _types = terms :: [(Single Text, Single Int)]
  setInfo [shamlet|
    Available fields are
    <ul>
      $forall field <- allFields
        <li>#{field}
                  |]
  defaultLayout [whamlet|
      <div.well>
        <form.form role=form method=GET action=@{AdministratorR (AResetCategoryCacheR)}>
          <input type=hidden name=category value="#{name}"> 
          <label for=stockFilterF> Skus
          <input#stockFilterF type="text" name=stockFilter>
          <button.btn.btn-danger> Reset category for all items
      <div.well>
          <table.table.table-hover.table-striped>
            <tr>
              <th> Value
              <th> Number of items
            $forall (Single term, Single co) <- terms
              <tr>
                <td>#{term}
                <td>
                  <a href=@?{(ItemsR (ItemsIndexR Nothing), [("category", name), ("category-filter", term)])}>#{co}
                 |]


-- * Test and previous category configuration
data TesterParam = TesterParam
  { tpStockFilter :: FilterExpression
  , tpConfiguration :: Textarea
  , tpDeliveryRule :: Maybe Textarea
  , tpShowFields :: Bool
  } deriving Show

categoryTesterForm :: Maybe TesterParam -> _
categoryTesterForm param = renderBootstrap3 BootstrapBasicForm form
  where form = TesterParam
         <$> areq filterEField "styles" (tpStockFilter <$> param)
         <*> areq textareaField "configuration" (tpConfiguration <$> param)
         <*> aopt textareaField "delivery  rule" (tpDeliveryRule <$> param)
         <*> areq boolField "show fields" (tpShowFields <$> param)

getItemsCategoryTesterR :: Handler Html
getItemsCategoryTesterR = do
  rulesMaps <- appCategoryRules <$> getsYesod appSettings
  deliveryRule <- appDeliveryCategoryRule <$> getsYesod appSettings
  let configuration = Textarea . decodeUtf8 $ encode rulesMaps
      deliveryConf = Textarea . decodeUtf8 . encode <$> deliveryRule
      filter = ""
  renderCategoryTester (TesterParam filter configuration deliveryConf False) Nothing

postItemsCategoryTesterR :: Handler Html
postItemsCategoryTesterR = do
  ((resp, formW), encType) <- runFormPost (categoryTesterForm Nothing)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      widget <- loadCategoriesWidget param
      renderCategoryTester param (Just widget)

renderCategoryTester :: TesterParam -> Maybe Widget -> Handler Html
renderCategoryTester param resultM = do
  (form, encType) <- generateFormPost (categoryTesterForm $ Just param)
  defaultLayout [whamlet|
  <form #category-tester role=form method=post action="@{ItemsR ItemsCategoryTesterR}" encType="#{encType}">
    ^{form}
    <button.btn.btn-primary type=submit>Submit
  $maybe result <- resultM
    <div.well #category-tester-result>
      ^{result}
                        |]

allFields :: [String]
allFields = [ "description"
            , "longDescription"
            , "unit"
            , "mbFlag"
            , "taxType"
            , "category"
            , "dimension1"
            , "dimension2"
            , "salesAccount"
            , "cogsAccount"
            , "inventoryAccount"
            , "adjustmentAccount"
            , "fifo-deliveries"
            , "fifo-deliveries-with-qoh"
            , "fifo-deliveries-with-short-qoh"
            , "fifo-deliveries-by-qoh" 
            ]
loadCategoriesWidget :: TesterParam -> Handler Widget
loadCategoriesWidget (TesterParam stockFilter configuration deliveryConf showFields) = do 
  let rulesE = decodeEither (encodeUtf8 $ unTextarea configuration) :: Either String [Map String CategoryRule]
      deliveryRuleE = sequence $ (decodeEither . encodeUtf8 . unTextarea) <$>  deliveryConf :: Either String (Maybe CategoryRule)
  case (rulesE, deliveryRuleE) of
    (Left err, _) -> setError "Error in configuration" >> return [whamlet|<div.well>#{err}|]
    (_, Left err) -> setError "Error in delivery configuration" >> return [whamlet|<div.well>#{err}|]
    (Right ruleMaps, Right deliveryRule) -> do
      let rules = map (first unpack) (concatMap mapToList ruleMaps)
          all = if showFields then  allFields else []
          categories = Nothing : map Just all <>   map (Just . fst) rules 
          headerFn = (,[]) . maybe "sku" toHtml
          makeRow :: (Key FA.StockMaster, Map String String) -> (Maybe String -> (Maybe (Html, [Text])), [Text])
          makeRow (sku, cats) = let
            f col = case col of
                      Nothing -> Just (toHtml $ FA.unStockMasterKey sku, [])
                      Just cat -> fmap ((,[]) . toHtml) $ lookup cat cats

            in (f, [])
      stockMasters <- loadStockMasterRuleInfos stockFilter
      sku'categories <- forM (take 100 stockMasters) $ \sm -> do
            deliveries <- runDB $ loadItemDeliveryForSku (smStockId sm)
            return $ applyCategoryRules [] deliveryRule rules (sm { smDeliveries = deliveries })
      return $ displayTable categories headerFn (map makeRow sku'categories)
