module Handler.Items.Category where
import Import
import Database.Persist.MySQL

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
  defaultLayout [whamlet|
      <div.well>
          <ul>
            $forall (Single term, Single co) <- terms
              <li>#{term} (#{co})
                 |]


