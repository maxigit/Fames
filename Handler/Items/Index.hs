{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Index where

import Import
import Yesod.Form.Bootstrap3

-- * Types
-- | SQL text filter expression. Can be use either the LIKE syntax or the Regex one.
-- Regex one starts with '/'.
data FilterExpression = LikeFilter Text  | RegexFilter Text deriving (Eq, Show, Read)
data IndexParam = IndexParam
  { ipStyles :: Maybe FilterExpression
  , ipVariations :: Maybe FilterExpression
  } deriving (Eq, Show, Read)

-- * Utils

showFilterExpression :: FilterExpression -> Text
showFilterExpression (LikeFilter t) = t
showFilterExpression (RegexFilter t) = "/" <> t

readFilterExpression :: Text -> FilterExpression
readFilterExpression t = case stripPrefix "/" t of
  Nothing -> LikeFilter t
  Just regex -> RegexFilter regex

filterEField = convertField readFilterExpression showFilterExpression textField

  

itemsTable :: Maybe FilterExpression -> Maybe FilterExpression -> Handler Html
itemsTable styleF varF = do
  return "pipo"

-- * Rendering
getItemsIndexR :: Handler Html
getItemsIndexR = renderIndex Nothing ok200

indexForm param = renderBootstrap3 BootstrapBasicForm form
  where form = IndexParam
          <$> (aopt filterEField "styles" (fmap ipStyles param))
          <*> (aopt filterEField "variations" (fmap ipStyles param))

renderIndex :: (Maybe IndexParam) -> Status -> Handler Html
renderIndex param status = do
  (form, encType) <- generateFormPost (indexForm param)
  index <- itemsTable (ipStyles =<< param) (ipVariations =<< param)
  let widget = [whamlet|
<div #items-index>
  <div.well>
    <form #items-form role=form method=post action=@{ItemsR ItemsIndexR} enctype=#{encType}>
      ^{form}
      <button type="submit" name="search" class="btn btn-default">Search
 <div.well>
   ^{index}
|]
  sendResponseStatus status =<< defaultLayout widget


  




