module Handler.WH.PackingList
( getWHPackingListR
, postWHPackingListR
, postWHPackingListSaveR
) where

import Import
import Yesod.Form.Bootstrap3

data Mode = Validate | Save deriving (Eq, Read, Show)
data UploadParam = UploadParam
  { reference :: Text
  , comment :: Textarea
  , spreadsheet :: Textarea
  } deriving (Eq, Read, Show)

uploadForm = renderBootstrap3 BootstrapBasicForm form
  where form = UploadParam
            <$> (areq textField "reference" Nothing)
            <*> (areq textareaField "comment" Nothing )
            <*> (areq textareaField "spreadsheet" Nothing )

getWHPackingListR :: Handler Html
getWHPackingListR = renderWHPackingList Validate


renderWHPackingList :: Mode -> Handler Html
renderWHPackingList mode = do
  let btn = case mode of
        Validate -> "primary" :: Text
        Save -> "primary"
  (form, encType) <- generateFormPost uploadForm 
  defaultLayout [whamlet|
    <div.well>
      <form #upload-form role=form method=post action=@{WarehouseR WHPackingListR} enctype=#{encType}>
        ^{form}
        <button type="submit" name="#{tshow mode}" class="btn bt-#{btn}">#{tshow mode}
                        |]
  
postWHPackingListR :: Handler Html
postWHPackingListR = do
  action <- lookupPostParam "action"
  ((resp, view), encType) <- runFormPost uploadForm
  case resp of
    FormMissing -> error "Form Missing"
    FormFailure a -> defaultLayout [whamlet|^{view}|]
    FormSuccess param ->  do
      processUpload (fromMaybe Validate (readMay =<< action)) param
      
  
processUpload :: Mode -> UploadParam -> Handler Html
processUpload mode param = do
  undefined

postWHPackingListSaveR :: Handler Html
postWHPackingListSaveR = undefined

-- getWHPackingListViewR :: Int64 -> Handler Html
-- getWHPackingListViewR plId = undefined

-- getWHPackingListTextcartR :: Int64 -> Handler Html
-- getWHPackingListTextcartR plId = undefined
