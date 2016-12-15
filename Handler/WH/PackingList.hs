{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Handler.WH.PackingList
( getWHPackingListR
, postWHPackingListR
, postWHPackingListSaveR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils
import qualified Data.Csv as Csv
import qualified Data.Map as Map

data Mode = Validate | Save deriving (Eq, Read, Show)

-- | Type of file
data Format = PartialFirst deriving (Eq, Read, Show)
data UploadParam = UploadParam
  { reference :: Text
  , description :: Textarea
  , spreadsheet :: Textarea
  } deriving (Eq, Read, Show)

uploadForm = renderBootstrap3 BootstrapBasicForm form
  where form = UploadParam
            <$> (areq textField "reference" Nothing)
            <*> (areq textareaField "description" Nothing )
            <*> (areq textareaField "spreadsheet" Nothing )

getWHPackingListR :: Handler Html
getWHPackingListR = renderWHPackingList Validate


renderWHPackingList :: Mode -> Handler Html
renderWHPackingList mode = do
  let btn = case mode of
        Validate -> "primary" :: Text
        Save -> "danger"
  (form, encType) <- generateFormPost uploadForm 
  defaultLayout [whamlet|
    <div.well>
      <form #upload-form role=form method=post action=@{WarehouseR WHPackingListR} enctype=#{encType}>
        ^{form}
        <button type="submit" name="#{tshow mode}" class="btn btn-#{btn}">#{tshow mode}
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


data Row s = Row
  { style :: FieldTF s Text
  , colour :: FieldTF s Text
  , orderQuantity :: FieldTF s Int
  , firstCartonNumber :: FieldTF s Int
  , lastCartonNumber :: FieldTF s Int
  , numberOfCarton :: FieldTF s Int
  , quantityPerCarton :: FieldTF s Int
  , totalQuantity :: FieldTF s Int
  , length :: FieldTF s  Double
  , width :: FieldTF s  Double
  , height :: FieldTF s  Double
  }

data RowTypes = Raw | FullBox | PartialBox | Total | Container deriving (Eq, Read, Show) 

type family FieldTF (s :: RowTypes) a where
  FieldTF s a = Either InvalidField a

columnNames :: [(Csv.Field, [String])]
columnNames = [("Style", ["S", "s", "Style", "style", "Style No.", "Style No"] )
             ,("quantity", ["q", "Q", "QTY", "qty", "order QTY"])
             ,("1st carton number", ["start", "f", "F", "cn", "C/NO", "first"])
             ,("last carton number", ["end", "last", "e"])
             ,("number of carton", ["CTNS", "n", "N"])
             ,("quantity per carton", ["qc", "Q/C", "QTY/CTN"])
             ,("total quantity", ["Total", "t", "T", "TOTAL"])
             ,("length", ["l", "L", "Length", "LENGTH"])
             ,("width", ["w", "W", "Width", "WIDTH"])
             ,("height", ["h", "H", "Height", "HEIGHT"])
             ]

instance Csv.FromNamedRecord (Row Raw) where
  parseNamedRecord m = let
    colMap = Map.fromList columnNames
    [style, col, qty, cn, cne, n, qc, tot, l, w, h] = map fst columnNames
    parse = parseMulti colMap
    in pure Row
        <*> m `parse` style
        <*> m `parse` col
        <*> m `parse` qty
        <*> m `parse` cn
        <*> m `parse` cne
        <*> m `parse` n
        <*> m `parse` qc
        <*> m `parse` tot
        <*> m `parse` l
        <*> m `parse` w
        <*> m `parse` h
