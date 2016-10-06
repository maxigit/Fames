{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Barcode where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))

import Formatting
-- | Allowed prefixes. Read from configuration file.
allowedPrefixes :: Handler [(Text, Text)]
allowedPrefixes = return $ [("ST", "Stock take")]
  
barcodeForm :: [(Text, Text)] -> Form (Text, Int)
barcodeForm prefixes = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField "Prefix" Nothing
  <*> areq intField "Number" Nothing

getWHBarcodeR :: Handler Html
getWHBarcodeR = do
  prefixes <- allowedPrefixes
  (form, encType) <- generateFormPost (barcodeForm prefixes)
  defaultLayout [whamlet|
<h1> Barcode Generator
  <form #barcode-form role=form method=post action=@{WarehouseR WHBarcodeR} enctype=#{encType}>
    ^{form}
    <button type="submit" .btn .btn-default>Download
|]

postWHBarcodeR :: Handler Html 
postWHBarcodeR = do
  prefixes <- allowedPrefixes
  ((resp, textW), encType) <- runFormPost $ barcodeForm prefixes
  case resp of
    FormMissing -> error "missing"
    FormSuccess (prefix, number) -> do
      addHeader "Content-Disposition" "attachment"
      addHeader "Filename" "barcodes.csv"
      let fmt = format $ (fitLeft 2 %. stext) % "000" % "-" % left 3 '0'
          serie = 21 :: Int
          barcodes = [fmt prefix serie n  | n <- [1..number]]
      defaultLayout [whamlet|
<ul>
  $forall (barcode) <- barcodes
    <li>
        #{barcode}
|]
      
