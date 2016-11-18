{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHStocktakeSpec where

import TestImport
import Text.Shakespeare.Text (st)

spec :: Spec
spec = appSpec

relPath path = "test/Handler/WHStocktakeSpec/" ++ path

uploadSTSheet status path = do
  get (WarehouseR WHStocktakeR)
  statusIs 200

  request $ do
    setMethod "POST"
    setUrl (WarehouseR WHStocktakeR)
    addToken_ "form#upload-form"
    fileByLabel "upload" path "text/plain"
    byLabel "encoding" (tshow $ 1)

  -- printBody
  statusIs status

-- write the sheet to a temporary file and upload it.
-- more convenient for testing
postSTSheet status sheet = do
  path <- saveToTempFile sheet
  uploadSTSheet status path

appSpec = withAppNoDB BypassAuth $ do
  describe "upload stocktake" $ do
    describe "upload page" $ do
      it "propose to upload a file" $ do
        get (WarehouseR WHStocktakeR)
        statusIs 200

        bodyContains "upload"

    describe "upload" $ do
      it "parses correctly a correct file" $ do
        postSTSheet 200 [st||]

    describe "process blanks" $ do
      it "fills barcode prefix" (const pending)
      it "fills barcode sequence" (const pending)
      it "fills everything else  with sequence" $ do  (const pending)

      it "groups mixed colours" (const pending)

    it "detects incorrect barcodes" (const pending)
    it "saves " (const pending)

