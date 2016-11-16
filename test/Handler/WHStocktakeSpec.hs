{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHStocktakeSpec where

import TestImport
import Text.Shakespeare.Text (st)

spec :: Spec
spec = appSpec


appSpec = withAppNoDB BypassAuth $ do
  describe "upload stocktake" $ do
    describe "upload page" $ do
      it "propose to upload a file" (const pending)
        -- get WHStocktakeR
        -- status 200

        -- bodyContains "upload"

    describe "upload" $ do
      it "parses correctly a correct file" (const pending) --  $ do
        -- postStocktakeSheet 200
          -- [st||]

    describe "process blanks" $ do
      it "fills barcode prefix" (const pending)
      it "fills barcode sequence" (const pending)
      it "fills everything else  with sequence" $ do  (const pending)

      it "groups mixed colours" (const pending)

    it "detects incorrect barcodes" (const pending)
    it "saves " (const pending)

