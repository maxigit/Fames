{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module RoleSpec where

import TestImport
import Data.Yaml (decodeThrow)
import Text.Shakespeare.Text (st)

spec :: Spec
spec = pureSpec


pureSpec = do
  describe "@pure @parallel #authorizing from route attributes:" $ do
    context "two attributes" $ do
      let fa = "fa"
          gl = "gl"
          attrs = setFromList [fa, gl]

      it "doesn't authorize if missing roles" $ do
        authorizeFromAttributes (RolePermission (setFromList [(fa, ReadRequest, mempty)])) attrs ReadRequest
        `shouldBe` False
      it "from one role" $ do
        authorizeFromAttributes (RolePermission (setFromList [(fa, ReadRequest, mempty)
                                                             , (gl, ReadRequest, mempty)
                                                             ])) attrs ReadRequest
        `shouldBe` True
      it "from many roles" $ do
        authorizeFromAttributes (RoleGroup [RolePermission (setFromList [(fa, ReadRequest, mempty)])
                                           , RolePermission (setFromList [(gl, ReadRequest, mempty)])
                                           ]) attrs ReadRequest
        `shouldBe` True
      it "authorizes Administrator" $ do
        authorizeFromAttributes Administrator attrs ReadRequest `shouldBe` True

      it "authorizes Read If Write" $ do
        authorizeFromAttributes (RolePermission $ setFromList [(fa, WriteRequest, mempty)]) [fa] ReadRequest
        `shouldBe` True
      it "authorizes Write If Write" $ do
        authorizeFromAttributes (RolePermission $ setFromList [(fa, WriteRequest, mempty)]) [fa] WriteRequest
        `shouldBe` True
      it "doesn't Write If Read" $ do
        authorizeFromAttributes (RolePermission $ setFromList [(fa, ReadRequest, mempty)]) [fa] WriteRequest
        `shouldBe` False
                                            

  describe "#authorizing from URL" $ do
    it "doesn't authorize if missing route" $ do
      authorizeFromPath (RoleRoute "bad" ReadRequest) "/fa/gl_trans" ReadRequest `shouldBe` False
    it "authorizes Read if Read" $ do
      authorizeFromPath (RoleRoute "/fa/gl_trans" ReadRequest) "/fa/gl_trans" ReadRequest `shouldBe` True
    it "authorizes Read if Write" $ do
      authorizeFromPath (RoleRoute "/fa/gl_trans" WriteRequest) "/fa/gl_trans" ReadRequest `shouldBe` True
    it "doesn't authorize Write if Read" $ do
      authorizeFromPath (RoleRoute "/fa/gl_trans" ReadRequest) "/fa/gl_trans" WriteRequest `shouldBe` False
    it "#authorizing from URL with parameter" $ do
      authorizeFromPath (RoleRoute "/fa/gl_trans/*" ReadRequest) "/fa/gl_trans/3" WriteRequest `shouldBe` False
    
  describe "#Yaml" $ do
    let shouldDecodeTo yaml result = decodeThrow (encodeUtf8 yaml) `shouldBe` Just result
    it "decodes Administrator" $ do
      "Administrator" `shouldDecodeTo` Administrator

    it "decodes route" $ do
      "/fa/gl" `shouldDecodeTo` RoleRoute "/fa/gl" ReadRequest
    it "decodes writable route" $ do
      "/fa/gl+" `shouldDecodeTo` RoleRoute "/fa/gl" WriteRequest

    it "decodes route attribute" $ do
      "fa" `shouldDecodeTo` RolePermission [("fa", ReadRequest, mempty)]
    it "decodes route attributes" $ do
      "fa gl" `shouldDecodeTo` RolePermission [("fa", ReadRequest, mempty), ("gl", ReadRequest, mempty)]
    it "decodes writable route attributes" $ do
      "fa+ db gl+" `shouldDecodeTo` RolePermission [("fa", WriteRequest, mempty)
                                                   , ("db", ReadRequest, mempty)
                                                   , ("gl", WriteRequest, mempty)
                                                   ]
    it "decodes group" $ do
      [st|
- fa gl
- /web/admin+
       |] `shouldDecodeTo` RoleGroup [ RolePermission [("fa", ReadRequest, mempty), ("gl", ReadRequest, mempty)]
                                     , RoleRoute "/web/admin" WriteRequest
                                     ]
  
    it "decodes nested groups" $ do
      [st|
- fa gl
- &Web
  - /web+
  - /web/admin
       |] `shouldDecodeTo` RoleGroup [ RolePermission [("fa", ReadRequest, mempty), ("gl", ReadRequest, mempty)]
                                     , RoleGroup [ RoleRoute "/web" WriteRequest
                                                 , RoleRoute "/web/admin" ReadRequest
                                                 ]
                                     ]
    it "decodes maps" $ do
      [st|
Accountant: /fa/gl
       |] `shouldDecodeTo`  ( mapFromList [ ("Accountant" , RoleRoute "/fa/gl" ReadRequest)]
                            :: Map Text Role
                            )
    it "decodes anchors" $ do
      [st|
acc1: &Accountant /fa/gl
acc2: *Accountant

       |] `shouldDecodeTo`  ( mapFromList [ ("acc1" , RoleRoute "/fa/gl" ReadRequest)
                                          , ("acc2" , RoleRoute "/fa/gl" ReadRequest)
                                          ]
                            :: Map Text Role
                            )
