{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module RoleSpec where

import TestImport
import Import.NoFoundation
import Role

spec :: Spec
spec = pureSpec


pureSpec = do
  describe "#authorizing from route attributes:" $ do
    context "two attributes" $ do
      let fa = "fa"
          gl = "gl"
          attrs = setFromList [fa, gl]

      it "doesn't authorize if missing roles" $ do
        authorizeFromAttributes (RolePermission (setFromList [(fa, ReadRequest)])) attrs ReadRequest
        `shouldBe` False
      it "from one role" $ do
        authorizeFromAttributes (RolePermission (setFromList [(fa, ReadRequest)
                                                             , (gl, ReadRequest)
                                                             ])) attrs ReadRequest
        `shouldBe` True
      it "from many roles" $ do
        authorizeFromAttributes (RoleGroup [RolePermission (setFromList [(fa, ReadRequest)])
                                           , RolePermission (setFromList [(gl, ReadRequest)])
                                           ]) attrs ReadRequest
        `shouldBe` True
      it "authorizes Administrator" $ do
        authorizeFromAttributes Administrator attrs ReadRequest `shouldBe` True

      it "authorizes Read If Write" $ do
        authorizeFromAttributes (RolePermission $ setFromList [(fa, WriteRequest)]) [fa] ReadRequest
        `shouldBe` True
      it "authorizes Write If Write" $ do
        authorizeFromAttributes (RolePermission $ setFromList [(fa, WriteRequest)]) [fa] WriteRequest
        `shouldBe` True
      it "doesn't Write If Read" $ do
        authorizeFromAttributes (RolePermission $ setFromList [(fa, ReadRequest)]) [fa] WriteRequest
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
    
      -- pending "parse json"

    
  
