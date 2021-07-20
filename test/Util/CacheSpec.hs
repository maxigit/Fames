{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Util.CacheSpec where

import Prelude hiding(log)
import Test.Hspec
import Util.Cache
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Concurrent(threadDelay)
-- | In order to capture what's been done or not
-- we create globals logs which allow us to push message
-- and use it later as expectation.
{-# NOINLINE logs #-}
logs :: IORef [String]
logs = unsafePerformIO $ newIORef []
log :: String -> IO ()
log msg = do
  print msg
  modifyIORef logs (msg:)
  return ()

logsShouldBe expected = do
  l <- readIORef logs
  expected `shouldBe` reverse l

clearLogs = do
  writeIORef logs []

spec :: Spec
spec = describe "@focus Cache" $ before_ clearLogs $ do
context "DelayedCache" $ do
  it "is executed when got" $ do
    delayed <- createDelayed  (log "Hello">> return 1)
    a <- getDelayed delayed
    a `shouldBe` 1
    logsShouldBe ["Hello"]
  it "is executed when output discarded " $ do
    delayed <- createDelayed  (log "Hello")
    _ <- getDelayed delayed
    logsShouldBe ["Hello"]
  it "is not executed when not called" $ do
    _delayed <- createDelayed  (log "Hello" >> return 1)
    -- cancelDelayed delayed
    threadDelay (3*1000000)
    logsShouldBe []
  it "stops when cancelled" $ do
    delayed <- createDelayed $ 
      do
        log "starting"
        threadDelay (2*1000000)
        log "finish"
    startDelayed delayed
    cancelDelayed delayed
    threadDelay (3*1000000)
    logsShouldBe ["starting"]


