{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-omit-yields #-}
module Util.CacheSpec where

import Prelude hiding(log)
import Test.Hspec
import Util.Cache
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Concurrent -- (threadDelay, yield)
-- import UnliftIO (timeout)

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
  reverse l `shouldBe` expected

clearLogs = do
  writeIORef logs []

sleepMilli :: Int -> IO ()
-- sleepMilli milli = timeout (milli*1000) (newEmptyMVar >>= takeMVar ) >> return ()
sleepMilli milli = threadDelay (milli*1000) -- -| milli <= 10000000000 =  threadDelay  (milli*1000) >> return ()
_sleepMilli milli = do
  print "BAD"
  threadDelay (100*1000)
  -- print ("sleeping for" ++ show milli) --  $ (milli, sum  $ reverse [1..10000000])
  yield -- give a chance to be cancelled
  sleepMilli (milli-100)

spec :: Spec
spec = describe "Cache" $ before_ clearLogs $ do
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
      sleepMilli 300
      logsShouldBe []
    it "stops when cancelled whilst waiting" $ do
      delayed <- createDelayed $ 
        do
          log "starting"
          sleepMilli 2000
          log "finish"
      cancelDelayed delayed
      sleepMilli 2300
      logsShouldBe []
    it "stops when cancelled whilst processing" $ do
      delayed <- createDelayed $ 
        do
          log "starting"
          sleepMilli 2000
          log "finish"
      startDelayed delayed
      sleepMilli 300
      cancelDelayed delayed
      log "cancelled"
      print "CANCELLED --------------------------------------------------"
      sleepMilli 2000
      logsShouldBe ["starting", "cancelled"]
  context "PreCache" $ do
    it "is executed when called" $ do
      cache <- newExpiryCache
      delayed <- preCache False cache "A" (
        do
          log "starting"
          sleepMilli 200
          log "done"
          return (1 :: Int)
        ) (cacheHour 1)

      sleepMilli 50
      logsShouldBe []
      _ <- startDelayed  delayed
      sleepMilli 250
      logsShouldBe ["starting", "done"]
    it "is aborted when purged" $ do
      cache <- newExpiryCache
      delayed <- preCache False cache "A" (
        do
          log "starting"
          sleepMilli 2000
          log "done"
          return (1 :: Int)
        ) (cacheHour 1)

      _ <- startDelayed  delayed
      sleepMilli 300
      purgeKey cache "A"
      sleepMilli 2000
      logsShouldBe ["starting"]






