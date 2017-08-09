module Util.Cache where

import Prelude
import Data.Dynamic
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time

-- * Delayed
-- | A syncronous actions, which can be started from another thread.
-- Usefull to get value pre-cached for the next request.
data Delayed a = Delayed
  { blocker :: MVar ()
  , action :: Async a
  } deriving Typeable

data DelayedStatus = Waiting | InProgress | Finished | OnError deriving Show
  
  

createDelayed :: IO a -> IO (Delayed a)
createDelayed a = do
   mvar <- newEmptyMVar
   -- don't actually start the action until mvar has been set
   as <- async (takeMVar mvar >> a)
   return (Delayed mvar as)

-- | Start the delayed action
startDelayed :: Delayed a -> IO ()
startDelayed d = do
  _ <- tryPutMVar (blocker d) ()
  return ()


-- | Get the actual value
getDelayed :: Delayed a -> IO a
getDelayed d = do
  startDelayed d
  wait (action d)

-- | Check if the job status
statusDelayed :: Delayed a -> IO DelayedStatus
statusDelayed d = do
  empty_ <- isEmptyMVar (blocker d)
  if empty_
     then return Waiting
     else do
       pollStatus <- poll (action d)
       case pollStatus of
          Nothing -> return InProgress
          Just (Left _) -> return OnError
          Just (Right _ ) -> return Finished

-- * Cached
-- | Mutable Map with expiry date
-- A value of Nothing means the MVar
type ExpiryCache = Map String (MVar (Dynamic, UTCTime))

-- ** Standard cache
newExpiryCache :: Monoid a => IO (MVar a)
newExpiryCache = newMVar mempty

-- Compute or use the cached value.
-- There is no way to get a value from the cache without actually
-- specifying the way to compute value. This is to make sure
-- the retrieved value is of the correct type.
expCache :: (Show k, Typeable v) => MVar ExpiryCache -> k -> IO v -> Int -> IO v
expCache cvar key vio seconds  = do
  let k = show key
  -- we release cvar as soon as possible
  let putV mvar = do
                  v <- vio
                  now <- getCurrentTime
                  putMVar mvar (toDyn v, addUTCTime (fromIntegral seconds) now)
                  return v
  let getCachedMVar cache = do
        case Map.lookup k cache of
          Nothing -> do -- key need creating
            mvar <- newEmptyMVar
            let todo = putV mvar
            return (Map.insert k mvar cache, Left todo)
          Just mvar -> do
            return (cache, Right mvar)
  (todo'mvar) <- modifyMVar cvar getCachedMVar 
  case todo'mvar of
    Left todo -> do
      todo
    Right mvar -> do
      (d, t) <- takeMVar mvar
      now <- getCurrentTime
      if now <= t
        then return $ fromDyn d (error "Wrong type for cached key.")
        else putV mvar

purgeKey :: Show k => MVar ExpiryCache -> k -> IO ()
purgeKey cvar key = modifyMVar_ cvar go
  where go cache = do
          let k = show key
          return $ Map.delete k cache
    
-- ** Delayed cache

-- | pre-cache a value, ie cache it but delayed its computation
-- The value will be purged from the cached after expiring delay.
-- This delay needs to be long enough to leave the time for the value
-- to be computed, as the delay starts when the Delayed is created
-- but not when it's actually started. We assume there, that the
-- start delay is small. When precaching, we delay the start of the computation
-- only to be started once the HTTP query has processed.
preCache :: (Show k, Typeable a) => MVar ExpiryCache -> k -> IO a -> Int -> IO (Delayed a)
preCache cvar key vio delay = do
  d <- createDelayed vio
  _ <- async $ purgeKey cvar (show key)
  expCache cvar key (return d) delay 
  

