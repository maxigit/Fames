module Util.Cache where

import Prelude
import Data.Dynamic
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Data.Time
import Control.Monad.IO.Class
import Control.Monad.Trans.Control(StM, MonadBaseControl)

-- * Delayed
-- | A syncronous actions, which can be started from another thread.
-- Usefull to get value pre-cached for the next request.
data Delayed m a = Delayed
  { blocker :: MVar ()
  , action :: Async (StM m a)
  } deriving Typeable

data DelayedStatus = Waiting | InProgress | Finished | OnError deriving Show
  
  
deriving instance Typeable Async

createDelayed :: (MonadBaseControl IO io, MonadIO io) =>  io a -> io (Delayed io a)
createDelayed a = do
   mvar <- liftIO newEmptyMVar
   -- don't actually start the action until mvar has been set
   as <- async (liftIO (takeMVar mvar) >> a)
   return (Delayed mvar as)

-- | Start the delayed action
startDelayed :: MonadIO io =>  Delayed io a -> io ()
startDelayed d = liftIO $ do
  _ <- tryPutMVar (blocker d) ()
  return ()


-- | Get the actual value
getDelayed :: (MonadBaseControl IO io, MonadIO io) => Delayed io a -> io a
getDelayed d = do
  startDelayed d
  wait (action d)

-- | Check if the job status
statusDelayed :: (MonadBaseControl IO io, MonadIO io) => Delayed io a -> io DelayedStatus
statusDelayed d = liftIO $ do
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
expCache :: (Show k, Typeable v, MonadIO io) => MVar ExpiryCache -> k -> io v -> Int -> io v
expCache cvar key vio seconds  = do
  let k = show key
  -- we release cvar as soon as possible
  let putV mvar = do
                  v <- vio
                  now <- liftIO $ getCurrentTime
                  liftIO $ putMVar mvar (toDyn v, addUTCTime (fromIntegral seconds) now)
                  return v
  let getCachedMVar cache = do
        case Map.lookup k cache of
          Nothing -> liftIO $ do -- key need creating
            mvar <- newEmptyMVar
            let todo = putV mvar
            return (Map.insert k mvar cache, Left todo)
          Just mvar -> do
            return (cache, Right mvar)
  (todo'mvar) <- liftIO $ modifyMVar cvar getCachedMVar 
  case todo'mvar of
    Left todo -> do
      todo
    Right mvar -> do
      (d, t) <- liftIO $ takeMVar mvar
      now <- liftIO $ getCurrentTime
      if now <= t
        then return $ fromDyn d (error "Wrong type for cached key.")
        else putV mvar

purgeKey :: (MonadIO io, Show k) => MVar ExpiryCache -> k -> io ()
purgeKey cvar key = liftIO $ modifyMVar_ cvar go
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
preCache :: (MonadBaseControl IO io, MonadIO io, Show k, Typeable a, Typeable io)
         => MVar ExpiryCache -> k -> io a -> Int -> io (Delayed io a)
preCache cvar key vio delay = do
  d <- createDelayed vio
  _ <- async $ error "add deleay" >> purgeKey cvar (show key)
  expCache cvar key (undefined d) delay 
  

