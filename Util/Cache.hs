{-# LANGUAGE ViewPatterns, PolyKinds, MagicHash, ScopedTypeVariables, DataKinds #-}
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
import Debug.Trace
import Unsafe.Coerce(unsafeCoerce)
import Data.Proxy
import GHC.Prim (Any, Proxy#)
import Data.Typeable

-- * Delayed
-- | A syncronous actions, which can be started from another thread.
-- Usefull to get value pre-cached for the next request.

data DAction = DStart | DCancel deriving Show
data Delayed m a = Delayed
  { blocker :: MVar DAction
  , action :: Async (StM m a)
  } deriving Typeable

data DelayedStatus = Waiting | InProgress | Finished | OnError deriving Show
  
  
deriving instance Typeable Async

createDelayed :: (MonadBaseControl IO io, MonadIO io) =>  io a -> io (Delayed io a)
createDelayed a = do
   mvar <- liftIO newEmptyMVar
   -- don't actually start the action until mvar has been set
   as <- async (do
                   act <- liftIO $ do
                     -- traceShowM "Reading"
                     act <- readMVar mvar
                     -- traceShowM $ "read done - Starting" ++ show act
                     return act
                   case act of
                     DStart -> do
                       v <- a
                       -- traceShowM "done"
                       return v
                     DCancel -> do
                       -- traceShowM "cancelling"
                       error "delayed cancelled"
                       

               )
   return (Delayed mvar as)

-- | Start the delayed action
startDelayed :: MonadIO io =>  Delayed io a -> io ()
startDelayed d = liftIO $ do
  _ <- tryPutMVar (blocker d) DStart
  return ()

cancelDelayed :: MonadIO io =>  Delayed io a -> io ()
cancelDelayed d = liftIO $ do
  _ <- tryPutMVar (blocker d) DCancel
  return ()


-- | Get the actual value
getDelayed :: (MonadBaseControl IO io, MonadIO io) => Delayed io a -> io a
getDelayed d = do
  startDelayed d
  wait (action d)

-- From Stackoverflow

-- This part reifies a `Typeable' dictionary from a `TypeRep'.
-- This works because `Typeable' is a class with a single field, so 
-- operationally `Typeable a => r' is the same as `(Proxy# a -> TypeRep) -> r'
newtype MagicTypeable r (kp :: KProxy k) =
  MagicTypeable (forall (a :: k) . Typeable a => Proxy a -> r)

withTypeRep :: MagicTypeable r (kp :: KProxy k)
            -> forall a . TypeRep -> Proxy a -> r
withTypeRep d t = unsafeCoerce d ((\_ -> t) :: Proxy# a -> TypeRep)

withTypeable :: forall r . TypeRep -> (forall (a :: k) . Typeable a => Proxy a -> r) -> r
withTypeable t k = withTypeRep (MagicTypeable k) t Proxy

-- The type constructor for Delayed
delayed_tycon = fst $ splitTyConApp $ typeRep (Proxy :: Proxy Delayed)
-- This is needed because Dynamic doesn't export its constructortraceShowId $ , and
-- we need to pattern match on it.
data DYNAMIC = Dynamic TypeRep Any

unsafeViewDynamic :: Dynamic -> DYNAMIC
unsafeViewDynamic = unsafeCoerce

-- The actual implementation, much the same as the one on GHC 8.2, but more 
-- 'unsafe' things
castToDelayed :: Monad m => (forall a . Typeable a => Delayed m a -> r) -> Dynamic -> Maybe r
castToDelayed k (unsafeViewDynamic -> Dynamic t x) =
  case splitTyConApp t of
    (((== delayed_tycon) -> True), [_,a]) -> Just $
      withTypeable a $ \(_ :: Proxy (a :: *)) -> k (unsafeCoerce x :: Delayed m a)
    _ -> Nothing

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
newExpiryCache :: IO (MVar ExpiryCache)
newExpiryCache = newMVar mempty

clearExpiryCache :: MVar ExpiryCache -> IO ()
clearExpiryCache cvar = swapMVar cvar mempty >> return ()
-- Compute or use the cached value.
-- There is no way to get a value from the cache without actually
-- specifying the way to compute value. This is to make sure
-- the retrieved value is of the correct type.
expCache :: (Show k, Typeable v, MonadIO io) => MVar ExpiryCache -> k -> io v -> Int -> io v
expCache cvar key vio seconds  = do
  let k = show key
  -- we release cvar as soon as possible
  let putV mvar = do
                  -- traceShowM $ "starting computation for key " ++ k
                  v <- vio
                  -- traceShowM $ "finishind computation for key " ++ k
                  now <- liftIO $ getCurrentTime
                  -- traceShowM $ "putMVar"
                  liftIO $ putMVar mvar (toDyn v, addUTCTime (fromIntegral seconds) now)
                  return v
  let getCachedMVar cache = do
        case Map.lookup k cache of
          Nothing -> liftIO $ do -- key need creating
            -- traceShowM $ "Create mvar for" ++ k
            mvar <- newEmptyMVar
            let todo = putV mvar
            return (Map.insert k mvar cache, Left todo)
          Just mvar -> do
            return (cache, Right mvar)
  -- traceShowM $ "acquiring cache for" ++ k
  (todo'mvar) <- liftIO $ modifyMVar cvar getCachedMVar 
  -- traceShowM $ "releasing cache for" ++ k
  case todo'mvar of
    Left todo -> do
      -- traceShowM "doing"
      todo
    Right mvar -> do
      (d, t) <- liftIO $ takeMVar mvar
      now <- liftIO $ getCurrentTime
      -- traceShowM ("NOW", now)
      if now <= t
        then do
          -- traceShowM "use cache value"
          liftIO $ putMVar mvar (d, t)
          return $ fromDyn d (error "Wrong type for cached key.")
        else do
          -- traceShowM $ "use cached version for" ++ k
          putV mvar

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
  _ <- async $ do
    liftIO $ threadDelay ((delay+1)*1000000)
    cancelDelayed d
    purgeKey cvar key
  expCache cvar key (return d) delay 
