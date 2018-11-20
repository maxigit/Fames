{-# LANGUAGE ViewPatterns, PolyKinds, MagicHash, ScopedTypeVariables, DataKinds #-}
module Util.Cache
( DAction(..)
, Delayed(..)
, getDelayed
, startDelayed
, statusDelayed
, DelayedStatus
, CacheDelay
, ExpiryCache
, cacheSecond
, cacheHour
, cacheMinute
, cacheDay
, cacheForEver
, preCache
, expCache
, newExpiryCache
, clearExpiryCache
, purgeKey
, castToDelayed
)
where

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
  
-- | Delay in second. No Num instance as the point
-- is to avoid implicit convertion to second.
newtype CacheDelay = CacheDelay Int
  
deriving instance Typeable Async

createDelayed :: (MonadBaseControl IO io, MonadIO io) =>  io a -> io (Delayed io a)
createDelayed a = do
   mvar <- liftIO newEmptyMVar
   -- don't actually start the action until mvar has been set
   as <- async (do
                   act <- liftIO $ do
                     act <- readMVar mvar
                     return act
                   case act of
                     DStart -> do
                       v <- a
                       return v
                     DCancel -> do
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
expCache :: (Show k, Typeable v, MonadIO io) => Bool -> MVar ExpiryCache -> k -> io v -> CacheDelay -> io v
expCache force cvar key vio (CacheDelay seconds)  = do
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
      if now <= t && not force
        then do
          liftIO $ putMVar mvar (d, t)
          return $ fromDyn d (error "Wrong type for cached key.")
        else do
          putV mvar

purgeKey :: (MonadIO io, Show k) => MVar ExpiryCache -> k -> io ()
purgeKey cvar key = liftIO $ modifyMVar_ cvar go
  where go cache = do
          let k = show key
          return $ Map.delete k cache
    
-- ** Delayed cache

cacheSecond, cacheMinute, cacheHour, cacheDay :: Int -> CacheDelay
cacheSecond second = CacheDelay second
cacheMinute minute = cacheSecond (60*minute)
cacheHour hour = cacheMinute (60*hour)
cacheDay day = cacheHour (24*day-1) -- a bit less so that a refresh at nearly the same time days later works
-- | used for value which normally don't change,
-- user preferences, price lists.
-- If those value change, just reset the cache.
cacheForEver :: CacheDelay
cacheForEver = cacheDay 365
-- | pre-cache a value, ie cache it but delayed its computation
-- The value will be purged from the cached after expiring delay.
-- This delay needs to be long enough to leave the time for the value
-- to be computed, as the delay starts when the Delayed is created
-- but not when it's actually started. We assume there, that the
-- start delay is small. When precaching, we delay the start of the computation
-- only to be started once the HTTP query has processed.
preCache :: (MonadBaseControl IO io, MonadIO io, Show k, Typeable a, Typeable io)
         => Bool -> MVar ExpiryCache -> k -> io a -> CacheDelay -> io (Delayed io a)
preCache force cvar key vio (CacheDelay delay) = do
  d <- createDelayed vio
  _ <- async $ do
    liftIO $ threadDelay ((delay+1)*1000000)
    cancelDelayed d
    purgeKey cvar key
  expCache force cvar key (return d) (CacheDelay delay) 
