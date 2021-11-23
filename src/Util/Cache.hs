{-# LANGUAGE ViewPatterns, PolyKinds, MagicHash, ScopedTypeVariables, DataKinds #-}
module Util.Cache
( DAction(..)
, Delayed(..)
, getDelayed
, createDelayed 
, cancelDelayed
, startDelayed
, statusDelayed
, DelayedStatus(..)
, CacheDelay
, ExpiryCache
, toCacheMap
, cacheSecond
, cacheHour
, cacheMinute
, cacheDay
, cacheForEver
, preCache
, expCache
, newExpiryCache
, clearExpiryCache
, purgeKey, purgeKey'
, castToDelayed
)
where

import Prelude
import Data.Dynamic
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import UnliftIO
-- import UnliftIO.Concurrent -- (threadDelay)
import Data.Time

import Control.Monad (filterM)
import Unsafe.Coerce(unsafeCoerce)
import Type.Reflection

-- * Delayed 
-- | A syncronous actions, which can be started from another thread.
-- Usefull to get value pre-cached for the next request.

data DAction = DStart | DCancel deriving Show
data Delayed m a = Delayed
  { blocker :: MVar DAction
  , action :: Async (m a)
  , abort :: MVar ()
  } deriving Typeable

data DelayedStatus = Waiting | InProgress | Finished | OnError deriving Show
  
-- | Delay in second. No Num instance as the point
-- is to avoid implicit convertion to second.
newtype CacheDelay = CacheDelay Int
  
deriving instance Typeable Async

createDelayed :: (MonadUnliftIO io) =>  io a -> io (Delayed io a)
createDelayed a = do
   mvar <- newEmptyMVar
   -- don't actually start the action until mvar has been set
   as <- async (do
                   act <- do
                     -- liftIO $ print "WAITING FOR ACTION"
                     act <- readMVar mvar
                     return act
                   case act of
                     DStart -> do
                       -- liftIO $ print "STARTING ACTION"
                       v <- a                
                        --liftIO $ print "ACTION DONE"
                       return v
                     DCancel -> do
                        --liftIO $ print "ACTION CANCELLED"
                       error "delayed cancelled"
                       

               )
   abortm <- newEmptyMVar

   return (Delayed mvar (fmap return as) abortm)

-- | Start the delayed action
startDelayed :: MonadIO io =>  Delayed io a -> io ()
startDelayed d = liftIO $ do
  _ <- tryPutMVar (blocker d) DStart
  return ()

cancelDelayed :: MonadIO io =>  Delayed io2 a -> io ()
cancelDelayed d = liftIO $ do
  -- cancel the async (kill the thread, works it if it is already finished
  cancel (action  d)
  putMVar (abort d) ()
  -- liftIO $ print "KILLING ACTION"
  -- killThread $ asyncThreadId (action d)
  _ <- tryPutMVar (blocker d) DCancel
  return ()


-- | Get the actual value
getDelayed :: (MonadUnliftIO io) => Delayed io a -> io a
getDelayed d = do
  startDelayed d
  r <- wait (action d)
  r

-- | Big Hack to get the blocker from a Dynamic Delayed
--  Without knowing it's actual type (which we don't need)
-- Trying to not use unsafeCoerce and just function from Type.Reflection
-- doesn't seem to work as easily as it should be.
castToDelayed :: (Delayed Maybe () -> r) -> Dynamic -> Maybe r
castToDelayed k (Dynamic typeR v) =
  case splitApps typeR of
    (cons, _) | cons == typeRepTyCon (typeRep :: TypeRep (Delayed Maybe ()))
                                       -> Just $ k (unsafeCoerce v :: Delayed Maybe () )
    _ -> Nothing


-- | Check if the job status
statusDelayed :: (MonadUnliftIO io) => Delayed m a -> io DelayedStatus
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
type ExpiryCache = Maybe (UTCTime, Map String (MVar (Dynamic, UTCTime)))
-- \^ next time a value expires. Used to check if the cache needs purging or not

-- ** Standard cache 
newExpiryCache :: IO (MVar ExpiryCache)
newExpiryCache = newMVar Nothing

clearExpiryCache :: MVar ExpiryCache -> IO ()
clearExpiryCache cvar = do
  -- get the list of key and purge them
  -- this is to kill thread hanging with a timeout
  -- created with preCache
  -- We probably should use WeakRef instead.
  --,Just (_, cache) <- readMVar cvar
  cmap <- toCacheMap <$> readMVar cvar
  let keys = Map.keys cmap
  mapM_ (\k -> {- print ("PURGE", k) >> -} purgeKey' cvar k {- >> print ("XXXXXXXX PURGED", k) -} ) keys
  return ()
  -- swapMVar cvar Nothing >> return ()
-- Compute or use the cached value.
-- There is no way to get a value from the cache without actually
-- specifying the way to compute value. This is to make sure
-- the retrieved value is of the correct type.
expCache :: (Show k, Typeable v, MonadIO io) => Bool -> MVar ExpiryCache -> k -> io v -> CacheDelay -> io v
expCache force cvar key vio (CacheDelay seconds)  = purgeExpired cvar >> do
  now <- liftIO $ getCurrentTime
  let expireAt = addUTCTime (fromIntegral seconds)  now
  let k = show key
  -- we release cvar as soon as possible
  let putV mvar = do
                  v <- vio
                  liftIO $ putMVar mvar (toDyn v, expireAt)
                  return v
  let getCachedMVar cachem = do
        case Map.lookup k =<< (fmap snd cachem) of
          Nothing -> liftIO $ do -- key need creating
            mvar <- newEmptyMVar
            let todo = putV mvar
            return $ ( Just $ case cachem of
                                Nothing -> (expireAt, Map.singleton k mvar)
                                Just (minExp, cache) ->
                                  (min minExp expireAt, Map.insert k mvar cache)
                     , Left todo
                     )

          Just mvar -> do
            return (cachem, Right mvar)
  (todo'mvar) <- liftIO $ modifyMVar cvar getCachedMVar 
  case todo'mvar of
    Left todo -> do
      todo
    Right mvar -> do
      (d, t) <- liftIO $ takeMVar mvar
      if now <= t && not force
        then do
          liftIO $ putMVar mvar (d, t)
          return $ fromDyn d (error "Wrong type for cached key.")
        else do
          putV mvar
-- | Purge from the cache the given key
purgeKey :: (MonadIO io, Show k) => MVar ExpiryCache -> k -> io ()
purgeKey cvar key = purgeKey' cvar (show key)
purgeKey' :: (MonadIO io) => MVar ExpiryCache -> String -> io ()
purgeKey' cvar k = liftIO $ modifyMVar_ cvar go -- liftIO $ print "MODIY" >>  modifyMVar_ cvar go >> print "MODIFIED"
  where go Nothing = return Nothing
        go (Just (_, cache)) = do
          -- check if the object is Delayed
          -- and kill the thread if needs to be
          ---    clear = castToDelayed cancelDelayed >> return ()
          case Map.lookup k cache of
            Just mvar -> do
                -- print ("Lookup", k)
                cached <- timeout (30*1000*1000) $ readMVar mvar
                --          ^ to avoid deadlock wait 30s
                case castToDelayed id . fst  =<< cached of
                  Nothing -> return ()
                  Just delayed -> do
                      -- liftIO $ print ("CANCEL FROM PURGE", k)
                      cancelDelayed delayed >> return ()
            Nothing -> return ()
          cacheFromMap  $ Map.delete k cache
    
-- | Purge from the cache all expired value
purgeExpired :: (MonadIO io) => MVar ExpiryCache -> io ()
purgeExpired cvar = liftIO $ modifyMVar_ cvar go
  where go Nothing = return Nothing
        go cachem@(Just (nextExpiry, cache)) = do
          now <- getCurrentTime
          if now < nextExpiry -- 
            then -- too early
              return cachem
            else do -- there are keys to expires
              let keys'mvar = Map.toList cache
              newKeys'mvar <- filterM (toKeep now) keys'mvar
              cacheFromMap $ Map.fromAscList newKeys'mvar
        toKeep now (_, mvar) = do
           (_ , expireAt) <- readMVar mvar
           return $ expireAt > now

cacheFromMap :: MonadIO io => Map String (MVar (Dynamic, UTCTime)) -> io ExpiryCache
cacheFromMap cache | cache == mempty = return Nothing
cacheFromMap cache = do
  _'exps <- mapM readMVar (Map.elems cache)
  let expireAt = minimum (map snd _'exps)
  return $  Just (expireAt, cache)

-- | Convert an ExpiryCache to a Map
toCacheMap :: ExpiryCache -> Map String (MVar (Dynamic, UTCTime))
toCacheMap Nothing = mempty
toCacheMap (Just (_, cache)) = cache
  
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
preCache :: (MonadUnliftIO io, Show k, Typeable a, Typeable io)
         => Bool -> MVar ExpiryCache -> k -> io a -> CacheDelay -> io (Delayed io a)
preCache force cvar key vio (CacheDelay delay) = do
  d <- createDelayed vio
  let ab = abort d
  -- \^ we don't need to keep a reference to d in the following thread
  _ <- async $ do
    abortedm <- timeout ((delay+1)*1000000) $ readMVar ab
    case abortedm of
      Nothing -> do -- timeout
        -- liftIO $ print ("TIMED OUT" , key)
        purgeKey cvar key
      Just () -> do -- cancelled
        -- liftIO $ print ("ABORTED" , key)
        return ()
  expCache force cvar key (return d) (CacheDelay delay) 
