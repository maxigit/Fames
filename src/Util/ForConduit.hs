module Util.ForConduit
where
import Import
import qualified Data.Conduit.List as C

-- | Tagged  element of a conduit to be ordered by
newtype OrderedBy k a = OrderedBy a
   deriving (Show, Eq, Ord)
   
   

-- | Element of conduit ready to build a map
data ForMap a b = ForMap a b
   deriving (Show, Eq, Ord)
   
unForMap :: ForMap a b -> (a, b)
unForMap (ForMap a b ) = (a, b)

sameFor :: Eq a => ForMap a b -> ForMap a c -> Bool
sameFor (ForMap x _) (ForMap y _) = x == y

forMapKey :: ForMap a b -> a
forMapKey (ForMap a _) =  a

forMapValue :: ForMap a b -> b
forMapValue (ForMap _ b) = b



-- | Interleave the source to connect with a parallel source controled by the first one.
-- example chunk the 2nd source using element of the first source as size
-- @
--    sourceList [1..5] .| interleave (\i -> do
--                                        xms <- replicateM i await
--                                        return $ catMaybe xms
--                                    )
--                                    sourceList [1..100]
--       
-- @
-- returns
-- [ [ 1]
-- , [ 2,  3]
-- , [ 4,  5,  6]
-- , [ 7,  8,  9, 10]
-- , [11, 12, 13, 14, 15]
-- ]
interleave :: Monad m => (a -> ConduitT b Void m o) -> ConduitT () b m () -> ConduitT a o m ()
interleave f source = void $ C.mapAccumM (\x sealed -> sealed $$++  f x) 
                                     (sealConduitT source)

             
            

joinOnWith :: (Ord k, Monad m) => (a -> k) -> (b -> k) -> (a -> [b] -> c) -> ConduitT () a m () -> ConduitT () b m () -> ConduitT () c m ()
joinOnWith aKey bKey f c1 c2 = c1 .| interleave (go []) c2 where
    go acc e1 = do
          e2m <- await
          case e2m of
               Nothing -> return $ f e1 acc
               Just e2 -> case compare (aKey e1) (bKey e2) of
                           EQ -> go (acc ++ [e2]) e1 -- same key keep accumulating
                           LT -> leftover e2 >> return  (f e1 acc) -- new key terminate
                           GT -> go acc e1 -- new key but before, skip until e2 == e1


        
alignConduit :: (Show k, Ord k, Monad m) => ConduitT () (ForMap k a) m () -> ConduitT () (ForMap k b) m () -> ConduitT () (ForMap k (These a b)) m ()
alignConduit sa sb = joinThese sa sb

-- | Full outer join of two *sorted* sources on `k`.
-- Emits (k, These a b) for every key present on either side.
-- -- ChatGPT
joinThese
  :: (Monad m, Ord k)
  => Source m (ForMap k a)
  -> Source m (ForMap k b)
  -> Source m (ForMap k (These a b))
joinThese sa sb = do
  -- turn each source into a ResumableSource
  (rA, _) <- lift $ sa $$+ return ()
  (rB, _) <- lift $ sb $$+ return ()
  loop rA rB
  where
    loop rA rB = do
      -- $$++ returns (ResumableSource, result), so bind in that order
      (rA', ma) <- lift $ rA $$++ await
      (rB', mb) <- lift $ rB $$++ await
      case (ma, mb) of
        (Nothing, Nothing) -> return ()
        (Just (ForMap ka a), Nothing) ->
          yield (ForMap ka $ This a) >> loop rA' rB'
        (Nothing, Just (ForMap kb b)) ->
          yield (ForMap kb $ That b) >> loop rA' rB'
        (Just (ForMap ka a), Just (ForMap kb b)) ->
          case compare ka kb of
            EQ -> do
              yield (ForMap ka  $ These a b)
              loop rA' rB'
            LT -> do
              -- left key is smaller; re-insert right item back into rB'
              yield (ForMap ka $ This a)
              (rB'', _) <- lift $ rB' $$++ leftover (ForMap kb b)
              loop rA' rB''
            GT -> do
              yield (ForMap kb $  That b)
              (rA'', _) <- lift $ rA' $$++ leftover (ForMap ka a)
              loop rA'' rB'
