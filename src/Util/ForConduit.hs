module Util.ForConduit
where
import ClassyPrelude
import Data.Conduit
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


        
       


