module Util.Transformable where
import Import.NoFoundation
import Util.ValidField

-- * Transformables 
-- | Sort of convert but can loose information
-- Used to demote valid rows to invalid ones.
class Transformable a b where
  transform :: a -> b

instance Transformable a () where
  transform = const ()

instance  {-# OVERLAPPABLE #-} (a~b) => Transformable a b where
  transform x =  x

instance Applicative f => Transformable a (f a) where
  transform = pure

instance Alternative f => Transformable () (f a) where
  transform = const empty

instance Transformable (ValidField a) a where
  transform v = validValue v
 
instance Transformable a (ValidField a) where
  transform x = Provided x


instance Transformable a b => Transformable [a] [b] where
  transform x = map transform x

instance Transformable b (Either e (Maybe b))  where
  transform x = Right (Just x)

instance Transformable a b => Transformable (Maybe a) (Maybe b) where
  transform x = map transform x
