
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)


instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)


-- Your hint for this one is that you’re writing the following function.
-- applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
