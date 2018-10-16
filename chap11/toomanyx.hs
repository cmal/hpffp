{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- the following one needs FlexibleInstances pragma
instance TooMany (Int, String) where
  tooMany (n, _) = n > 33

-- or do this:
newtype AnotherTooMany = AnotherTooMany (Int, String) deriving (Eq, Show, TooMany)

-- instance TooMany (Int, Int) where
--   tooMany (n, m) = (n + m) > 44

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (n, m) = (n + m) > 44

newtype YetAnotherTooMany a =
  YetAnotherTooMany (a, a)
  deriving (Eq, Show, TooMany)
