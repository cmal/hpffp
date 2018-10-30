module ChapExAppl where

-- | Write applicative instances for the following datatypes.
-- Confused?  Write out what the type should be. Use the checkers
-- library to validate the instances.

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.
data Pair a =
  Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a) where
  (Pair x0 y0) =-= (Pair x1 y1) =
    (x0, y0) `eq` (x1, y1) -- better implementation ?


-- 2. This should look familiar.

-- data Two a b = Two a b

-- instance Functor (Two a) where
--   fmap f (Two x y) = Two x (f y)

-- instance Applicative (Two a) where
--   pure = undefined
--   (<*>) = undefined

-- -- 3.
-- data Three a b c = Three a b c

-- instance Functor (Three a b) where
--   fmap f (Three x y z) = Three x y (f z)

-- instance Applicative (Three a b) where
--   pure = undefined
--   (<*>) = undefined

-- -- 4.
-- data Three' a b = Three' a b b

-- instance Functor (Three' a) where
--   fmap f (Three' x y z) = Three' x (f y) (f z)

-- instance Applicative (Three' a) where
--   pure = undefined
--   (<*>) = undefined

-- -- 5.
-- data Four a b c d = Four a b c d

-- instance Functor (Four a b c) where
--   fmap f (Four a b c d) = Four a b c (f d)

-- instance Applicative (Four a b c) where
--   pure = undefined
--   (<*>) = undefined

-- -- 6.

-- data Four' a b = Four' a a a b

-- instance Functor (Four' a) where
--   fmap f (Four' a b c d) = Four' a b c (f d)

-- instance Applicative (Four' a) where
--   pure = undefined
--   (<*>) = undefined


main :: IO ()
main = do
  quickBatch $ applicative
