module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)
  -- fmap f (CountMe i a) = CountMe i (f a) -- should be this

instance Applicative CountMe where
  pure = CountMe 0
  -- pure = CountMe 1 -- monoid
  
  -- CountMe n f <*> CountMe n' a = CountMe (n + n') (f a) -- part broken
  CountMe n f <*> CountMe _ a = CountMe (n + 1) (f a) -- all broken

instance Monad CountMe where
  return = pure

  CountMe n a >>= f =
    let CountMe _ b = f a
    in CountMe (n + 1) b
  CountMe _ a >>= f = f a -- this is a valid functor and applicative, but not monad

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: CountMe (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
