module ZipListAppl where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  -- Cons f Nil <*> l = fmap f l
  -- Cons f fs <*> l = append (fmap f l) (fs <*> l)
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  -- ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' (Cons (f x) (append (fmap f xs) (fs <*> xs)))
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' (Cons (f x) (fs <*> xs))

repeat' :: Int -> List Int
repeat' x = (Cons x (repeat' x))

-- main :: IO ()
-- main = do
   -- let add9 = (+9)
   -- let mult2 = (*2)
   -- let add8 = (+8)
   -- let z = ZipList' (Cons add9 (Cons mult2 (Cons add8 Nil)))
   -- let z' = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))
   -- let z'' = ZipList' (repeat' 1)
