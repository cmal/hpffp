{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool

functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)
