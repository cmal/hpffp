module Eft where

eftChar :: Char -> Char -> [Char]
eftChar x y = [x..y]

eftInt :: Int -> Int -> [Int]
eftInt x y = [x..y]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x == y = [x]
  | x > y = []
  | x == GT = [GT, EQ]
  | y == GT = [LT, GT]
  | otherwise = [LT, GT, EQ]

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x == y = [x]
  | x > y = []
  | otherwise = [False, True]
