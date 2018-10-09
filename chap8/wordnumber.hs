module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "Zero"
  1 -> "One"
  2 -> "Two"
  3 -> "Three"
  4 -> "Four"
  5 -> "Five"
  6 -> "Six"
  7 -> "Seven"
  8 -> "Eight"
  9 -> "Nine"

digits :: Int -> [Int]
digits n = go n []
  where go num res
          | num < 10 = num:res
          | otherwise = go num1 (num2:res)
          where (num1, num2) = divMod num 10

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))
