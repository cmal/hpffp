module Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == (reverse x))


myAbs :: Integer -> Integer
myAbs x = if (x > 0) then x else (- x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = (((snd x), (snd y)), ((fst x), (fst y)))


x = (+)
ff xs = x w 1
  where w = length xs

idd x = x

f3 lst = head lst

f4 tp = fst tp
