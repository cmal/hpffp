module Stands where

myAnd :: [Bool] -> Bool

myAnd [] = True
-- myAnd (x:xs) = if x == False then False else myAnd xs
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = (f x) || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp (x:xs) = go comp x xs
  where go comp curr [] = curr
        go comp curr (x:xs)
          | (comp curr x) == GT = go comp curr xs
          | otherwise = go comp x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp (x:xs) = go comp x xs
  where go comp curr [] = curr
        go comp curr (x:xs)
          | (comp curr x) == LT = go comp curr xs
          | otherwise = go comp x xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
