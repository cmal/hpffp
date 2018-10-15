module Exercise where

import Data.List (isPrefixOf)

stops = "pbtdkg"
vowels = "aeiou"

sv :: String -> String -> [String]
sv stops vowels = foldl (\a b -> a ++ (map (\s -> b:s) (map (\x -> x:"") stops))) [] vowels

svs :: String -> String -> [String]
svs stops vowels = foldl (\a b -> a ++ (map (\s -> b:s)
                                       -- (map (\x -> x ++ "") (sv stops vowels))
                                       (sv stops vowels)
                                       )) [] stops

svsp :: String -> String -> [String]
svsp stops vowels = filter (\s -> isPrefixOf "p" s) (svs stops vowels)


nouns = ["apple", "baby", "tree"]
-- verbs = ["eat"]
verbs = ["eat", "cry", "stand"]

nv :: [String] -> [String] -> [(String, String)]
nv nouns verbs = foldl (\a b -> a ++ (map (\s -> (b, s)) nouns)) [] verbs

nvn :: [String] -> [String] -> [(String, String, String)]
nvn nouns verbs = foldl (\a b -> a ++ (map (\s -> (b, (fst s), (snd s))) (nv nouns verbs))) [] nouns


myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x y -> (y || (e == x))) False

myReverse :: [a] -> [a]
myReverse = foldl (\y x -> x:y) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x):y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []

squish :: [[a]] -> [a]
squish = foldl (\y x -> y ++ x) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldl (\y x -> y ++ f x) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\x y -> if f x y == LT then y else x)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\x y -> if f x y == LT then x else y) 
