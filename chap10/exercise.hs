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
verbs = ["eat" "cry" "stand"]

nv :: [String] -> [String] -> [(String, String)]
nv nouns verbs = foldl (\a b -> a ++ (map (\s -> (b, s)) nouns)) [] verbs

nvn :: [String] -> [String] -> [(String, String, String)]
nvn nouns verbs = foldl (\a b -> a ++ (map (\s -> (b, (fst s), (snd s))) (nv nouns verbs))) [] nouns
