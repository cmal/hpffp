module AsPattern where

import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) =
  do
    print a
    return t


doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs


contains :: (Eq a) => a -> [a] -> Bool
contains y [] = False
contains y (x:xs) = (y == x) || contains y xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] as = True
isSubsequenceOf (x:xs) as = contains x as && isSubsequenceOf xs as


capitalizeWords :: String -> [(String, String)]
capitalizeWords s = [(x, capitalizeWord y), (capitalizeWord x, y)]
  where x:y:[] = words s


-- Prelude> capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x):xs

splitOn :: Char -> String -> [String]
splitOn c s = go c s 0 []
  where
    go c "" 0 lst = lst
    go c (' ':xs) 0 lst = go c xs 0 lst
    go c s i lst =
      if length s == i
      then lst ++ [s]
      else
        case s !! i of
          '.' -> lst ++ [take i1 s] ++ go c (drop i1 s) 0 lst
          otherwise -> go c s i1 lst
        where i1 = succ i

join :: [String] -> String
join = foldr (\x y -> x ++ " " ++ y) ""

capitalizeParagraph :: String -> String
capitalizeParagraph p = join $ map capitalizeWord (splitOn '.' p)


-- Prelude> capitalizeParagraph "blah. woot ha."
-- "Blah. Woot ha."
