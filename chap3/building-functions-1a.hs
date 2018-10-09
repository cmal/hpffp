-- building-functions-1a.hs
module BuildingFunctions1a where

appendExclamation :: [Char] -> [Char]
appendExclamation x = x ++ "!"

theFourth :: [a] -> [a]
theFourth x = take 1 (drop 4 x)

dropNine :: [a] -> [a]
dropNine x = drop 9 x

theThirdChar :: [a] -> a
theThirdChar x = x !! 2

rvrs :: [a] -> [a]
rvrs x = concat [(drop 9 x), (take 1 (drop 8 x)), (take 2 (drop 6 x)), (take 1 (drop 5 x)), (take 5 x)]

main :: IO ()
main = print (rvrs "Curry is awesome")
