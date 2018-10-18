module Idempotence where

import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

twice f = f . f

fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x):xs

f x = (capitalizeWord x == twice capitalizeWord x) &&
      (capitalizeWord x == fourTimes capitalizeWord x)

prop_cap :: String -> Bool
prop_cap s = f s == True

runQc :: IO ()
runQc = quickCheck prop_cap


ff x = (sort x == twice sort x) &&
       (sort x == fourTimes sort x)

prop_sort :: String -> Bool
prop_sort s = ff s == True

runQc1 :: IO()
runQc1 = quickCheck prop_sort
