-- Combinations
-- Remember the vowels and stops chapter exercise in folds? Write the
-- function to generate the possible combinations of three input lists
-- using liftA3 from Control.Applicative.


import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = undefined
