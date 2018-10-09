module Cipher where

import Data.Char

-- only encode & decode lowercase a-z
caesar :: String -> String

unCaesar :: String -> String

shift = 5 :: Int -- right shift

encode :: Char -> Char
encode c
  | (ord c > 122) || (ord c < 97) = c
  | otherwise = chr ((mod (x + shift) 26) + ord 'a' - 1)
  where x = ord c - ord 'a' + 1

decode :: Char -> Char
decode c
  | (ord c > 122) || (ord c < 97) = c
  | otherwise = chr ((mod (x - shift) 26) + ord 'a' - 1)
  where x = ord c - ord 'a' + 1

caesar = map encode

unCaesar = map decode
