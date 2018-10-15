module Cipher where

-- MEET AT DAWN
-- ALLY AL LYAL

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

--- vigenère cipher

vigenère :: String -> String -> String

encodeV :: Char -> Int -> Char
encodeV c shift
  | (ord c > 90) || (ord c < 65) = c
  | otherwise = chr ((mod (x + shift) 26) + ord 'A' - 1)
  where x = ord c - ord 'A' + 1

-- decodeV :: Char -> Int -> Char
-- decodeV c i = undefined

toShift :: Char -> Int
toShift c = ord c - 65 -- assume all codeS are upperCase

vigenère msgS codeS =
  map (\(x, y) -> encodeV x y) $ zip msgS (map toShift codeS)
