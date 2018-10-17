module Cipher where

-- MEET AT DAWN
-- ALLY AL LYAL

import Data.Char
import System.IO

-- only encode & decode lowercase a-z

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

caesar :: IO String
caesar = do
  s <- getLine
  return (map encode s)

unCaesar :: String -> String
unCaesar = map decode

--- vigenère cipher

encodeV :: Char -> Int -> Char
encodeV c shift
  | (ord c > 90) || (ord c < 65) = c
  | otherwise = chr ((mod (x + shift) 26) + ord 'A' - 1)
  where x = ord c - ord 'A' + 1

-- decodeV :: Char -> Int -> Char
-- decodeV c i = undefined

toShift :: Char -> Int
toShift c = ord c - 65 -- assume all codeS are upperCase

vigenère :: IO String
vigenère = do
  msgS <- getLine
  codeS <- getLine
  return $ map (\(x, y) -> encodeV x y) $ zip msgS (map toShift codeS)
