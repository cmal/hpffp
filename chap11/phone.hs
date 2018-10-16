module Phone where

-- |1    |2ABC |3DEF  |
-- |4GHI |5JKL |6MNO  |
-- |7PQRS|8TUV |9WXYZ |
-- |*^   |0+_  |#.,   |


-- Create a data structure that captures the phone layout above. The
-- data structure should be able to express enough of how the layout
-- works that you can use it to dictate the behavior of the functions
-- in the following exercises.

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Prelude
import qualified Data.Char

-- fill in the rest.
type DaPhone = (Map.Map Digit String)

daPhoneList :: [(Digit, String)]
daPhoneList = [('1',     "1"),
               ('2',  "ABC2"),
               ('3',  "DEF3"),
               ('4',  "GHI4"),
               ('5',  "JKL5"),
               ('6',  "MNO6"),
               ('7', "PQRS7"),
               ('8',  "TUV8"),
               ('9', "WXYZ9"),
               ('0',   "0+ "),
               ('*',   "*^"),
               ('#',  "#.,")]

daPhone :: DaPhone
daPhone = Map.fromList daPhoneList


-- Convert the following conversations into the keypresses required to
-- express them. We’re going to suggest types and functions to fill
-- in order to accomplish the goal, but they’re not obligatory. If
-- you want to do it differently...you do you.

convo :: [String]

convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
-- Valid presses: 1 and up

type Digit = Char
type Presses = Int

tapToChar :: Digit -> Presses -> Char
tapToChar digit presses = 
  daPhone Map.! digit !! presses

findPress :: Char -> (Digit, String) -> Maybe (Digit, Presses)
findPress c (d, s) =
  case a of
    Nothing -> Nothing
    otherwise -> do
      let (Just x) = a
      Just (d, succ x) 
  where a = List.elemIndex c s


-- :TODO:

getTap :: Char -> Maybe (Digit, Presses)
getTap c = foldr (\y x -> (if x == Nothing then (findPress (Data.Char.toUpper c) y) else x)) Nothing daPhoneList
  where daPhoneList = Map.toList daPhone


reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps c
  | (o >= 65 && o <= 90) = do
      let (Just tap) = getTap c
      [('*', 1), tap]
--  | (o >= 97 && o <= 122) = do
  | otherwise = do
      let (Just tap) = getTap c
      [tap]
  where daPhoneList = Map.toList daPhone
        o = (Data.Char.ord c)


-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: String -> [(Digit, Presses)]
cellPhonesDead = List.concatMap reverseTaps


-- How many times do digits need to be pressed for each message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = length . (map (\(d, p) -> tapToChar d p))

-- What was the most popular letter for each message? What was its
-- cost? You’ll want to combine reverseTaps and fingerTaps to figure
-- out what it cost in taps. reverseTaps is a list because you need to
-- press a different button in order to get capitals.


mostPopularLetter :: String -> Digit
mostPopularLetter s =
  fst maxVEntry
  where taps = cellPhonesDead s
        mp = foldr (\(d, p) m -> Map.insertWith (+) d p m) Map.empty taps
        lst = Map.toList mp
        maxVEntry = foldr (\y x -> if (snd y) > (snd x) then y else x) (head lst) (tail lst)
-- What was the most popular letter overall?

coolestLtr :: [String] -> Digit
coolestLtr = mostPopularLetter . concat

-- -- What was the most popular word?

coolestWord :: [String] -> String
coolestWord strs =
  fst maxVEntry
  where wds = words $ concat strs
        freqs = foldr (\wd m -> Map.insertWith (+) wd 1 m) Map.empty wds
        lst = Map.toList freqs
        maxVEntry = foldr (\y x -> if (snd y) > (snd x) then y else x) (head lst) (tail lst)
