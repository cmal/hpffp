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
import qualified Data.Char (ord)

-- fill in the rest.
type DaPhone = (Map.Map Digit String)

daPhoneList :: [(Digit, String)]
daPhoneList = [('1',     ""),
               ('2',  "ABC"),
               ('3',  "DEF"),
               ('4',  "GHI"),
               ('5',  "JKI"),
               ('6',  "MNO"),
               ('7', "PQRS"),
               ('8',  "TUV"),
               ('9', "WXYZ"),
               ('0',   "+ "),
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
    otherwise -> Just (d, succ x) where Just x = a
  where a = List.elemIndex c s


-- :TODO:

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone c
  | (o >= 97 && o <= 122) = do
      let (Just tap) = (foldr (\x y -> (if x == Nothing then (findPress c y) else x)) Nothing daPhoneList)
      [tap]
  | (o >= 65 && o <= 90) = do
      let (Just tap) = (foldr (\x y -> (if x == Nothing then (findPress (toUpper c) y) else x)) Nothing daPhoneList)
      [('*', 1), tap]
  where daPhoneList = toList daPhone
        o = (ord c)


-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

-- cellPhonesDead :: DaPhone
--   -> String
--   -> [(Digit, Presses)]
-- cellPhonesDead daPhone s =
--   List.concatMap (reverseTaps daPhone)

-- fingerTaps :: [(Digit, Presses)] -> Presses
-- fingerTaps = map (\(d, p) -> tapToChar d p)

-- -- What was the most popular letter for each message? What was its
-- -- cost? You’ll want to combine reverseTaps and fingerTaps to figure
-- -- out what it cost in taps. reverseTaps is a list because you need to
-- -- press a different button in order to get capitals.

-- mostPopularLetter :: String -> Char
-- mostPopularLetter s =
--   foldr (\m (d, p) -> Map.alterF f d m
--                       let f Nothing -> insert d p m
--                           f (Just (dold, pold)) -> update (++ p) d m)
--   Map.empty s
--   where taps = cellPhonesDead daPhone s

-- -- What was the most popular letter overall?

-- coolestLtr :: [String] -> Char
-- coolestLtr = mostPopularLetter . concat

-- -- What was the most popular word?

-- coolestWord :: [String] -> String
-- coolestWord strs =
--   foldr (\m wd -> Map.alterF f wd m
--                   let f Nothing -> insert wd 1 m
--                       f _ -> update succ wd m)
--   Map.empty wds
--   where wds = words $ concat strs

