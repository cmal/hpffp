module Phone where

-- |1    |2ABC |3DEF  |
-- |4GHI |5JKL |6MNO  |
-- |7PQRS|8TUV |9WXYZ |
-- |*^   |0+_  |#.,   |


-- Create a data structure that captures the phone layout above. The data
-- structure should be able to express enough of how the layout works
-- that you can use it to dictate the behavior of the functions in the
-- following exercises.


-- fill in the rest.
data DaPhone = DaPhone


Convert the following conversations into the keypresses required to
express them. We’re going to suggest types and functions to fill in
order to accomplish the goal, but they’re not obligatory. If you want
to do it differently...you do you.

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
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone
  -> String
  -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined

