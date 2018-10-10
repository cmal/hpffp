import Data.Time

import Data.List -- for foldl'

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]

theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


itemToDate :: [UTCTime] -> DatabaseItem -> [UTCTime]
itemToDate dates (DbDate x) = dates ++ [x]
itemToDate dates _ = dates

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldl' itemToDate []

itemToNumber :: [Integer] -> DatabaseItem -> [Integer]
itemToNumber ints (DbNumber x) = ints ++ [x]
itemToNumber ints _ = ints

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldl' itemToNumber []


recent :: UTCTime -> DatabaseItem -> UTCTime
recent date (DbDate x) = case compare date x of
  LT -> x
  otherwise -> date
recent date _ = date

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldl' recent (UTCTime
                            (fromGregorian 1582 10 4)
                            (secondsToDiffTime 34123))

mayBeSum :: Int -> DatabaseItem -> Int
mayBeSum sum (DbNumber x) = sum + (fromIntegral x)
mayBeSum sum _ = sum

sumDb :: [DatabaseItem] -> Int
sumDb = foldl' mayBeSum (fromIntegral 0)

mayBeAvg :: (Integer, Integer) -> DatabaseItem -> (Integer, Integer)
mayBeAvg (sum, cnt) (DbNumber x) = (sum + x, cnt + 1)
mayBeAvg x _ = x

avgDb :: [DatabaseItem] -> Double
avgDb items = (fromIntegral x) / (fromIntegral y)
  where (x, y) = foldl' mayBeAvg (0, 0) items


-- fibs = 1 : scanl (+) 1 fibs

-- scans.1
-- fibsFst20 = 1 : scanl (+) 1 (take 19 fibs)

-- scans.2
-- fibsLT100 = last (takeWhile (\x -> (last x) < 100) (map (\x -> take x fibs) [1..]))

-- scans.3
-- fact = (scanl (*) | [1..]!!)
