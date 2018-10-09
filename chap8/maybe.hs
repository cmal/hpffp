data Maybee a = Nothinge | Juste a

f :: Bool -> Maybee Int
f False = Juste 0
f _ = Nothinge


nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


-- h :: Num a => a
-- h = 1.0

g :: Fractional a => a
g = 1.0

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = div x 10
        d     = mod xLast 10


tensD x = d
  -- where d = snd (divMod (fst (divMod x 10)) 10)
  where d = (snd . (`divMod` 10) . fst . (`divMod` 10)) x


hunsD x = d
  where d = (snd . (`divMod` 10) . fst . (`divMod` 100)) x

cattyConny :: String -> String -> String

cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"

frappe = flippy "haha"


sumUp :: (Eq a, Num a) => a -> a
sumUp n
  | n == 0 = 0
  | otherwise = n + sumUp (n - 1)


mult2 :: (Integral a) => a -> a -> a
mult2 x y = go x y 0
  where go x y res
          | y == 0 = res
          | otherwise = go x (y - 1) (res + x)

data DividedResult =
  Result Integer
  | DividedByZero

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | (num < 0) && (denom < 0) = Result (go (-num) (-denom) 0)
  | num < 0 = Result (go (-num) denom (-1))
  | denom < 0 = Result (go num (-denom) (-1))
  | otherwise = Result (go num denom 0)
  where go n d count
          | n < d = count
          | otherwise = go (n - d) d (count + 1)


mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) (n + 11)
