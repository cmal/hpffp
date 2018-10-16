module Exercise where

import Data.List (intercalate)


-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

-- >>> replaceThe "the cow loves us" -- "a cow loves us"
replaceThe :: String -> String
replaceThe s = go (words s) ""
  where
    go [] newStr = newStr
    go (wd:wds) newStr = go wds (if newStr == ""
                                 then case (notThe wd) of
                                    Nothing -> "a"
                                    Just value -> value
                                 else newStr ++ " " ++ wd)
    
vowels :: String
vowels = "aeiou" :: String

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow" -- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where
    go [] i = i
    go (wd:[]) i = i
    go (wd:wds) i = go wds (case (notThe wd) of
                               Nothing -> if isVowel (head $ head wds)
                                          then i + 1
                                          else i
                               _ -> i)


-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak" -- 4
countVowels :: String -> Integer
countVowels s = toInteger $ length $ filter id (map isVowel s)

-- Use the Maybe type to write a function that counts the number of
-- vowels in a string and the number of consonants. If the number of
-- vowels exceeds the number of consonants, the function returns
-- Nothing.

newtype Word' =
  Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord s =
  if exceed then Nothing else Just (Word' s)
  where cntV = countVowels s
        cntC = (toInteger $ length s) - cntV
        exceed = cntV > cntC



-- represent natual numbers

-- As natural as any competitive bodybuilder
data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero)) -- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = Just (Succ v) where (Just v) = integerToNat (n - 1)


-- Simple boolean checks for Maybe values.
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False


-- The following is the Maybe catamorphism. You can turn a
-- Maybe value into anything else with this.
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee init _ Nothing = init
mayybee init f (Just v) = f v



-- In case you just want to provide a fallback value.
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1

fromMaybe :: a -> Maybe a -> a
fromMaybe v Nothing = v
fromMaybe _ (Just v) = v


-- Try writing it in terms of the maybe catamorphism

-- Converting between List and Maybe.
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just v) = [v]


-- For when we just want to drop the Nothing values from our list.

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr f [] lst
  where
    f :: Maybe a -> [a] -> [a]
    f Nothing l = l
    f (Just v) l = v:l


-- You’ll see this called “sequence” later.
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe lst = foldr f (Just []) lst
  where
    f :: Maybe a -> Maybe [a] -> Maybe [a]
    f Nothing _ = Nothing
    f _ Nothing = Nothing
    f (Just v) (Just l) = Just (v:l)


-- Small library for Either


-- Write each of the following functions. If more than one possible
-- unique function exists for the type, use common sense to determine
-- what it should do.

-- 1. Try to eventually arrive at a solution that uses foldr, even if
-- earlier versions don’t use foldr.

lefts' :: [Either a b] -> [a]
lefts' lst = foldr f [] lst
  where f :: Either a b -> [a] -> [a]
        f (Left x) l = x:l
        f _ l = l

-- 2. Same as the last one. Use foldr eventually.

rights' :: [Either a b] -> [b]
rights' lst = foldr f [] lst
  where f :: Either a b -> [b] -> [b]
        f (Right x) l = x:l
        f _ l = l

-- Write each of the following functions. If more than one possible
-- unique function exists for the type, use common sense to determine
-- what it should do.

-- 1. Try to eventually arrive at a solution that uses foldr, even if
-- earlier versions don’t use foldr.

lefts :: [Either a b] -> [a]
lefts [] = []
lefts ((Left x):xs) = x:(lefts xs)
lefts (_:xs) = lefts xs

-- 2. Same as the last one. Use foldr eventually.

rights :: [Either a b] -> [b]
rights [] = []
rights ((Right x):xs) = x:(rights xs)
rights (_:xs) = rights xs

-- Most of the functions you just saw are in the Prelude, Data.Maybe,
-- or Data.Either but you should strive to write them yourself without
-- looking at existing implementations. You will deprive yourself if
-- you cheat.

-- Unfolds

-- While the idea of catamorphisms is still relatively fresh in our
-- minds, let’s turn our attention to their dual: anamorphisms. If
-- folds, or catamorphisms, let us break data structures down then
-- unfolds let us build them up. There are, just as with folds, a few
-- different ways to unfold a data structure. We can use them to
-- create finite and infinite data structures alike.

-- iterate is like a very limited unfold that never ends

--  Prelude> :t iterate
--  iterate :: (a -> a) -> a -> [a]
-- because it never ends, we must use
-- take to get a finite list
--  Prelude> take 10 $ iterate (+1) 0
--  [0,1,2,3,4,5,6,7,8,9]
-- unfoldr is more general, the full monty as it were
--  Prelude> :t unfoldr
--  unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- -- Using unfoldr to do the same thing as iterate
--  Prelude> take 10 $ unfoldr (\b -> Just (b, b+1)) 0
--  [0,1,2,3,4,5,6,7,8,9]


-- Why bother?
-- We bother with this for the same reason we abstracted direct recursion into folds, such as with sum, product, and concat.

-- import Data.List
-- mehSum :: Num a => [a] -> a mehSum xs = go 0 xs
-- where go :: Num a => a -> [a] -> a go n [] = n
-- go n (x:xs) = (go (n+x) xs) niceSum :: Num a => [a] -> a
-- niceSum = foldl' (+) 0
-- mehProduct :: Num a => [a] -> a mehProduct xs = go 1 xs
-- where go :: Num a => a -> [a] -> a go n [] = n
-- go n (x:xs) = (go (n*x) xs) niceProduct :: Num a => [a] -> a
-- niceProduct = foldl' (*) 1
-- Remember the redundant structure when we looked at folds?





--   Your eyes may be spouting gouts of blood, but you may also see that this same principle of abstracting out common patterns and giving them names applies as well to unfolds as it does to folds.



-- -- Write your own iterate and unfoldr
-- 1. Write the function myIterate using direct recursion. Com- pare the behavior with the built-in iterate to gauge cor- rectness. Do not look at the source or any examples of iterate so that you are forced to do this yourself.
-- myIterate :: (a -> a) -> a -> [a] myIterate = undefined
-- 2. Write the function myUnfoldr using direct recursion. Com- pare with the built-in unfoldr to check your implementation. Again, don’t look at implementations of unfoldr so
-- that you figure it out yourself.
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] myUnfoldr = undefined
-- 3. Rewrite myIterate into betterIterate using myUnfoldr. A hint — we used unfoldr to produce the same results as iterate earlier. Do this with different functions and see if you can abstract the structure out.


  
--      -- It helps to have the types in front of you
--      -- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- betterIterate :: (a -> a) -> a -> [a] betterIterate f x = myUnfoldr ...?
-- Remember, your betterIterate should have the same re- sults as iterate.
--      Prelude> take 10 $ iterate (+1) 0
--      [0,1,2,3,4,5,6,7,8,9]
--      Prelude> take 10 $ betterIterate (+1) 0
--      [0,1,2,3,4,5,6,7,8,9]




--      Finally something other than a list!
-- Given the BinaryTree from last chapter, complete the following exercises. Here’s that datatype again:
-- data BinaryTree a = Leaf
-- | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
-- 1. Write unfold for BinaryTree.
-- unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
-- unfold = undefined
-- 2. Make a tree builder.
-- Using the unfold function you’ve just made for BinaryTree, write the following function:
-- treeBuild :: Integer -> BinaryTree Integer treeBuild n = undefined



-- You should be producing results that look like the following:
-- Prelude> treeBuild 0
-- Leaf
-- Prelude> treeBuild 1
-- Node Leaf 0 Leaf



-- Prelude> treeBuild 2
-- Node (Node Leaf 1 Leaf)
-- 0
--      (Node Leaf 1 Leaf)
-- Prelude> treeBuild 3
-- Node (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
