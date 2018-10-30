module Validation where

data Validation err a =
  Failure err
  | Success a
  deriving (Eq, Show)

-- validToEither :: Validation e a -> Either e a
-- validToEither (Failure err) = Left err
-- validToEither (Success a) = Right a

-- eitherToValid :: Either e a -> Validation e a
-- eitherToValid (Left err) = Failure err
-- eitherToValid (Right a) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id



-- Prelude> pure 1 :: Either e Int
-- Right 1
-- Prelude> Right (+1) <*> Right 1
-- Right 2
-- Prelude> Right (+1) <*> Left ":("
-- Left ":("
-- Prelude> Left ":(" <*> Right 1
-- Left ":("
-- Prelude> Left ":(" <*> Left "sadface.png"
-- Left ":("



data Errors =
  DividedByZero
  | StackOverFlow
  | MooglesChewedWires
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

-- instance Monoid e => Applicative (Validation e) where
  -- pure = Success
  -- Failure x <*> Failure y = Failure (x <> y)
  -- Failure x <*> Success _ = Failure x
  -- Success _ <*> Failure x = Failure x
  -- Success f <*> Success x = Success (f x)
instance Applicative (Validation e) where
  pure = Success
  Failure x <*> _ = Failure x
  Success _ <*> Failure x = Failure x
  Success f <*> Success x = Success (f x)


-- rules:

--   1. identity:
--   pure id <*> Failure 1 = Failure 1
--   pure id <*> Success 2 = Success 2

--   2. composition:
--   pure (.) <*> Success (+1) <*> Success 1 <*> Failure 2 = Success (+1) <*> (Success 1 <*> Failure 2)

--   3. homomorphism
--   pure f <*> pure x = pure f x
--   pure (+1) <*> pure 1 = pure ((+1) 1)

--   4. interchange
--   Success (+2) <*> pure 2 = pure ($ 2) <*> Success (+2)
  

-- success =
--   Success (+1) <*> Success 1

-- success == Success 2

-- failure =
--   Success (+1) <*> Failure [StackOverflow]

-- failure == Failure [StackOverflow]

-- failure' = Failure [StackOverflow]
--            <*> Success (+1)

-- failure' == Failure [StackOverflow]
  
-- failures =
--   Failure [MooglesChewedWires] <*> Failure [StackOverflow]

-- failures ==
--   Failure [MooglesChewedWires , StackOverflow]
