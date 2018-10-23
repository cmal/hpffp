module Exercise where

import Test.QuickCheck

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity1 a =
  Identity1 a
  deriving (Eq, Show)

instance Semigroup Int where
  (<>) = (+)

instance (Semigroup a) => Semigroup (Identity1 a) where
  (Identity1 a1) <> (Identity1 a2) = Identity1 (a1 <> a2)

-- let xs = 1 :| [2,3]
-- let ys = 4 :| [5,6]

instance (Arbitrary a) => Arbitrary (Identity1 a) where
  arbitrary = do
    x <- arbitrary
    return (Identity1 x)

type Identity1IntsAssoc =
  (Identity1 Int) -> (Identity1 Int) -> (Identity1 Int) -> Bool

data Two a b =
  Two a b
  deriving (Eq, Show)

-- Hint: Ask for another Semigroup instance.

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

-- instance Semigroup String where
--   (<>) = (++)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

-- type TwoAssoc = (Two a b) -> (Two a b) -> (Two a b) -> Bool
type TwoIntStringAssoc = (Two Int String) -> (Two Int String) -> (Two Int String) -> Bool

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

type ThreeIntStringIntAssoc = (Three Int String Int)
                              -> (Three Int String Int)
                              -> (Three Int String Int)
                              -> Bool

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)


instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
          => Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
          => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourIntStringIntStringAssoc = (Four Int String Int String)
  -> (Four Int String Int String)
  -> (Four Int String Int String)
  -> Bool

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

type BoolConjBoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- What it should do:
-- Prelude> (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- Prelude> (BoolConj True) <> (BoolConj False)
-- BoolConj False


newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

type BoolDisjBoolAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- What it should do:
--      Prelude> (BoolDisj True) <> (BoolDisj True)
--      BoolDisj True
--      Prelude> (BoolDisj True) <> (BoolDisj False)
--      BoolDisj True


data Or a b =
  Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  -- (Or a1 b1) <> (Or a2 b2) = Or (a1 <> a2) (b1 <> b2)
  (Fst _) <> (Fst y) = Fst y
  (Fst _) <> (Snd y) = Snd y
  (Snd x) <> (Fst _) = Snd x
  (Snd x) <> (Snd _)= Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    -- choose (Fst x, Snd y)
    frequency [(1, return $ Fst x)
              ,(1, return $ Snd y)]

type OrIntAssoc = (Or Int String) -> (Or Int String) -> (Or Int String) -> Bool

-- The Semigroup for Or should have the following behavior. We can
-- think of this as having a “sticky” Snd value where it’ll hold
-- onto the first Snd value when and if one is passed as an
-- argument. This is similar to the First' Monoid you
-- wrote earlier.
--      Prelude> Fst 1 <> Snd 2
--      Snd 2
--      Prelude> Fst 1 <> Fst 2
--      Fst 2
--      Prelude> Snd 1 <> Fst 2
--      Snd 1
--      Prelude> Snd 1 <> Snd 2
--      Snd 1

-- 9. Keep in mind you won’t be able to easily test associativity for
-- Combine because it contains functions.

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup (Combine a b) where
  Combine { unCombine = f :: (a -> b) } <> Combine { unCombine = g :: (a -> b) } =
    Combine { unCombine = f <> g :: (a -> b) }

-- What it should do:
--    Prelude> let f = Combine $ \n -> Sum (n + 1)
--     Prelude> let g = Combine $ \n -> Sum (n - 1)
--     Prelude> unCombine (f <> g) $ 0
--     Sum {getSum = 0}
--     Prelude> unCombine (f <> g) $ 1
--     Sum {getSum = 2}
--     Prelude> unCombine (f <> f) $ 1
--     Sum {getSum = 4}
--     Prelude> unCombine (g <> f) $ 1
--     Sum {getSum = 2}

-- Hint: This function will eventually be applied to a single value of
-- type a. But you’ll have multiple functions that can produce a
-- value of type b. How do we combine multiple values so we have a
-- single b? This one will probably be tricky! Remember that the type
-- of the value inside of Combine is that of a function. If you can’t
-- figure out CoArbitrary, don’t worry about QuickChecking this one.

-- 10. newtype Comp a =
-- Comp { unComp :: (a -> a) }
-- Hint: We can do something that seems a little more spe- cific and natural to functions now that the input and out- put types are the same.


--   11. -- Look familiar?
-- data Validation a b = Failure a | Success b deriving (Eq, Show)
-- instance Semigroup a =>
-- Semigroup (Validation a b) where
-- (<>) = undefined
-- 12. -- Validation with a Semigroup
--      -- that does something different
-- newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
-- instance Semigroup b =>
-- Semigroup (AccumulateRight a b) where
-- (<>) = undefined


-- 13. -- Validation with a Semigroup
--      -- that does something more
-- newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)
-- instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
-- (<>) = undefined




-- Monoid exercises
-- Given a datatype, implement the Monoid instance. Add Monoid constraints to type variables where needed. For the datatypes you’ve already implemented Semigroup instances for, you just need to figure out what the identity value is.
-- 1. Again, validate all of your instances with QuickCheck. Example scaffold is provided for the Trivial type.


-- data Trivial = Trivial deriving (Eq, Show)
-- instance Semigroup Trivial where (<>) = undefined
-- instance Monoid Trivial where mempty = undefined
-- mappend = (<>)
--      type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
-- main :: IO () main = do
-- quickCheck (semigroupAssoc :: TrivialAssoc) quickCheck (monoidLeftIdentity1 :: Trivial -> Bool) quickCheck (monoidRightIdentity1 :: Trivial -> Bool)
-- 2. newtype Identity1 a = Identity1 a deriving Show
-- 3. data Two a b = Two a b deriving Show
-- 4. newtype BoolConj = BoolConj Bool
-- What it should do:
-- Prelude> (BoolConj True) `mappend` mempty


--    BoolConj True
--    Prelude> mempty `mappend` (BoolConj False)
--    BoolConj False
-- 5. newtype BoolDisj = BoolDisj Bool
-- What it should do:
--    Prelude> (BoolDisj True) `mappend` mempty
--    BoolDisj True
--    Prelude> mempty `mappend` (BoolDisj False)
--    BoolDisj False
-- 6. newtype Combine a b =
-- Combine { unCombine :: (a -> b) }
-- What it should do:
--    Prelude> let f = Combine $ \n -> Sum (n + 1)
--    Prelude> unCombine (mappend f mempty) $ 1
--    Sum {getSum = 2}
-- 7. Hint: We can do something that seems a little more spe- cific and natural to functions now that the input and out- put types are the same.


--   newtype Comp a =
-- Comp (a -> a)
-- 8. This next exercise will involve doing something that will feel a bit unnatural still and you may find it di cult. If you get it and you haven’t done much FP or Haskell before, get yourself a nice beverage. We’re going to toss you the instance declaration so you don’t churn on a missing Monoid constraint you didn’t know you needed.
-- newtype Mem s a = Mem {
-- runMem :: s -> (a,s) }
-- instance Monoid a => Monoid (Mem s a) where mempty = undefined
-- mappend = undefined
-- Given the following code:


--   f' = Mem $ \s -> ("hi", s + 1)
-- main = do
-- print $ runMem (f' <> mempty) 0
-- print $ runMem (mempty <> f') 0
-- print $ (runMem mempty 0 :: (String, Int)) print $ runMem (f' <> mempty) 0 == runMem f' 0 print $ runMem (mempty <> f') 0 == runMem f' 0
-- A correct Monoid for Mem should, given the above code, get the following output:
--      Prelude> main
--     ("hi",1)
--     ("hi",1)
--     ("",0)
-- True True
-- Make certain your instance has output like the above, this
-- is sanity-checking the Monoid identity laws for you! It’s
-- not a proof and it’s not even as good as quick-checking,
-- but it’ll catch the most common mistakes people make. If you’d like to learn how to generate functions with QuickCheck, not just values, look at CoArbitrary in QuickCheck’s docu- mentation.

-- It’s not a trick and you don’t need a Monoid for 𝑠. Yes, such a Monoid can and does exist. Hint: chain the s values from one function to the other. You’ll want to check the identity laws as a common first attempt will break them.


main :: IO ()
main =
  -- quickCheck (semigroupAssoc :: TrivialAssoc)
  -- quickCheck (semigroupAssoc :: Identity1IntsAssoc)
  -- verboseCheck (semigroupAssoc :: TwoIntStringAssoc)
  -- verboseCheck (semigroupAssoc :: ThreeIntStringIntAssoc)
  -- quickCheck (semigroupAssoc :: FourIntStringIntStringAssoc)
  -- verboseCheck (semigroupAssoc :: BoolConjBoolAssoc)
  -- verboseCheck (semigroupAssoc :: BoolDisjBoolAssoc)
  verboseCheck (semigroupAssoc :: OrIntAssoc)
