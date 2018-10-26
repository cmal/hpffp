{-# LANGUAGE FlexibleInstances #-}

module ExerciseSemigroup where

import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Function

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity1 :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity1 x = mappend mempty x == x

monoidRightIdentity1 :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity1 x = mappend x mempty == x


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity1 a =
  Identity1 a
  deriving (Eq, Show)

instance Semigroup Int where
  (<>) = (+)

instance (Semigroup a) => Semigroup (Identity1 a) where
  (Identity1 a1) <> (Identity1 a2) = Identity1 (a1 <> a2)

instance (Monoid a) => Monoid (Identity1 a) where
  mempty = Identity1 mempty
  mappend = (<>)

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

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

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

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)


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

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)


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

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

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

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

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
  (Fst _) <> (Fst y) = Fst y
  (Fst _) <> (Snd y) = Snd y
  (Snd x) <> (Fst _) = Snd x
  (Snd x) <> (Snd _)= Snd x

-- According to the rules above, Or a b can not be a Monoid
-- instance (Monoid a, Monoid b) => Monoid (Or a b) where
--   mempty = (Fst x)
--   mappend = (<>)

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

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine { unCombine = f } <> Combine { unCombine = g } =
    Combine { unCombine = \x -> f x <> g x }

-- instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
--   mempty = Combine mempty
--   mappend = (<>)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine { unCombine = \_ -> mempty }
  mappend = (<>)

-- instance Eq (Combine Int (Sum Int)) where
--   Combine { unCombine = f } == Combine { unCombine = g } =
--     f 0 == g 0 &&
--     g 1 == g 1

-- funoidLeftIdentity1 :: (Monoid b, Eq b) => Fun a b -> a -> Bool
-- funoidLeftIdentity1 (Fn f) x =
--   unCombine (Combine f <> mempty) x == unCombine mempty x

-- funoidRightIdentity1 :: (Monoid b, Eq b) => Fun a b -> a -> Bool
-- funoidRightIdentity1 (Fn f) x =
--   unCombine x (Combine f <> mempty) == unCombine x mempty



monoidLeftIdentityF :: (Eq b, Monoid m) =>
                       (Fun a b -> m)
                    -> (m -> a -> b)
                    -> a
                    -> Fun a b
                    -> Bool
monoidLeftIdentityF wrap eval point candidate =
  eval (mappend mempty m) point == eval m point 
  where m = wrap candidate

monoidRightIdentityF :: (Eq b, Monoid m) =>
                        (Fun a b -> m)
                     -> (m -> a -> b)
                     -> a
                     -> Fun a b
                     -> Bool
monoidRightIdentityF wrap eval point candidate =
  eval (mappend m mempty) point == eval m point 
  where m = wrap candidate

-- instance (CoArbitrary b, Arbitrary b) => Arbitrary (Combine a b) where
--   arbitrary = do
--     f <- arbitrary
--     return (Combine { unCombine = f })


-- type CombineIntAssoc = (Combine Int (Sum Int)) -> (Combine Int (Sum Int)) -> (Combine Int (Sum Int)) -> Sum Int -> Bool

type IntAssoc = Int -> Bool

semigroupCombineAssoc :: (Combine Int (Sum Int)) -> (Combine Int (Sum Int)) -> (Combine Int (Sum Int)) -> Int -> Bool
semigroupCombineAssoc a b c d = (unCombine (a <> (b <> c)) $ d) == (unCombine ((a <> b) <> c) $ d)

-- instance (CoArbitrary a) => CoArbitrary (Combine a b) where
--   coarbitrary = coarbitrary

-- What it should do:
--     Prelude> let f = Combine $ \n -> Sum (n + 1)
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

newtype Comp a =
  Comp { unComp :: (a -> a) }

-- Hint: We can do something that seems a little more specific and
-- natural to functions now that the input and output types are the
-- same.

instance Show (Comp a) where
    show f = "Unicorns!!"

instance (Semigroup a) => Semigroup (Comp a) where
  Comp { unComp = f } <> Comp { unComp = g } =
    Comp { unComp = f . g }

newtype Comp1 a =
  Comp1 (a -> a)

instance Show (Comp1 a) where
    show f = "Unicorns!!"

instance (Semigroup a) => Semigroup (Comp1 a) where
  Comp1 f <> Comp1 g =
    Comp1 (f . g)

instance Eq (Comp1 (Sum Int)) where
  (Comp1 f) == (Comp1 g) =
    f 0 == g 0 &&
    f 1 == g 1
    -- f (Sum { getSum = 0 }) == g (Sum { getSum = 0}) &&
    -- f (Sum { getSum = 1 }) == g (Sum { getSum = 1})

instance (Monoid a) => Monoid (Comp1 a) where
  mempty = Comp1 id
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp1 a) where
  arbitrary = do
    f <- arbitrary
    return (Comp1 f)

type CompIntAssoc = (Comp Int) -> (Comp Int) -> (Comp Int) -> Int -> Bool
semigroupCompIntAssoc :: (Semigroup a, Eq a) => (Comp a) -> (Comp a) -> (Comp a) -> a -> Bool
semigroupCompIntAssoc a b c d = (unComp (a <> (b <> c)) $ d) == (unComp ((a <> b) <> c) $ d)

-- instance (CoArbitrary a) => CoArbitrary (Comp a) where
--   coarbitrary = coarbitrary

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)

--   11. -- Look familiar?
data Validation a b =
  Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a =>
  Semigroup (Validation a b) where
  (Failure a) <> (Failure b) = Failure (a <> b)
  f@(Success _) <> _ = f
  _ <> f@(Success _) = f

type ValidationAssoc = (Validation Int String) -> (Validation Int String) -> (Validation Int String) -> Bool


-- semigroupValidationAssoc :: (Validation a b) -> (Validation a b) -> (Validation a b) -> Bool
-- semigroupValidationAssoc a b c = (a <> b) <> c == a <> (b <> c)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Failure x)
              ,(1, return $ Success y)]


-- 12. -- Validation with a Semigroup
--     -- that does something different
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success a)) <> (AccumulateRight (Success b)) = AccumulateRight (Success (a <> b))
  f@(AccumulateRight (Failure _)) <> _ = f
  _ <> f@(AccumulateRight (Failure _)) = f

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [(AccumulateRight (Success x))
             ,(AccumulateRight (Failure y))]

type AccRightAssoc = AccumulateRight Int String
     -> AccumulateRight Int String
     -> AccumulateRight Int String
     -> Bool


-- 13. -- Validation with a Semigroup
--      -- that does something more
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
  (AccumulateBoth x) <> (AccumulateBoth y) = AccumulateBoth $ x <> y

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [(AccumulateBoth (Success x))
             ,(AccumulateBoth (Failure y))]
  
type AccBothAssoc = AccumulateBoth Int String
  -> AccumulateBoth Int String
  -> AccumulateBoth Int String
  -> Bool


newtype Mem s a =
  Mem {
  runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem { runMem = f }) <> (Mem { runMem = g }) =
    Mem { runMem = \s ->
                     let (x, y) = f s
                         (x', y') = g y
                     in (x <> x', y') }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem { runMem = \x -> (mempty, x) }
  mappend = (<>)

main :: IO ()
main = do
  -- or verboseCheck
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity1 :: Trivial -> Bool)
  quickCheck (monoidRightIdentity1 :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity1IntsAssoc)
  quickCheck (monoidLeftIdentity1 :: Identity1 (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity1 :: Identity1 (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: TwoIntStringAssoc)
  quickCheck (monoidLeftIdentity1 :: Two (Sum Int) String -> Bool)
  quickCheck (monoidRightIdentity1 :: Two (Sum Int) String -> Bool)
  quickCheck (semigroupAssoc :: ThreeIntStringIntAssoc)
  quickCheck (monoidLeftIdentity1 :: Three (Sum Int) String (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity1 :: Three (Sum Int) String (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: FourIntStringIntStringAssoc)
  quickCheck (monoidLeftIdentity1 :: Four (Sum Int) String (Sum Int) String -> Bool)
  quickCheck (monoidRightIdentity1 :: Four (Sum Int) String (Sum Int) String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjBoolAssoc)
  quickCheck (monoidLeftIdentity1 :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity1 :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjBoolAssoc)
  quickCheck (monoidLeftIdentity1 :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity1 :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrIntAssoc)
  -- quickCheck (semigroupCombineAssoc :: CombineIntAssoc)
  quickCheck ((semigroupCombineAssoc
               (Combine $ \n -> Sum (n + 1))
               (Combine $ \n -> Sum (n - 1))
               (Combine $ \n -> Sum (n + 8))
              ) :: IntAssoc)
  -- quickCheck (monoidLeftIdentity1 . Combine . apply :: Fun Int (Sum Int) -> Bool)
  -- quickCheck (monoidRightIdentity1 . Combine . apply :: Fun Int (Sum Int) -> Bool)

  -- TODO
  -- quickCheck (funoidLeftIdentity1 apply :: Fun Int (Sum Int) -> Bool)
  -- quickCheck (funoidRightIdentity1 apply :: Fun Int (Sum Int) -> Bool)

  quickCheck (monoidLeftIdentityF
              (Combine . applyFun)
              unCombine :: Int -> Fun Int (Sum Int) -> Bool)
  quickCheck (monoidRightIdentityF
              (Combine . applyFun)
              unCombine :: Int -> Fun Int (Sum Int) -> Bool)

  quickCheck (semigroupCompIntAssoc :: CompIntAssoc)
  quickCheck (monoidLeftIdentity1 :: Comp1 (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity1 :: Comp1 (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccRightAssoc)
  quickCheck (semigroupAssoc :: AccBothAssoc)

  let f' = Mem $ \s -> ("hi", s + 1)
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
