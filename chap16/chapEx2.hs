{-# LANGUAGE FlexibleInstances #-}
module ChapEx2 where

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a

-- instance Functor (K a) where
--   fmap _ (K a) = K a

-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))


data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)


-- You thought you'd escaped the goats
-- by now didn't you? Nope.
-- No, it doesn’t do anything interesting. No magic here or in the
-- previous exercise. If it works, you succeeded.


-- Do you need something extra to make the instance work?

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x)


data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)


-- Don’t ask for more typeclass instances than you need. You
-- can let GHC tell you what to do.

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)


data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)



-- You’ll need to use recursion.

data List a =
  Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)



-- AtreeofgoatsformsaGoat-Lord,fearsomepoly-creature.

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  -- A VERITABLE HYDRA OF GOATS

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- You’ll use an extra functor for this one,although your solution
-- might do it monomorphically without using fmap.

-- Keep in mind that you will probably not be able to validate this
-- one in the usual manner. Do your best to make it work.

data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read fun) = Read (fmap f fun)
