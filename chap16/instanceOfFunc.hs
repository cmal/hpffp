-- Implement Functor instances for the following datatypes. Use the
-- QuickCheck properties we just showed you to validate them.

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
  -- ?? or
  -- fmap f (Identity x) = Identity x

data Pair a =
  Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)


data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)


data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)


data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' x y z w) = Four' x y z (f w)

-- Can you implement one for this type? Why? Why not?

data Trivial = Trivial

-- NO! The parameter to Functor must be of kind * -> *,
-- but Trivial is of kind *,
-- can not construct a kind * -> * from Trivial


