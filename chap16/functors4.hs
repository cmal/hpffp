data FixMePls a =
  FixMe
  | Pls a
  deriving (Eq, Show)


-- WRONG! the first argument to Functor
-- should have kind * -> *, here
-- (FixMePls a) has kind *.

instance Functor (FixMePls a) where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)
