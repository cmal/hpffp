
data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope =  LolNope
  fmap f (Yeppers a) = Yeppers (f a)


-- If it helps, you’re basically writing the following function:
-- applyIfJust :: (a -> b) -> Maybe a -> Maybe b
