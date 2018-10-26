data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)


-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (f fa)
-- cannot construct the infinite type: b ~ f b

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)
-- No instance for (Functor f) arising from a use of ‘fmap’

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- fmap (+1) $ Wrap $ Just 1
