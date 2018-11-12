module ChapEx2 where

import Control.Applicative
import Control.Monad

j :: Monad m => m (m a) -> m a
j = join

-- Prelude> j [[1, 2], [], [3]]
-- [1,2,3]
-- Prelude> j (Just (Just 1))
-- Just 1
-- Prelude> j (Just Nothing)
-- Nothing
-- Prelude> j Nothing
-- Nothing

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = undefined

a :: Monad m => m a -> m (a -> b) -> m b
a = undefined

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = undefined

flipType :: (Monad m) => [m a] -> m [a]
flipType = undefined
