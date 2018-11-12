module ChapEx where

data Nope a =
  NopeDotJpg

data PhhhbbtttEither b a =
  Left a
  | Right a


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap = undefined

instance Applicative Identity where
  pure = undefined
  (<*>) = undefined

instance Monad Identity where
  return = pure
  (>>=) = undefined

data List a =
  Nil
  | Cons a (List a)

  
