{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

-- Determine if a valid Functor can be written for the datatype
-- provided.

data Bool =
  False | True
-- NO!


data BoolAndSomthingElse a =
  False' a | True' a
-- YES!

instance Functor BoolAndSomthingElse where
  -- fmap f (t x) = t (f x)  -- parse error
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)


data BoolAndMaybeSomthingElse a =
  Falsish | Truish a

instance Functor BoolAndMaybeSomthingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- Use the kinds to guide you on this one, don’t get too hung up on
-- the details.

-- this will not work:
-- newtype Mu f = InF { outF :: f (Mu f) }
-- instance Functor Mu where
--   fmap f (InF { outF = x }) = InF { outF = f x }

-- Again, just follow the kinds and ignore the unfamiliar
-- parts

-- import GHC.Arr

data D =
  D (Array Word Word) Int Int
-- NO!

-- Rearrange the arguments to the type constructor of the datatype so
-- the Functor instance works.

data Sum b a =
  First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
  DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
  L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Keeping in mind that it should result in a Functor that
-- does the following:
-- Prelude> fmap (+1) (L 1 2 3)
-- L 2 2 4
-- Prelude> fmap (+1) (R 1 2 3)
-- R 1 3 3


-- Write Functor instances for the following datatypes.
data Quant a b =
  Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- No, it’s not interesting by itself.
data K a b =
  K a

instance Functor (K a) where
  fmap f (K a) = K a

