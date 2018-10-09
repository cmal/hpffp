data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False


data TisAnInteger =
  TisAn Integer

-- instance Eq TisAn where
--   (==)


instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'
  -- 0531 82975824

data TwoInteger =
  Two Integer Integer

instance Eq TwoInteger where
  (==) (Two i j) (Two i' j') = (i == i') && (j == j')


data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = (i == i')
  (==) (TisAString s) (TisAString s') = (s == s')
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a a1) (Pair a' a1') = (a == a') && (a1 == a1')

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data Which a =
  ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (GoodBye b) (GoodBye b') = b == b'
  (==) _ _ = False
