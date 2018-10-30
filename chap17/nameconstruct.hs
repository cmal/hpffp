data Maybee a =
  Nothingg
  | Justt a
  deriving (Eq, Show)

instance Functor Maybee where
  fmap _ Nothingg = Nothingg
  fmap f (Justt x) = Justt (f x)

instance Applicative Maybee where
  pure = Justt

  Nothingg <*> _ = Nothingg
  _ <*> Nothingg = Nothingg
  Justt f <*> Justt a = Justt (f a)

validateLength :: Int -> String -> Maybee String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothingg
  else Justt s

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

mkName :: String -> Maybee Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybee Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)


