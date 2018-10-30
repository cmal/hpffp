module EitherMonad where


type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
  NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

-- mkSoftware 0 0
-- mkSoftware (-1) 0
-- mkSoftware 0 (-1)
-- mkSoftware 500 0
-- mkSoftware 501 0
-- mkSoftware 501 501
-- mkSoftware 100 5001
-- mkSoftware 0 500


data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

-- Functor Laws
  -- 1. fmap id = id
  -- 2. fmap (g . f) = fmap g . fmap f

instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  Second _ <*> First y = First y
  Second x <*> Second y = Second (x y)

-- Applicative Laws
  -- 1. identity
  -- 2. composition
  -- 3. homomorphism
  -- 4. interchange


-- instance Monad (Sum a) where
--   return = pure
--   (>>=) = undefined

