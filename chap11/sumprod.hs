module SumProd where


data BigSmall =
  Big Bool
  | Small Bool deriving (Eq, Show)

-- data NumberOrBool =
--   Numba Int8
--   | BoolyBool Bool deriving (Eq, Show)

-- Example use of Numba, parentheses due to
-- syntactic collision between (-) minus and
-- the negate function

-- let myNumba = Numba (-128)



data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)


data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)


-- type TwoQs = (QuantumBool, QuantumBool)


data Person =
  MkPerson String Int
  deriving (Eq, Show)


jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s


-- data Person =
--   Person { name :: String
--          , age :: Int }
--             deriving (Eq, Show)



data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show


type AuthorName = String
-- data Author = Author (AuthorName, BookType)

-- data Author = Fiction AuthorName
--   | Nonfiction AuthorName
--   deriving (Eq, Show)

data Expr =
  Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr


type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)



-- type Expr =
--   Either Number
--   (Either Add
--     (Either Minus
--       (Either Mult Divide)))


data FlowerType = Gardenia
  | Daisy
  | Rose
  | Liliac
  deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show


data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b } deriving (Eq, Show)


newtype NumCow = NumCow Int
  deriving (Eq, Show)

newtype NumPig = NumPig Int
  deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo =
  CowInfo Name Age deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo =
  SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal =
  Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- Alternately
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)
