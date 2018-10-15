module Example where

data Example0 = Example0 deriving (Eq, Show)
data Example1 = Example1 Int deriving (Eq, Show)
data Example2 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)


data Example = MakeExample deriving Show

data Examplex = MakeExamplex Int deriving Show -- cannot use MakeExample again
