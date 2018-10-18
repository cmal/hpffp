module Random where

import Test.Hspec
import Test.QuickCheck

data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)


genFool :: Gen Fool
genFool = do
  choose (Fulse, Frue)

genFool' :: Gen Fool
genFool' = do
  frequency [(2, return Fulse)
            ,(1, return Frue)]

