module RandomFool where

-- import Test.Hspec
import Test.QuickCheck
import System.Random

data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)


instance Random Fool where
  randomR (Frue, Frue) g = (Frue, g)
  randomR (Fulse, Fulse) g = (Fulse, g)
  -- randomR (x, y) g = do
  --   b <- getStdRandom $ randomR (True, False)
  --   return (if b then x else y, g)
  randomR _ g = random g
  random g = do
    b <- getStdRandom $ randomR (True, False)     -- TODO not right
    return (if b then Frue else Fulse, g)

genFool :: Gen Fool
genFool = choose (Fulse, Frue)

genFool' :: Gen Fool
genFool' = do
  frequency [(2, return Fulse)
            ,(1, return Frue)]

