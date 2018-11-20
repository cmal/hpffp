import Data.Char

main :: IO ()
main = do
  contents <- getContents
  putStr (map toUpper contents)
