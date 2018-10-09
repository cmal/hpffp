module Print3 where

myGreeting :: String
-- The above line read as: "myGreeting has the type String"
myGreeting = "hello" ++ " world!"
-- Could alse be: "hello" ++ " " ++ "world!"
-- to obtain the same result

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]
