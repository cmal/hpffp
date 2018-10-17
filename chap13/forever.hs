module Forever where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"


palindrome2 :: IO ()
palindrome2 = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It'a palinedrome!"
    False -> exitSuccess

palindrome3 :: IO ()
palindrome3 = forever $ do
  line1 <- getLine
  let line2 = filter isAlpha line1
  let line = map toLower line2
  case (line == reverse line) of
    True -> putStrLn "It'a palinedrome!"
    False -> exitSuccess
