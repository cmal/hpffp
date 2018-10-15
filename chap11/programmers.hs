module Programmers where

data OperatingSystem =
  GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows
  deriving Show

data ProgrammingLanguage =
  Haskell | Agda | Idris | PureScript
  deriving Show

data Programmer = Programmer OperatingSystem ProgrammingLanguage
  deriving Show

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]
allProgrammers :: [Programmer]
allProgrammers = foldl (\a b -> a ++ (map (\s -> Programmer b s) allLanguages)) [] allOperatingSystems

