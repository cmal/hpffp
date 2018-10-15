module HuttonsRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add exp1 exp2) = eval exp1 + eval exp2


-- Prelude> eval (Add (Lit 1) (Lit 9001))
-- 9002


printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add exp1 exp2) = (printExpr exp1) ++ " + " ++ (printExpr exp2)
  

-- Prelude> printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
-- Prelude> let a1 = Add (Lit 9001) (Lit 1)
-- Prelude> let a2 = Add a1 (Lit 20001)
-- Prelude> let a3 = Add (Lit 1) a2

-- Prelude> printExpr a3
-- "1 + 9001 + 1 + 20001"
