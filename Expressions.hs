module Expressions where

data Exp =  Lit Int
            | Add Exp Exp
            | Mul Exp Exp
            deriving (Eq)

instance Show Exp where
    show (Lit n)= show n
    show (Add a b) = par (show a ++ " + " ++ show b)
    show (Mul a b) = par(show a ++ " * " ++ show b)

evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add a b) = evalExp a + evalExp b
evalExp (Mul a b) = evalExp a * evalExp b

par :: String -> String 
par x = "(" ++ x ++ ")"

e0 = Add (Lit 1) (Mul (Lit 2) (Lit 3))
e1 = Mul (Add (Lit 1) (Lit 2)) ( Lit 3)
e2 = Add e0 (Mul (Lit 3) e1)