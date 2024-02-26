module Introduccion where

import Data.Char(ord, chr)

suma :: Int -> Int -> Int
suma a b = a + b

suma' :: Float -> Float -> Float
suma'  a b = a + b

unChar :: Char
unChar = 'A'

tupla :: Char -> (Char, Int)
tupla x = (x, ord x)   

sumaTupla :: (Int, Int, Char) -> Int
sumaTupla (a, b, c) = a + b

sumaLista :: [Int] -> [Int]
sumaLista xs = xs 

esPar :: Int -> Bool
esPar x = even x

filtrarParses :: [Int] -> [Int]
filtrarParses xs = filter esPar xs

mySplit :: Int -> [Int] -> ([Int], [Int])
mySplit n xs = (take n xs, drop n xs) 

myAbs:: Int -> Int
myAbs n = if n >= 0 then n else -n 

validPositiveNumber :: Int -> Int
validPositiveNumber n = if n < 0 then -1 else
                            if n == 0 then 0  else 1
-- Guard
myAbs' :: Int -> Int
myAbs' n | n >= 0 = n
         | otherwise = -n   

validPositiveNumber' :: Int -> Int
validPositiveNumber' n   | n < 0  = -1
                         | n == 0 = 0
                         | otherwise = 1 

-- Pattern matching
negar :: Bool -> Bool
negar True = False
negar False = True 

-- Charlatan fuction
esUnoODos :: Int -> String
esUnoODos 1 = "One"
esUnoODos 2 = "Dos"
esUnoODos _ = "No se que es"

-- Lambda Function
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

myFunctions :: [(Int -> Int -> Int)]
myFunctions = [(+), (-), (*), div, suma]

getFuntions :: Char -> (Int -> Int -> Int)
getFuntions e | e == '+' = myFunctions !! 0
              | e == '-' = myFunctions !! 1
              | e == '*' = myFunctions !! 2
              | e == '/' = myFunctions !! 3
              | otherwise = myFunctions !! 4

--myExp :: Char -> (Int -> Int -> Int)
--myExp e = getFuntions e

-- Carry 
-- Curried Function
-- ((multiTree 1)3)4
multTree :: Int -> (Int -> (Int -> Int))
multTree x y z = x * y * z

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort menor) ++ [x] ++ (qsort mayor)
                where
                    menor = [me | me <- xs, me < x]
                    mayor = [ma | ma <- xs, ma >= x]


esPrimo :: Int -> Bool
esPrimo x  | div x 1 == x && div x x == 1 && div x (x+1) == 1 = True
            | otherwise = False
