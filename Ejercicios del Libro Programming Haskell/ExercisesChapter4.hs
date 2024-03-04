module Exercises4 where

exercise5 :: Bool -> Bool -> Bool
exercise5 a b = if a == True 
                    then if b  
                        then True
                    else False
                else False

exercise6 :: Bool -> Bool -> Bool
exercise6 a b = if a == True
                    then b
                else
                    False

exercise7 :: Int -> (Int -> (Int -> Int)) 
exercise7 = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble a = if (a * 2) > 9
                    then (a * 2) - 9
                else a * 2

toDigits :: Integer -> [Integer]
toDigits n
        | n <= 0 = []
        | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
            |  n <= 0 = []
            | otherwise = (mod n 10) : toDigits (div n 10)

dobleEveryOther :: [Integer] -> Bool -> [Integer]
dobleEveryOther [] _ = []
dobleEveryOther (x:xs) a = if a == True 
                                then dobleEveryOther xs False ++ [x*2] 
                            else dobleEveryOther xs True ++ [x]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = if x > 9
                        then sum (toDigits x) + sumDigits xs 
                    else x + sumDigits xs

validate :: Integer -> Bool
validate n = remainder == 0
    where
        digits = toDigitsRev n
        doubled = dobleEveryOther digits True
        totalSum = sumDigits doubled
        remainder = mod totalSum 10

