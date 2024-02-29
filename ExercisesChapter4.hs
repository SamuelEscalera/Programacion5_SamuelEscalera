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


