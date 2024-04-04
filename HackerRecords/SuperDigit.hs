lastDigit :: Int -> Int
lastDigit x =  (div x 10)

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = (mod n 10) + sumDigits (div n 10)

superDigit :: Int -> Int -> Int -> Int
superDigit n y x
            | n < 10 && y == x= n
            | otherwise = superDigit (sumDigits n)  (y +1) x

contador ::Int -> Int -> Int
contador x y = if x < y 
                    then x + 1
                else x