module TareaCalcularLosNumerosPrimos where
numPrimos :: Int -> [Int]
numPrimos n = [x | x <- [2..n], esPrimo x]

esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

-- Recursividad
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

insertVal:: Ord a => a -> [a] -> [a]
insertVal x [] = [x]
insertVal x (y:ys) | x <= y = x : y : ys
                   | otherwise = y : insertVal x ys