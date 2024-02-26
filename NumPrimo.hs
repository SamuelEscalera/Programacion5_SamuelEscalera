module TareaCalcularLosNumerosPrimos where
numPrimos :: Int -> [Int]
numPrimos n = [x | x <- [2..n], esPrimo x]

esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

