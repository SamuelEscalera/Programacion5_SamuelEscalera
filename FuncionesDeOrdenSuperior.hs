fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myFac :: Num a => [a] -> a
myFac = foldr (*) 1