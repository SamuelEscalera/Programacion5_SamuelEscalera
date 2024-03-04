import Data.List (find)

-- Exercise 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]


perfects :: Int -> [Int]
perfects x = [x | x <- [1..x], isPerfect x]

isPerfect :: Int -> Bool
isPerfect x  = if sum (factors x) - x == x
                then True
            else False

-- Exercise 7
generatorX :: [Int]
generatorX = [x | x <- [1,2]]

generatorY :: [Int]
generatorY = [x | x <- [3,4]]

linkXAndY :: [Int]
linkXAndY = concat [generatorX, generatorY]

-- Exercise 8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = findIndices (== x) xs
  where
    findIndices p xs = [i | (x', i) <- zip xs [0..], p x']