{-
[7,0,9,6,1,2,1,3,5,1,1]

 *
 *
 *
*** * ** *
==========
0123456789
-}


{-
spearador = "=========="
histograma :: [Int] -> String
histograma [] = "0"
histograma (x:xs) | x > 9 = "Todos los numero tiene que ser menor a 9"
                  | otherwise = aux (xs) x 0
                  -}

countNumberRepit :: [Int] -> Int -> Int -> Int
countNumberRepit [] _ y = y
countNumberRepit (x:xs) a y = if  a == x 
                                then countNumberRepit (xs) a (y + 1)
                            else countNumberRepit (xs) a y

generalCount :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
generalCount [] _ _ = []
generalCount ((a,b):xs) x y = if x == a
                                then (a, y) :xs
                            else generalCount (xs) x y 