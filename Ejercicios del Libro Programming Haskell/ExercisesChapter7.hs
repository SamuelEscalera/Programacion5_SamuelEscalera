chop8 :: [a] -> [[a]]
chop8 xs = unfold null (\xs -> let (head8, tail) = splitAt 8 xs in (head8, tail)) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([], xs)
            | otherwise = (take n xs, drop n xs)

map f :: (a -> b) -> [a] -> [b]
map f = unfold null (\xs -> let (x, xs') = head xs in (f x, xs'))

iterate f :: a -> [a]
iterate f x = unfold (== x) (\y -> (y, f y)) x
