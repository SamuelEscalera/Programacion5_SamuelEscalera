data Tree a = Leaf a
                | Node (Tree a) (Tree a)
            deriving Show

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next x =
    case next x of
        Nothing -> []
        Just (y, r) -> y:unfoldr next r

unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a
unfoldTree next x =
        case next x of
            Left y -> Leaf y
            Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = unfoldr (\x -> Just (x, f x)) a

fExample :: Int -> Int
fExample x = x