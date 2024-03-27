module SwapNodes where

data Tree a = Empty
                |Leaf a  
                | Node (Tree a) a (Tree a) 
                deriving (Show)

treeInitialiaze :: Tree Int
treeInitialiaze = Leaf 1

addNode :: Tree Int -> Int -> Int -> Tree Int
addNode (Leaf x) a b
                | a < 0 && b < 0 = Node Empty x Empty
                | a < 0 && b > 0 = Node Empty x (Leaf b)
                | a > 0 && b < 0 = Node (Leaf a) x Empty
                | otherwise = Node (Leaf a) x (Leaf b)
addNode (Node Empty x rightT) a b = Node Empty x (addNode rightT a b)
addNode (Node leftT x Empty) a b = Node (addNode leftT a b) x Empty
addNode (Node (Node Empty x Empty) y righT) a b = Node (Node Empty x Empty) y (addNode righT a b)
addNode (Node leftT y (Node Empty x Empty)) a b = Node (addNode leftT a b) y (Node Empty x Empty)
addNode (Node leftT x rightT) a b = if getMinDepth leftT <= getMinDepth rightT
                                        then Node (addNode leftT a b) x rightT
                                        else Node leftT x (addNode rightT a b)

getDepth :: Tree a -> Int -> Int
getDepth (Leaf _)  x = x
getDepth (Node Empty _ _ ) x = x
getDepth (Node leftTree _ _ ) x = getDepth leftTree (x+1)

depth :: Tree a -> Int
depth x = getDepth x 1

getMinDepth :: Tree a -> Int
getMinDepth (Leaf x) = 0
getMinDepth Empty = 1
getMinDepth (Node leftT x (Node Empty y Empty)) = 1 + getMinDepth leftT
getMinDepth (Node (Node Empty y Empty) x righT) = 1 + getMinDepth righT
getMinDepth (Node Empty x righT) = 1 + getMinDepth righT
getMinDepth (Node leftT x Empty) = 1 + getMinDepth leftT
getMinDepth (Node leftT x rightT)= 1 + getMinDepth rightT

swapNodes :: Tree a -> Int -> Int -> Tree a
swapNodes Empty a b = Empty
swapNodes (Node (Node Empty x Empty) y Empty) a b = Node Empty y (Node Empty x Empty)
swapNodes (Node Empty y (Node Empty z Empty)) a b = Node (Node Empty z Empty) y Empty
swapNodes (Node (Node Empty x Empty) y (Node Empty z Empty)) a b = Node (Node Empty z Empty) y (Node Empty x Empty)
swapNodes (Node leftT x rightT) a b = if b == a then Node (swapNodes rightT a (b+1)) x (swapNodes leftT a (b+1)) else Node (swapNodes leftT a (b+1)) x (swapNodes rightT a (b+1))

inordenTree :: Tree a -> [a]
inordenTree Empty = []
inordenTree (Leaf x) = [x]
inordenTree (Node Empty x rightT) = x:inordenTree rightT
inordenTree (Node leftT x Empty) = inordenTree leftT ++ [x]
inordenTree (Node leftT x rightT) = inordenTree leftT ++ x:inordenTree rightT

secondExample :: Tree Int
secondExample = addNode (addNode (addNode (addNode (addNode treeInitialiaze 2 3) (-1) 4) (-1) 5) (-1) (-1)) (-1) (-1)