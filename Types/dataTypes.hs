module DataType where

data Nat = Zero
        | Succ Nat
        deriving (Show, Eq)

natToNum :: Nat -> Int
natToNum Zero = 0
natToNum (Succ n) = 1 + natToNum n

numToNat :: Int -> Nat
numToNat 0 =  Zero
numToNat n = Succ (numToNat (n-1))

suma :: Nat -> Nat -> Nat
suma n m = numToNat (natToNum n + natToNum m)

suma2 :: Nat -> Nat -> Nat
suma2 Zero n = n
suma2 (Succ m) n = Succ (suma2 m n)

data Tree  a  = Empty
            | Leaf a 
            | Node (Tree a ) a (Tree a)
            deriving(Show)

addNode :: Ord a => Tree a -> a -> Tree a
addNode(Leaf x) y  = if y <= x
                        then Node (Leaf y) x Empty
                        else Node Empty x (Leaf y)
addNode (Node left x right) y 
        | y <= x = Node (addNode left y) x (Leaf x) 
        | otherwise = Node left x (addNode right y)

instance Eq a => Eq (Tree a) where
        (Leaf x) == (Leaf y) = x == y
        (Node left1 val1 right1) == (Node left2 val2 right2) =
                (left1 == left2) && (val1 == val2) && ( right1 == right2)

inOrden :: Tree a -> [a]
inOrden (Leaf x) = [x]
inOrden (Node left x right) = []

postOrden :: Tree a -> [a]
postOrden (Leaf x) = [x]
postOrden (Node left x right) = []

preOrden :: Tree a -> [a]
preOrden (Leaf x) = [x]
preOrden (Node left x right) = []

data List a = Vacio
                | Siguiente a (List a)
                deriving(Show)
myNum :: Nat
myNum = Succ (Succ Zero)

myList :: List String
myList = Siguiente "manzana " (Siguiente "limones" (Vacio))

len :: List a -> Int
len Vacio   = 0
len (Siguiente _ xs) = 1 + len xs

arbol :: Tree Int
arbol = Node (Node (Leaf 3) 5 (Leaf 6)) 7 (Leaf 9)