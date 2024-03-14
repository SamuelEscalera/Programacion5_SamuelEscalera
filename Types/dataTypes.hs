module DataType where

data Nat = Zero
        | Succ Nat
        deriving (Show)

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