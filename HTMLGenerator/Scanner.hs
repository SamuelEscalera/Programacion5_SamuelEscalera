{-# LANGUAGE UndecidableInstances #-}

module Scanner where
import Data.Char (isAlphaNum)

type Col = Int

type Line = Int

type Value = String

type Input = String

data Token = Token Type Value Line Col

data Type = String
  | OpenBlock
  | EndBlock
  | Keyword
  | EndSlide
  | Error
  deriving (Eq, Ord)

instance Show Token where
  show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
  show String = "String: "
  show OpenBlock = "OpenBlock: "
  show EndBlock = "EndBlock: "
  show Keyword = "Keyword: "
  show Error = "Error: "
  show EndSlide = "EndSlide: "

instance (Eq Type) => (Eq Token) where
  (Token String s1 _ _) == (Token String s2 _ _) = True
  (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
  (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
  (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2 
  (Token Error _ _ _) == (Token Error _ _ _) = True
  (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
  (Token t1 s1 _ _) == (Token t2 s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token t1 s1 _ _) <= (Token t2 s2 _ _) = t1 < t2 || (t1 == t2 && s1 <= s2)


scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan ('-':'-':'-':xs) l c = Token EndSlide "---" l c : scan xs l (c+3)
scan ('#':'#':'#':'#':'#':'#': xs) l c = Token Keyword "######" l c : scan xs l (c+6)
scan ('#':'#':'#':'#':'#': xs) l c = Token Keyword "#####" l c : scan xs l (c+5)
scan ('#':'#':'#':'#': xs) l c = Token Keyword "####" l c : scan xs l (c+4)
scan ('#':'#':'#': xs) l c = Token Keyword "###" l c : scan xs l (c+3)
scan ('#':'#': xs) l c = Token Keyword "##" l c : scan xs l (c+2)
scan ('$':'$': xs) l c = Token Keyword "$$" l c : scan xs l (c+2)
scan (x:xs) l c 
    | x == '!' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '#' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '<' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '>' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '$' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '?' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '[' = Token Keyword [x] l c: scan xs l (c+1)
    | x == ']' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '{' = Token OpenBlock [x] l c: scan xs l (c+1)
    | x == '}' = Token EndBlock [x] l c: scan xs l (c+1)
    | x == ';' = scan (dropWhile (/= '\n') xs) (l+1) 1
    | x == ' ' = scan xs l (c+1)
    | x == '\n' = scan xs (l+1) 1
    | isAlphaNum x || x == ' ' = let (str, rest, col') = buildString (x:xs) (c+1)
                                in Token String str l c : scan rest (l+1) col'
    | otherwise = Token Error [x] l c: scan xs l (c+1)

buildString :: Input -> Col -> (Value, Input, Col)
buildString [] col = ([], [], col)
buildString ('>':xs) col = ([], '>' : xs, col + 1)
buildString (']':xs) col = ([], ']' : xs, col + 1)
buildString (x:xs) col
    | x == '\n' = ([], xs, col)
    | otherwise = let (str, rest, col') = buildString xs (col+1)
                in (x:str, rest, col')