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
        deriving(Eq)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan ('-':'-':'-':xs) l c = Token EndSlide "---" l c : scan xs l (c+3)
scan (x:xs) l c 
    | x == '!' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '#' = Token Keyword [x] l c: scan xs l (c+1)
    | x == '{' = Token OpenBlock [x] l c: scan xs l (c+1)
    | x == '}' = Token EndBlock [x] l c: scan xs l (c+1)
    | x == ';' = scan (dropWhile (/= '\n') xs) (l+1) 1
    | x == ' ' = scan xs l (c+1)
    | x == '\n' = scan xs (l+1) 1
    | isAlphaNum x || x == ' ' = let (str, rest, col') = buildString (x:xs) (c+1)
                                in Token String str l c : scan rest l col'
    | otherwise = Token Error [x] l c: scan xs l (c+1)

buildString :: Input -> Col -> (Value, Input, Col)
buildString [] col = ([], [], col)
buildString (x:xs) col
    | x == '\n' = ([], xs, col)
    | otherwise = let (str, rest, col') = buildString xs (col+1)
                in (x:str, rest, col')
