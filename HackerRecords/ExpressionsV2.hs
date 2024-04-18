import qualified Data.Char as Char
import Data.Function ((&))

type NumberType = Rational

data Token = NumberTypeToken NumberType 
    |OperatorToken Operator 
    |LeftParenthesis 
    |RightParenthesis
    deriving (Show)
    
data Operator = PlusOperator
    |MinusOperator
    |MultiplicationOperator
    |DivisionOperator
    deriving (Show)

    
tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize (x : xs) 
    | Char.isSpace x = tokenize xs
    | Char.isDigit x, (integer, afterNumberType) : _ <- reads (x : xs) = 
        NumberTypeToken (fromIntegral integer) : tokenize afterNumberType
    | otherwise = token : tokenize xs
    where 
    token = case x of
        '+' -> OperatorToken PlusOperator
        '-' -> OperatorToken MinusOperator
        '*' -> OperatorToken MultiplicationOperator
        '/' -> OperatorToken DivisionOperator
        '(' -> LeftParenthesis
        ')' -> RightParenthesis

parseExpression :: [Token] -> (NumberType, [Token])
parseExpression tokens
    | OperatorToken PlusOperator : afterOperator <- afterTerm = (term + expression, afterExpression)
    | OperatorToken MinusOperator : afterOperator <- afterTerm = (term - expression, afterExpression)
    | otherwise = (term, afterTerm)
    where
    (term, afterTerm) = parseTerm tokens
    (expression, afterExpression) = parseExpression $ tail afterTerm

parseTerm :: [Token] -> (NumberType, [Token])
parseTerm tokens
    | OperatorToken MultiplicationOperator : afterOperator <- afterFactor = (factor * term, afterTerm)
    | OperatorToken DivisionOperator : afterOperator <- afterFactor = (factor `divide` term, afterTerm)
    | otherwise = (factor, afterFactor)
    where
    (factor, afterFactor) = parseFactor tokens
    (term, afterTerm) = parseTerm $ tail afterFactor
    a `divide` b = a / b

parseFactor :: [Token] -> (NumberType, [Token])
parseFactor [] = error "Error"
parseFactor (token : afterToken)
    | NumberTypeToken integer <- token = (integer, afterToken)
    | OperatorToken PlusOperator <- token = (factor, afterFactor)
    | OperatorToken MinusOperator <- token = ( - factor, afterFactor)
    | LeftParenthesis <- token, RightParenthesis : afterRightParenthesis <- afterExpression =
        (expression, afterRightParenthesis)
    | otherwise = error $ "Error: " ++ (show token)
    where 
    (factor, afterFactor) = parseFactor afterToken
    (expression, afterExpression) = parseExpression afterToken

main :: IO()
main = do
    text <- getLine 
    let tokens = tokenize text
    let (expression, rest) = parseExpression tokens
    let double = fromRational expression :: Double
    let integer = truncate double :: Integer
    print $ integer `mod` 1000000007