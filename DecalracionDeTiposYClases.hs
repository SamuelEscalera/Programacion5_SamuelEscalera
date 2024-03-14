module DecalracionDeTiposYClases where

data Boolean = True | False

type Pos = (Int,Int)

data Move = North | South | East | West | Aleatorio

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)
move Aleatorio (x,y) =(x*x, y*y)

data Shape = Circle Float |
             Rect Float Float 
            deriving(Show)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Rect x y ) =  x * y

data MyMaybe a = NoHay
                 | Existe a 
                 deriving(Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv m n = Just (div m n)

getValue :: Maybe Int -> Int
getValue Nothing = 0
getValue (Just a) = a