module TareaCurriedFunctions where
-- EJEMPLO Aare de un triangulo
-- funcion para calcular el área de un triángulo
areaTriangulo :: Float -> Float -> Float
areaTriangulo base altura = (base * altura) / 2

-- funcion currificada que toma la base como primer argumento
areaTrianguloPorBase :: Float -> Float -> Float
areaTrianguloPorBase base = areaTriangulo base

-- funcion para calcular el área de un triángulo equilátero
areaTrianguloEquilatero :: Float -> Float
areaTrianguloEquilatero lado = areaTrianguloPorBase lado (lado * sqrt(3) / 2)

-- EJEMPLO Mayor de 3 numeros
max3 :: Ord a => a -> a -> a -> a
max3 x y z = max (max x y) z

-- EJEMPLO Filter
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f xs = [x | x <- xs, f x]

-- EJEMPLO 2  Funciones
componer :: (b -> c) -> (a -> b) -> a -> c
componer f g x = f (g x)
