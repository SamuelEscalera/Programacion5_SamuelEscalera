module Estudiante where 
import System.Random

data Dificultad = Facil 
                    | Medio
                    | Dificil
                    deriving(Show)

data Ejercicio = Ejercicio {
    dificultad :: Dificultad,
    problema :: String,
    resuelto :: Bool
} deriving(Show)


data Estudiante = Estudiante {
    nombreCompleto :: String,
    apellidos :: String,
    salioElegido :: Bool,
    resolvioEjercicio :: Bool,
    existe :: Bool
    } deriving(Show)

estudiantes :: [Estudiante]
estudiantes = [Estudiante "Samuel" "Escalera Herrera" True True True,
                Estudiante "Diego" "Figueroa Sevillanos" False False True
                ]

ejercicios :: [Ejercicio]
ejercicios = [
    Ejercicio Facil "Suma de dos numeros" False,
    Ejercicio Medio "Factorial de numero" False,
    Ejercicio Facil "Dividir dos numeros" False
    ]

problemas :: [Ejercicio]
problemas = filter (resuelto False)

ejercicioSeleccionado :: []