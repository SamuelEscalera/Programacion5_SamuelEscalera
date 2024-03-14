module Estudiante where 

data Estudiante = Estudiante {
    nombre :: String,
    segundoNombre :: String,
    apellidoPaterno :: String,
    apellidoMaterno :: String
    } deriving(Show)

estudiantes :: [Estudiante]
estudiantes = [Estudiante "Pedro" " " "Perez" "Velasquez",
               Estudiante "Diego" "Hernan" "Figueroa" "Sevillanos" ]