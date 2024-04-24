module Main where

import Scanner(scanner)
import UU.Parsing
import Parser

import System.IO

main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn (show tree)

          let contenido = "<!DOCTYPE html>\n<html>\n<head>\n<title>¡Hola, Mundo!</title>\n</head>\n<body>\n<h1>¡Hola, Mundo!</h1>\n<p>Este es un archivo HTML creado desde Haskell.</p>\n</body>\n</html>"
    writeFile "index.html" contenido
    putStrLn "Archivo index.html creado correctamente."