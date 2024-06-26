module Main where

import Scanner(scanner)
import UU.Parsing
import Parser
import System.IO
import HTMLGenerator (generateHTMLForSlides)

main :: IO ()
main = do 
    input <- readFile "slide.p5"
    let token = scanner input
    putStrLn (show token) 
    tree <- parseIO pSlides token
    putStrLn (show tree)
    putStrLn "*******************"
    putStrLn "Generating HTML"
    let html = generateHTMLForSlides tree
    writeFile "index.html" html
    putStrLn "¡Archivo escrito con éxito!"   