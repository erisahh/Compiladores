module Main where

import Token
import System.IO
import qualified Lex as L
import Parser as P

main :: IO ()
main = do 
 input <- openFile "teste.j--" ReadMode
 contents <- hGetContents input

 output <- openFile "output.j--" WriteMode
 hSetEncoding output utf8  

 let tokens = L.alexScanTokens contents

 putStrLn "Análise léxica: "
 print tokens
 putStrLn "\n"

 hPutStrLn output (show tokens)
 hPutStrLn output "\n"

 let asa = P.calc tokens
 putStrLn "Análise sintática: "
 print asa
 putStrLn "\n"

 hPutStrLn output (show asa)  
 hPutStrLn output "\n"

 hClose input 
 hClose output
