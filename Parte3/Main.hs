module Main where

import Token
import System.IO
import qualified Lex as L
import Parser as P
import ASA
import Semantic 
import Translator
import Control.Monad.State

main :: IO ()
main = do 
 input <- openFile "teste.j--" ReadMode
 contents <- hGetContents input

 output <- openFile "output.j--" WriteMode
 hSetEncoding output utf8  

 bytecode <- openFile "bytecode.txt" WriteMode
 hSetEncoding bytecode utf8

 let tokens = L.alexScanTokens contents

 -- ANÁLISE LÉXICA
 putStrLn "Análise léxica: "
 print tokens
 putStrLn "\n"

 hPutStrLn output (show tokens)
 hPutStrLn output "\n"

 -- ANÁLISE SINTÁTICA
 let asa = P.calc tokens
 putStrLn "Análise sintática: "
 print asa
 putStrLn "\n"

 hPutStrLn output (show asa)  
 hPutStrLn output "\n"

 -- ANÁLISE SEMÂNTICA
 let Result (houveErro, mensagens, resultado) = analisaPrograma asa
 let status = if houveErro then "[ERRO]" else "[OK]"
 
 putStrLn $ "Análise semântica: " ++ status
 putStrLn mensagens
 print resultado
 putStrLn ""

 hPutStrLn output $ status
 hPutStrLn output mensagens
 hPutStrLn output $ show resultado

 -- CÓDIGO DE MÁQUINA
 let (res, i) = runState (genProg "Prog" resultado) 0

hPutStrLn bytecode res

hClose input 
hClose output
hClose bytecode
