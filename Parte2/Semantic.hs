module Semantic where
import System.IO
import ASA
import Lex

-- monada Result encapsula o valor 'a' junto de um Bool e String (mensagem de erro/aviso)
data Result a = Result (Bool, String, a) deriving Show

-- Functor define como alteramos o valor da monada Result usando um fmap
instance Functor Result where
  fmap f (Result (b, s, a)) = Result (b, s, f a)

-- Applicative define como aplicar funções dentro da monada Result e cria novas monadas usando 'pure' que é uma 
-- função auxiliar para botar valor ou função normal em um contexto e daí combiná-las usando <*>
instance Applicative Result where
  pure a = Result (False, "", a)
  Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)   

-- instância Monad de Result que define como encadeamos operações sequenciais com o bind, o que permite manipulações
-- de estado (mensagem de erro/aviso) de forma incremental
instance Monad Result where 
--  return a = Result (False, "", a)
  Result (b, s, a) >>= f = let Result (b', s', a') = f a in Result (b || b', s++s', a')

-- define mensagem de erro como função que retorna um valor (com Bool = True) pra dentro da monada Result
errorMsg s = Result (True, "Erro:"++s++"\n", ())

-- define mensagem de aviso como função que retorna um valor (com Bool = False) pra dentro da monada Result
warningMsg s = Result (False, "Advertencia:"++s++"\n", ())