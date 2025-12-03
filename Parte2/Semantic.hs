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

-- Analisa semanticamente o programa retornando a AST validada (com erros/warns)
analisaPrograma :: Programa -> Result Programa
analisaPrograma (Prog listaFuncoes corpoFuncoes variaveis bloco) = do
    (listaFuncoes', corpoFuncoes') <- verificaFuncoes listaFuncoes corpoFuncoes
    variaveis' <- verificaVariaveis variaveis
    bloco' <- verificaBloco listaFuncoes Nothing variaveis bloco
    pure (Prog listaFuncoes' corpoFuncoes' variaveis' bloco')

-- VERIFICAÇÃO DE EXPRESSÕES
verificaExpressao :: [Funcao] -> [Var] -> Expr -> Result (Tipo, Expr)
verificaExpressao _ _ (Const (CInt n)) = pure (TInt, Const (CInt n))
verificaExpressao _ _ (Const (CDouble n)) = pure (TDouble, Const (CDouble n))
verificaExpressao _ _ (Const (CString s)) = pure (TString, Const (CString s))

-- CONVERSÕES DE TIPO
-- Conversão int -> double
verificaExpressao funcoes variaveis (IntDouble e1) = do 
    (t, e') <- verificaExpressao funcoes variaveis e1 
    if t == TInt then 
        pure (TDouble, IntDouble e')
    else if t == TDouble then 
        pure (t, e')
    else do
        errorMsg $ "Conversão inválida: esperado TInt ou TDouble, mas recebeu " ++ show t
        return (t, e')

-- Conversão double -> int
verificaExpressao funcoes variaveis (DoubleInt e1) = do 
    (t, e') <- verificaExpressao funcoes variaveis e1 
    if t == TDouble then 
        pure (TInt, DoubleInt e')
    else if t == TInt then 
        pure (t, e')
    else do 
        errorMsg $ "Conversão inválida: esperado TDouble ou TInt, mas recebeu " ++ show t
        return (t, e')

-- Chamadas de função em expressões
verificaExpressao funcoes variaveis (Chamada id es) = do 
    case buscarFuncao id funcoes of 
        Nothing -> do 
            errorMsg $ "Função '" ++ id ++ "' não foi declarada"
            return (TVoid, Chamada id es)
        Just f -> do 
            let (_ :->: (_, tipo)) = f
            es' <- verificaChamadas funcoes variaveis f es
            return (tipo, Chamada id es')

-- Expressão aritmética
verificaExpressao funcoes variaveis (Add e1 e2) = verificaExpressaoArit funcoes variaveis Add e1 e2 
verificaExpressao funcoes variaveis (Sub e1 e2) = verificaExpressaoArit funcoes variaveis Sub e1 e2 
verificaExpressao funcoes variaveis (Mul e1 e2) = verificaExpressaoArit funcoes variaveis Mul e1 e2 
verificaExpressao funcoes variaveis (Div e1 e2) = verificaExpressaoArit funcoes variaveis Div e1 e2 

-- Referência a variáveis
verificaExpressao funcoes variaveis (IdVar id) = do 
    case buscarVar id variaveis of  
        Just tipo -> pure (tipo, IdVar id)
        Nothing -> do
            errorMsg $ "Variável '" ++ id ++ "' não foi declarada"
            return (TVoid, IdVar id)

-- Negação aritmética
verificaExpressao funcoes variaveis (Neg e) = do
    (t, e') <- verificaExpressao funcoes variaveis e
    if t == TString then do
        errorMsg $ "Operador de negação não pode ser aplicado a TString"
        return (TVoid, Neg e')
    else 
        pure (t, Neg e')


-- VERIFICAÇÃO DE EXPRESSÕES RELACIONAIS
verificaExprR :: [Funcao] -> [Var] -> ExprR -> Result ExprR
verificaExprR funcoes vars (Req e1 e2) = verificaExpressaoRel funcoes vars Req e1 e2 
verificaExprR funcoes vars (Rdif e1 e2) = verificaExpressaoRel funcoes vars Rdif e1 e2 
verificaExprR funcoes vars (Rlt e1 e2) = verificaExpressaoRel funcoes vars Rlt e1 e2 
verificaExprR funcoes vars (Rgt e1 e2) = verificaExpressaoRel funcoes vars Rgt e1 e2
verificaExprR funcoes vars (Rle e1 e2) = verificaExpressaoRel funcoes vars Rle e1 e2 
verificaExprR funcoes vars (Rge e1 e2) = verificaExpressaoRel funcoes vars Rge e1A e2 

verificaExpressaoRel :: [Funcao] -> [Var] -> (Expr -> Expr -> ExprR) -> Expr -> Expr -> Result ExprR
verificaExpressaoRel funcoes vars op e1 e2 = do
    (t1, e1') <- verificaExpressao funcoes vars e1
    (t2, e2') <- verificaExpressao funcoes vars e2

    if t1 == TString && t2 /= TString || t1 /= TString && t2 == TString then do 
        errorMsg $ "Comparação entre tipos diferentes: " ++ show t1 ++ " e " ++ show t2
        return (op e1' e2')
    else if t1 == t2 then
        pure (op e1' e2')
    else if t1 == TInt && t2 == TDouble then do
        warningMsg "Conversão implícita TInt -> TDouble no operando esquerdo"
        return (op (IntDouble e1') e2')
    else if t1 == TDouble && t2 == TInt then do
        warningMsg "Conversão implícita TInt -> TDouble no operando direito"
        return (op e1' (IntDouble e2'))
    else do
        errorMsg $ "Tipos incompatíveis na comparação: " ++ show t1 ++ " e " ++ show t2
        return (op e1' e2') 


-- VERIFICAÇÃO DE EXPRESSÕES LÓGICAS
verificaExprL :: [Funcao] -> [Var] -> ExprL -> Result ExprL
verificaExprL funcoes vars (And e1 e2) = verificaExpressaoBool funcoes vars And e1 e2
verificaExprL funcoes vars (Or e1 e2) = verificaExpressaoBool funcoes vars Or e1 e2
verificaExprL funcoes vars (Not e1) = verificaExprL funcoes vars e1
verificaExprL funcoes vars (Rel e1) = pure Rel <*> verificaExprR funcoes vars e1


-- VERIFICAÇÃO DE EXPRESSÕES ARITMÉTICAS
verificaExpressaoArit :: [Funcao] -> [Var] -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Result (Tipo, Expr)
verificaExpressaoArit funcoes vars op e1 e2 = do
    (t1, e1') <- verificaExpressao funcoes vars e1
    (t2, e2') <- verificaExpressao funcoes vars e2 

    if t1 == TString || t2 == TString then do
        errorMsg "Operações aritméticas não podem ser aplicadas a strings"
        return (TString, op e1' e2')
    else if t1 == t2 then
        pure (t1, op e1' e2')
    else if t1 == TInt && t2 == TDouble then do
        warningMsg $ "Conversão implícita TInt -> TDouble aplicada ao operando esquerdo"
        return (TDouble, op (IntDouble e1') e2')
    else if t1 == TDouble && t2 == TInt then do
        warningMsg $ "Conversão implícita TInt -> TDouble aplicada ao operando direito"
        return (TDouble, op e1' (IntDouble e2'))
    else do 
        errorMsg $ "Incompatibilidade de tipos: " ++ show t1 ++ " e " ++ show t2
        return (TVoid, op e1' e2')


-- VERIFICAÇÃO DE EXPRESSÕES BOOLEANAS
verificaExpressaoBool :: [Funcao] -> [Var] -> (ExprL -> ExprL -> ExprL) -> ExprL -> ExprL -> Result ExprL
verificaExpressaoBool funcoes vars op e1 e2 = do 
    e1' <- verificaExprL funcoes vars e1 
    e2' <- verificaExprL funcoes vars e2
    pure (op e1' e2') 


-- FUNÇÕES AUXILIARES DE BUSCA E EXTRAÇÃO
extrairId :: Maybe Funcao -> Id
extrairId (Just (id :->: _)) = id 
extrairId Nothing = "" 

extrairIdFuncao :: Funcao -> Id
extrairIdFuncao (id :->: _) = id 

extrairIdVar :: Var -> Id
extrairIdVar (id :#: _) = id 

buscarVar :: Id -> [Var] -> Maybe Tipo 
buscarVar _ [] = Nothing 
buscarVar id ((id' :#: (tipo, _)) : vs) 
    | id == id' = Just tipo 
    | otherwise = buscarVar id vs

buscarFuncao :: Id -> [Funcao] -> Maybe Funcao 
buscarFuncao _ [] = Nothing 
buscarFuncao id ((id' :->: (vars, tipo)) : fs) 
    | id == id' = Just (id' :->: (vars, tipo))
    | otherwise = buscarFuncao id fs 


-- VERIFICAÇÃO DE VARIÁVEIS
verificaVariaveis :: [Var] -> Result [Var]
verificaVariaveis vars = verificar vars [] []
  where
    verificar [] _ validadas = pure validadas 

    verificar ((id' :#: tipo) : vs) vistos validadas = 
        if id' `elem` vistos then do
            errorMsg $ "Declaração duplicada da variável '" ++ id' ++ "'"
            verificar vs vistos validadas 
        else 
            verificar vs (id' : vistos) ((id' :#: tipo) : validadas)


-- VERIFICAÇÃO DE FUNÇÕES
verificaMultiplasFuncoes :: Id -> [Funcao] -> Result [Funcao]
verificaMultiplasFuncoes _ [] = pure []
verificaMultiplasFuncoes id ((id' :->: (vars, tipo)) : fs) = 
    if id == id' then do 
        rest <- verificaMultiplasFuncoes id fs 
        pure ((id' :->: (vars, tipo)) : rest)
    else 
        verificaMultiplasFuncoes id fs 

verificaFuncao :: [Funcao] -> Funcao -> (Id, [Var], Bloco) -> Result (Funcao, (Id, [Var], Bloco)) 
verificaFuncao funcoes f (id, vars, bloco) = do 
    bloco' <- verificaBloco funcoes (Just f) vars bloco 
    funcoesRepetidas <- verificaMultiplasFuncoes (extrairIdFuncao f) funcoes
    
    if length funcoesRepetidas > 1 then
        errorMsg $ "Declaração duplicada da função '" ++ id ++ "'"
    else
        pure ()
    
    pure (f, (id, vars, bloco'))

verificaFuncoes :: [Funcao] -> [(Id, [Var], Bloco)] -> Result ([Funcao], [(Id, [Var], Bloco)])
verificaFuncoes _ [] = pure ([], [])
verificaFuncoes (f : fs) ((id, vars, bloco) : restante) = do 
    vars' <- verificaVariaveis vars
    (f', bloco') <- verificaFuncao (f : fs) f (id, vars', bloco)
    (fs', bs') <- verificaFuncoes fs restante
    pure (f' : fs, bloco' : bs') 


-- VERIFICAÇÃO DE BLOCOS
verificaBloco :: [Funcao] -> Maybe Funcao -> [Var] -> Bloco -> Result Bloco 
verificaBloco _ _ _ [] = pure [] 
verificaBloco funcoes f vars (comando : bloco) = do
    bloco' <- verificaBloco funcoes f vars bloco 
    comando' <- verificaComando funcoes f vars comando  
    pure (comando' : bloco')


-- VERIFICAÇÃO DE COMANDOS
-- atribuição
verificaComando :: [Funcao] -> Maybe Funcao -> [Var] -> Comando -> Result Comando
verificaComando funcoes _ vars (Atrib id expr) = do
    case buscarVar id vars of
        Nothing -> do 
            errorMsg $ "Atribuição a variável não declarada: '" ++ id ++ "'"
            return (Atrib id expr)
		Just t1 -> do
            (t2, e2') <- verificaExpressao funcoes vars expr  
            
            if t1 == t2 then
                pure (Atrib id e2')
            else if t1 == TDouble && t2 == TInt then
                return (Atrib id (IntDouble e2'))
            else if t1 == TInt && t2 == TDouble then do
                warningMsg $ "Conversão TDouble -> TInt pode causar perda de precisão"
                return (Atrib id (DoubleInt e2'))
            else do
                errorMsg $ "Atribuição incompatível: variável '" ++ id ++ "' é " ++ show t1 ++ ", mas recebeu " ++ show t2
                return (Atrib id expr)

-- print
verificaComando funcoes _ vars (Imp e) = do
    (_, e') <- verificaExpressao funcoes vars e
    pure (Imp e')

-- procedimento
verificaComando funcoes _ vars (Proc id expressoes) = 
    case buscarFuncao id funcoes of
        Just fun -> do 
            expressoes' <- verificaChamadas funcoes vars fun expressoes
            return (Proc id expressoes')
        Nothing -> do  
            errorMsg $ "Chamada a função não declarada: '" ++ id ++ "'"
            return (Proc id expressoes)

-- return
verificaComando funcoes f vars (Ret maybe) = do 
    let tipoRetornoEsperado = case buscarFuncao (extrairId f) funcoes of
            Just (_ :->: (_, tipo)) -> tipo
            Nothing -> TVoid

    case maybe of  
        Just e -> do 
            (tipoRetornado, e') <- verificaExpressao funcoes vars e
            let nomeFuncao = extrairId f

            if tipoRetornoEsperado == tipoRetornado then 
                pure (Ret (Just e'))
            else if tipoRetornoEsperado == TDouble && tipoRetornado == TInt then 
                return (Ret (Just (IntDouble e')))
            else if tipoRetornoEsperado == TInt && tipoRetornado == TDouble then do 
                warningMsg $ "Conversão TDouble -> TInt no retorno: pode haver perda de precisão"
                return (Ret (Just (DoubleInt e')))
            else do 
                errorMsg $ "Função '" ++ nomeFuncao ++ "' espera retorno " ++ show tipoRetornoEsperado ++ ", mas recebeu " ++ show tipoRetornado
                return (Ret (Just e'))

        Nothing -> 
            if tipoRetornoEsperado == TVoid then 
                pure (Ret Nothing)
            else do 
                errorMsg $ "Função '" ++ extrairId f ++ "' deve retornar " ++ show tipoRetornoEsperado
                pure (Ret Nothing)

-- if-else
verificaComando funcoes f vars (If e b1 b2) = do 
    e' <- verificaExprL funcoes vars e 
    b1' <- verificaBloco funcoes f vars b1 
    b2' <- verificaBloco funcoes f vars b2 
    pure (If e' b1' b2')

-- while
verificaComando funcoes f vars (While e b) = do
    e' <- verificaExprL funcoes vars e 
    b' <- verificaBloco funcoes f vars b
    pure (While e' b')

-- for
verificaComando funcoes f vars (For c1 e c2 b) = do
    c1' <- verificaComando funcoes f vars c1
    e' <- verificaExprL funcoes vars e
    c2' <- verificaComando funcoes f vars c2
    b' <- verificaBloco funcoes f vars b
    pure (For c1' e' c2' b')

-- read
verificaComando _ _ vars (Leitura v) = do 
    case buscarVar v vars of
        Just _ -> pure (Leitura v)
        Nothing -> do 
            errorMsg $ "Leitura de variável não declarada: '" ++ v ++ "'"
            return (Leitura v)


-- VERIFICAÇÃO DE CHAMADAS DE FUNÇÃO
verificaNumParametros :: [Var] -> [Expr] -> Int
verificaNumParametros vars exprs = length vars - length exprs

verificaChamadas :: [Funcao] -> [Var] -> Funcao -> [Expr] -> Result [Expr]
verificaChamadas _ _ (_ :->: ([], _)) [] = pure []
verificaChamadas funcoes variaveis f exprs = do 
    let (nomeFuncao :->: (parametros, tipoRetorno)) = f 
    let diff = verificaNumParametros parametros exprs

    if diff < 0 then do 
        errorMsg $ "Função '" ++ nomeFuncao ++ "' espera " ++ show (length parametros) ++ " argumentos, mas recebeu " ++ show (length exprs)
        return exprs 
    else if diff > 0 then do 
        errorMsg $ "Função '" ++ nomeFuncao ++ "' espera " ++ show (length parametros) ++ " argumentos, mas recebeu apenas " ++ show (length exprs)
        return exprs 
    else
        verificaArgumentos funcoes variaveis nomeFuncao tipoRetorno parametros exprs

-- Função auxiliar para verificar cada argumento individualmente
verificaArgumentos :: [Funcao] -> [Var] -> Id -> Tipo -> [Var] -> [Expr] -> Result [Expr]
verificaArgumentos _ _ _ _ [] [] = pure []
verificaArgumentos funcoes variaveis nomeFuncao tipoRetorno ((paramId :#: (tipoEsperado, _)) : restoParams) (expr : restoExprs) = do
    (tipoRecebido, expr') <- verificaExpressao funcoes variaveis expr 
    restoExprs' <- verificaArgumentos funcoes variaveis nomeFuncao tipoRetorno restoParams restoExprs

    if tipoEsperado == tipoRecebido then 
        pure (expr' : restoExprs')
    else if tipoEsperado == TDouble && tipoRecebido == TInt then
        return (IntDouble expr' : restoExprs')
    else if tipoEsperado == TInt && tipoRecebido == TDouble then do 
        warningMsg $ "Conversão TDouble -> TInt no argumento da função '" ++ nomeFuncao ++ "'"
        return (DoubleInt expr' : restoExprs')
    else do 
        errorMsg $ "Argumento de função '" ++ nomeFuncao ++ "' espera " ++ show tipoEsperado ++ ", mas recebeu " ++ show tipoRecebido
        return (expr' : restoExprs')
