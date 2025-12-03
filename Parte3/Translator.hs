module Translator where

import Control.Monad.State
import Lex
import ASA

novoLabel :: State Int String 
novoLabel = do 
    n <- get
    put (n+1)
    return ("l" ++ show n)

genCab nome = return (".class public " ++ nome ++ 
                      "\n.super java/lang/Object\n" ++
                      ".field private static scanner Ljava/util/Scanner;\n\n" ++
                      ".method public <init>()V\n\taload_0\n\tinvokenonvirtual java/lang/Object/<init>()V\n\treturn\n.end method\n\n")

genMainCab s l = return (".method public static main([Ljava/lang/String;)V" ++
                         "\n\t.limit stack " ++ show s ++
                         "\n\t.limit locals " ++ show l ++ "\n\n")

genScannerInit nome = "new java/util/Scanner\n" ++
                      "dup\n" ++
                      "getstatic java/lang/System/in Ljava/io/InputStream;\n" ++
                      "invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n" ++
                      "putstatic " ++ nome ++ "/scanner Ljava/util/Scanner;\n"


-- FUNÇÕES AUXILIARES
-- tipos
genTipo :: Tipo -> String
genTipo t | t == TInt    = "I"
          | t == TDouble = "D"
          | t == TString = "Ljava/lang/String;"
          | t == TVoid   = "V"

genInt i | i <= 5             = "iconst_" ++ show i ++ "\n"
         | i > 5 && i <= 128  = "bipush " ++ show i ++ "\n"
         | otherwise          = "ldc " ++ show i ++ "\n"

genDouble d = "ldc2_w " ++ show d ++ "\n"

genString s = "ldc " ++ s ++ "\n"

-- tipos read
genTipoRead :: Tipo -> String
genTipoRead t | t == TInt    = "nextInt()I"
              | t == TDouble = "nextDouble()D"
              | t == TString = "nextLine()Ljava/lang/String;"

-- enumerar vars
enumVars :: [Var] -> Int -> [Var]
enumVars [] val = []
enumVars ((id :#: (t, _)) : vrs) val = ((id:#:(t, val)) : (enumVars vrs (val+1)))

-- gerar bloco
genBloco :: String -> [Var] -> [Funcao] -> Bloco -> State Int String
genBloco c tab fun [] = return ""
genBloco c tab fun (cmd:bloco) = do
    cmd' <- genCmd c tab fun cmd
    bloco' <- genBloco c tab fun bloco
    return (cmd' ++ bloco')

-- limit local (talvez não precise)
getLimitLocal :: [Var] -> Int
getLimitLocal [] = 0;
getLimitLocal (_ :#: (t, _) : vrs) | t == TDouble = 2+getLimitLocal vrs
                                   | otherwise    = 1+getLimitLocal vrs

-- gerar chamada de função
genFuncCall :: [Var] -> State Int String
genFuncCall [] = return ""
genFuncCall (v : vrs) = do
    let (_ :#: (tv, iv)) = v
    let st = genTipo tv
    s' <- genFuncCall vrs
    return (st ++ s')

-- listar expressões
listGenExpr :: String -> [Var] -> [Funcao] -> [Expr] -> State Int String
listGenExpr _ _ _ [] = ""
listGenExpr c tab fun (e : es) = do
    (_, s) <- genExpr c tab fun e
    s' <- listGenExpr c tab fun es
    return (s ++ s')

-- gerar função
genFuncs :: String -> [Funcao] -> [Funcao] -> [(Id, [Var], Bloco)] -> State Int String
genFuncs _ _ [] [] = return ""
genFuncs c ttl (f@(id :->: (par, tipo)) : fs) ((nm, vrs, bloc) : blocs) = do
    let vrs' = enumVars vrs 0
    cal <- genFuncCall par
    let cab = (".method public static " ++ nm ++ "(" ++ cal ++ ")" ++
               genTipo tipo ++ "\n" ++ "\n\t.limit stack " ++ show 15 ++ "\n\t.limit locals " ++
               show (getLimitLocal vrs) ++ "\n\n")

    bloc' <- genBloco c vrs' ttl bloc
    rst <- genFuncs c ttl fs blocs
    return (cab ++ bloc' ++ ".end method\n"++ rst)

-- gerar tipo retorno
genReturn :: Tipo -> String
genReturn t | t == TInt    = "ireturn\n"
            | t == TDouble = "dreturn\n"
            | t == TString = "areturn\n"
            | otherwise    = "return\n"

-- busca variável
searchVar :: Id -> [Var] -> State Int (Tipo, Int)
searchVar _ [] = return (TVoid, 0)
searchVar id ((idv :#: (t, nm)) : vs) | id==idv   = return (t, nm)
                                      | otherwise = searchVar id vs
                        
loadVar :: Id -> [Var] -> State Int (Tipo, String)
loadVar _ [] = return (TVoid, "")
loadVar id ((idv :#: (t, nm)) : vs) | id==idv && t==TInt    = return (t, "iload" ++ show nm ++ "\n")
                                    | id==idv && t==TDouble = return (t, "dload" ++ show nm ++ "\n")
                                    | id==idv && t==TString = return (t, "aload" ++ nm ++ "\n")
                                    | otherwise             = loadVar idv vs

-- store
genVarStore :: Tipo -> Int -> String
genVarStore t nm | t == TInt && 5 < nm                = "istore " ++ show nm ++ "\n"
                 | t == TInt && 0 <= nm && nm <= 5    = "istore_" ++ show nm ++ "\n"
                 | t == TDouble && 5 < nm             = "dstore " ++ show nm ++ "\n"
                 | t == TDouble && 0 <= nm && nm <= 5 = "dstore_" ++ show nm ++ "\n"
                 | t == TString                       = "astore " ++ show nm ++ "\n"
                 | otherwise                          = "Erro -> Assign de tipo inválido\n"

-- busca função
searchFunc :: Id -> [Funcao] -> State Int Funcao
searchFunc _ [] = return ("" :->: ([], TVoid))
searchFunc id (f@(idf :->: (_,_)) : fs) | id==idf   = return f
                                        | otherwise = searchFunc id fs

-- PRINCIPAL
genProg :: String -> Programa -> State Int String
genProg nome (Prog fun funcBlocs vars main) = do
    cab <- genCab nome
    funcs' <- genFuncs nome fun fun funcBlocs
    let vars' = enumVars vars 0
    mainCab <- genMainCab 15 (getLimitLocal vars')
    main' <- genBloco nome vars' fun main
    return (cab ++ funcs' ++ mainCab ++ genScannerInit nome ++ main' ++ "\treturn\n.end method\n")


-- EXPRESSÕES RELACIONAIS
genExprR :: String -> [Var] -> [Funcao] -> String -> String -> ExprR -> State Int String

genRel :: Tipo -> Tipo -> String -> String
genRel t1 t2 v op | t1==TInt    = ("if_icmp" ++ op ++ v)
                  | t1==TString = ("if_acmp" ++ op ++ v) 
                  | t1==TDouble = ("dcmpg\nif" ++ op ++ v)

-- req
genExprR c tab fun v f (Req e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1' ++ e2' ++ genRel t1 t2 v "eq" ++ "\tgoto " ++ f ++ "\n")

-- rdif
genExprR c tab fun v f (Rdif e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1' ++ e2' ++ genRel t1 t2 v "ne" ++ "\tgoto " ++ f ++ "\n")

-- rlt
genExprR c tab fun v f (Rlt e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1' ++ e2' ++ genRel t1 t2 v "lt" ++ "\tgoto " ++ f ++ "\n")

-- rgt
genExprR c tab fun v f (Rgt e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1' ++ e2' ++ genRel t1 t2 v "gt" ++ "\tgoto " ++ f ++ "\n")

-- rle
genExprR c tab fun v f (Rle e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1' ++ e2' ++ genRel t1 t2 v "le" ++ "\tgoto " ++ f ++ "\n")

-- rge
genExprR c tab fun v f (Rge e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1' ++ e2' ++ genRel t1 t2 v "ge" ++ "\tgoto " ++ f ++ "\n")


-- EXPRESSÕES LÓGICAS
genExprL :: String -> [Var] -> [Funcao] -> String -> String -> ExprL -> State Int String

-- and
genExprL c tab fun v f (And e1 e2) = do 
    l1 <- novoLabel 
    e1' <- genExprL c tab fun l1 f e1
    e2' <- genExprL c tab fun v f e2
    return (e1' ++ l1 ++ ":\n" ++ e2')

-- or
genExprL c tab fun v f (Or e1 e2) = do
    l1 <- novoLabel
    e1' <- genExprL c tab fun v l1 e1
    e2' <- genExprL c tab fun v f e2
    return (e1' ++ l1 ++ ":\n" ++ e2')

-- not
genExprL c tab fun v f (Not e1) = do
    e1' <- genExprL c tab fun f v e1
    return (e1')

-- rel
genExprL c tab fun v f (Rel e1) = do
    e1' <- genExprR c tab fun v f e1
    return (e1')


-- OPERAÇÕES
genOp :: Tipo -> String -> String
genOp t1 op | t1==TInt    = ("i" ++ op)
            | t1==TDouble = ("d" ++ op)


-- EXPRESSÕES GERAIS
genExpr :: String -> [Var] -> [Funcao] -> Expr -> State Int String

-- const
genExpr c tab fun (Const (CInt i)) = return (TInt, genInt i)
genExpr c tab fun (Const (CDouble i)) = return (TDouble, genDouble i)
genExpr c tab fun (Const (CString i)) = return (TString, genString i)

-- add
genExpr c tab fun (Add e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "add")

-- sub
genExpr c tab fun (Sub e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "sub")

-- mul
genExpr c tab fun (Mul e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "mul")

-- div
genExpr c tab fun (Div e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "div")

-- neg
genExpr c tab fun (Neg e1) = do 
    (t1, e1') <- genExpr c tab fun e1 
    return (t1, e1' ++ genOp t1 "neg")

-- intdouble
genExpr c tab fun (IntDouble e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "2d")

-- doubleint
genExpr c tab fun (DoubleInt e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "2i")

-- idvar
genExpr c tab fun (IdVar id) = (loadVar id tab)

-- COMANDOS
genCmd :: String -> [Var] -> [Funcao] -> Comando -> State Int String

-- if-else
genCmd c tab fun (If e vb fb) = do
    lv  <- novoLabel
    lf  <- novoLabel
    e'  <- genExprL c tab fun lv lf e
    vb' <- genBloco c tab fun vb
    fb' <- genBloco c tab fun fb
    return (e' ++ lv ++ ":\n" ++ vb'"\n" ++ lf ++ ":\n" ++ fb' ++ "\n")

-- while
genCmd c tab fun (While e b) = do 
    li <- novoLabel 
    lv <- novoLabel 
    lf <- novoLabel 
    e' <- genExprL c tab fun lv lf e
    b' <- genBloco c tab fun b
    return (li ++ ":\n" ++ e' ++ lv ++ ":\n" ++ b' ++ "\tgoto " ++ li ++ "\n" ++ lf ++ ":\n")

-- for
    
-- atrib
genCmd c tab fun (Atrib id e) = do
    (te, s)  <- genExpr c tab fun e
    (tv, nm) <- searchVar id tab
    return (s ++ (genVarStore tv nm))

-- read
genCmd c tab fun (Leitura id) = do
    (tv, nm) <- searchVar id tab
    return ("getstatic " ++ c ++ "/scanner Ljava/util/Scanner;\ninvokevirtual java/util/Scanner/" ++ genTipoRead tv ++ "\n" ++ genVarStore tv nm)
    
-- imp
genCmd c tab fun (Imp e) = do
    (t1, e') <- genExpr c tab fun e
    return ("getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ e' ++ "invokevirtual java/io/PrintStream/println(" ++ genTipo t1 ++ ")V\n")

-- return
genCmd c tab fun (Return e) = do
    case e of
        Just e' -> do
            (t1, e1) <- genExpr c tab fun e
            return (genReturn ++ t1)
        Nothing -> return (genReturn Tvoid)

-- proc
genCmd c tab fun (Proc id es) = do
    (_ :->: (vs, tf)) <- searchFunc id fun
    params <- genFuncCall vs
    es' <- listGenExpr c tab fun es
    let invoke = "invokestatic " ++ id ++ "(" ++ params ++ ")" ++ (genTipo tf) ++ "\n"
    return (es' ++ invoke)    

gerar nome p = fst $ runState (genProg nome p) 0
