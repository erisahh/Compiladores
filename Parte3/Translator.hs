module Translator where

import Control.Monad.State
import Lex
import ASA

novoLabel::State Int String 
novoLabel = do 
    n <- get
    put (n+1)
    return ("l"++show n)

genCab nome = return (".class public " ++ nome ++ 
                      "\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\tinvokenonvirtual java/lang/Object/<init>()V\n\treturn\n.end method\n\n")

genMainCab s l = return (".method public static main([Ljava/lang/String;)V" ++
                         "\n\t.limit stack " ++ show s ++
                         "\n\t.limit locals " ++ show l ++ "\n\n")

-- ATRIBUIÇÕES
genInt i | i <= 5 			 = "iconst_"++show i++"\n"
		 | i > 5 && i <= 128 = "bipush "++show i++"\n"
		 | otherwise         = "ldc "++show i++"\n"

genDouble d = "ldc2_w "++show d++"\n"

genString s = "ldc "++s++"\n"


-- EXPRESSÕES LÓGICAS
genExprL String -> [Var] -> [Funcao] -> String -> String -> ExprL -> String Int State

-- and
genExprL c tab fun v f (And e1 e2) = do 
	l1 <- novoLabel 
    e1' <- genExprL c tab fun l1 f e1
    e2' <- genExprL c tab fun v f e2
    return (e1'++l1++":\n"++e2')

-- or
genExprL c tab fun v f (Or e1 e2) = do
    l1 <- novoLabel
    e1' <- genExprL c tab fun v l1 e1
    e2' <- genExprL c tab fun v f e2
    return (e1'++l1++":\n"++e2')

-- not
genExprL c tab fun v f (Not e1) = do
    e1' <- genExprL c tab fun f v e1
    return (e1')

-- rel
genExprL c tab fun v f (Rel e1) = do
    e1' <- genExprR c tab fun v f e1
    return (e1')

-- EXPRESSÕES RELACIONAIS
genExprR String -> [Var] -> [Funcao] -> String -> String -> ExprR -> String Int State

-- req
genExprR c tab fun v f (Req e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "eq"++"\tgoto "++f++"\n")

-- rdif
genExprR c tab fun v f (Rdif e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "ne"++"\tgoto "++f++"\n")

-- rlt
genExprR c tab fun v f (Rlt e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "lt"++"\tgoto "++f++"\n")

-- rgt
genExprR c tab fun v f (Rgt e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "gt"++"\tgoto "++f++"\n")

-- rle
genExprR c tab fun v f (Rle e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "le"++"\tgoto "++f++"\n")

-- rge
genExprR c tab fun v f (Rge e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "ge"++"\tgoto "++f++"\n")

genRel :: Tipo -> Tipo -> String -> String
genRel t1 t2 v op | t1==TInt    = ("if_icmp"++op++v)
                  | t1==TString = ("if_acmp"++op++v) 
                  | t1==TDouble = ("dcmpg\nif"++op++v)

-- EXPRESSÕES GERAIS
genExpr String -> [Var] -> [Funcao] -> Expr -> State Int String

-- const
genExpr c tab fun (Const (CInt i)) = return (TInt, genInt i)
genExpr c tab fun (Const (CDouble i)) = return (TDouble, genDouble i)
genExpr c tab fun (Const (CString i)) = return (TString, genString i)

-- OPERAÇÕES
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
genExpr c tab fun (Neg e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1 
    (t2, e2') <- genExpr c tab fun e2 
    return (t1, e1' ++ e2' ++ genOp t1 "neg")

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

genOp :: Tipo -> String -> String
genOp t1 op | t1==TInt    = ("i"++op)
            | t1==TDouble = ("d"++op)

-- COMANDOS
genCmd String -> [Var] -> [Funcao] -> Comando -> State Int String

-- if-else
genCmd c tab fun (If e vb fb) = do
	lv <- novoLabel
	lf <- novoLabel
	e' <- genExprL c tab fun lv lf e
	vb' <- genBloco c tab fun vb
	fb' <= genBloco c tab fun fb
	return (e'++lv++":\n"++vb'"\n"++lf++":\n"++fb'++"\n")

-- while
genCmd c tab fun (While e b) = do 
    li <- novoLabel 
    lv <- novoLabel 
    lf <- novoLabel 
    e' <- genExprL c tab fun lv lf e
    b' <- genBloco c tab fun b
    return (li++":\n"++e'++lv++":\n"++b'++"\tgoto "++li++"\n"++lf++":\n")

-- atrib
genCmd c tab fun (Atrib id e) = do
	

-- for
genCmd c tab fun (For c1 el c2 b) = do
		

-- read
-- imp
-- return
-- proc

genBloco :: String -> [Var] -> [Funcao] -> Bloco -> State Int String
genBloco c tab fun [] = ""
genBloco c tab fun (cmd:bloco) do =
	cmd' = genCmg c tab fun cmd
	bloco' = genBloco c tab fun bloco
	return (cmd'++bloco)

gerar nome p = fst $ runState (genProg nome p) 0
