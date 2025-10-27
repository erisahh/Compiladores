{
module Parser where
import Token
import ASA
import qualified Lex as L
import System.IO
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    -- tipo
    'int' {TINT}
    'double' {TDOUBLE}
    'string' {TSTRING}
    'void' {TVOID}

    -- const
    CINT {CINT $$}
    CDOUBLE {CDOUBLE $$}
    LITERAL {LITERAL $$}

    -- ops
    '+' {ADD}
    '-' {SUB}
    '*' {MUL}
    '/' {DIV}

    '(' {LPAR}
    ')' {RPAR}
    '{' {LBRA}
    '}' {RBRA}

    '&&' {AND}
    '||' {OR}
    '!' {NOT}

    'if' {IF}
    'else' {ELSE}
    'while' {WHILE}
    'for' {FOR}

    '<=' {RLEQ}
    '>=' {RGEQ}
    '<' {RL}
    '>' {RG}
    '==' {REQ}
    '/=' {RNEQ}

    ',' {COMMA}
    ';' {SEMI}
    '=' {ASSIGN}
    'read' {READ}
    'print' {PRINT}
    'return' {RETURN}
    ID {ID $$}
%%

Programa : ListaFuncoes BlocoPrincipal {Prog (map fst $1) (map retornaFuncaoCompleta $1) (fst $2) (snd $2)}
         | BlocoPrincipal {Prog [] [] (fst $1) (snd $1)}

ListaFuncoes : ListaFuncoes Funcao {$1 ++ [$2]}
             | Funcao {[$1]}

Funcao : TipoRetorno ID '(' DeclParametros ')' BlocoPrincipal {($2:->:($4,$1), $6)}  
       | TipoRetorno ID '(' ')' BlocoPrincipal {($2:->:([],$1), $5)}   

TipoRetorno : 'void' {TVoid}
            | Tipo {$1}

DeclParametros : DeclParametros ',' Parametro {$1 ++ [$3]}
               | Parametro {[$1]}

Parametro : Tipo ID {$2:#:($1, 0)} 

BlocoPrincipal : '{' Declaracoes ListaCmd '}' {($2, $3)}
               | '{' ListaCmd '}' {([], $2)}

Declaracoes : Declaracoes Declaracao {$1 ++ $2}
            | Declaracao {$1}

Declaracao : Tipo ListaId ';' {map (\x -> x :#: ($1,0)) $2} 

Tipo : 'int' {TInt}
     | 'string' {TString}
     | 'double' {TDouble}

ListaId : ListaId ',' ID {$1 ++ [$3]}
        | ID {[$1]}

Bloco : '{' ListaCmd '}' {$2}

ListaCmd : ListaCmd Comando {$1 ++ [$2]}
         | Comando {[$1]}

Comando : CmdSe {$1}
        | CmdEnquanto {$1}
        | CmdPara {$1}
        | CmdAtrib {$1}
        | CmdEscrita {$1}
        | CmdLeitura {$1}
        | ChamadaProc {$1}
        | Retorno {$1}

Retorno : 'return' ExpressaoAritmetica ';' {Ret (Just $2)}
        | 'return' ';' {Ret Nothing}

CmdSe : 'if' '(' ExpressaoLogica ')' Bloco {If $3 $5 []}
      | 'if' '(' ExpressaoLogica ')' Bloco 'else' Bloco {If $3 $5 $7}

CmdEnquanto : 'while' '(' ExpressaoLogica ')' Bloco {While $3 $5}

CmdPara : 'for' '(' AtribSimples ';' ExpressaoLogica ';' AtribSimples ')' Bloco {For $3 $5 $7 $9}

ExpressaoLogica : ExpressaoLogica '&&' ExprLog {And $1 $3}
                | ExpressaoLogica '||' ExprLog {Or $1 $3}
                | ExprLog {$1}

ExprLog : '!' ExprLog {Not $2}
        | '(' ExpressaoLogica ')' {$2}
        | ExpressaoRelacional {Rel $1}

ExpressaoRelacional : ExpressaoAritmetica '>=' ExpressaoAritmetica {Rge $1 $3}
                    | ExpressaoAritmetica '<=' ExpressaoAritmetica {Rle $1 $3}
                    | ExpressaoAritmetica '==' ExpressaoAritmetica {Req $1 $3}
                    | ExpressaoAritmetica '/=' ExpressaoAritmetica {Rdif $1 $3}
                    | ExpressaoAritmetica '>' ExpressaoAritmetica {Rgt $1 $3}
                    | ExpressaoAritmetica '<' ExpressaoAritmetica {Rlt $1 $3}

AtribSimples : ID '=' ExpressaoAritmetica {Atrib $1 $3}
             | ID '=' LITERAL {Atrib $1 (Lit $3)}

CmdAtrib : AtribSimples ';' {$1}

CmdEscrita : 'print' '(' ExpressaoAritmetica ')' ';' {Imp $3}
           | 'print' '(' LITERAL ')' ';' {Imp (Lit $3)} 

CmdLeitura : 'read' '(' ID ')' ';' {Leitura $3} 

ChamadaProc : ChamadaFuncao ';' {Proc (fst $1) (snd $1)}

ChamadaFuncao : ID '(' ListaParametros ')' {($1, $3)}
              | ID '(' ')' {($1, [])} 

ListaParametros : ListaParametros ',' ExpressaoAritmetica {$1 ++ [$3]}
                | ExpressaoAritmetica {[$1]}
                | {[]} 

ExpressaoAritmetica : '-' ExprA {Neg $2}
                    | ExprA {$1}

ExprA : ExprA '+' EA {Add $1 $3}
      | ExprA '-' EA {Sub $1 $3}
      | EA {$1}

EA : EA '*' E {Mul $1 $3}
   | EA '/' E {Div $1 $3}
   | E {$1} 

E : CINT {Const (CInt $1)}
    | CDOUBLE {Const (CDouble $1)}
    | LITERAL {Lit $1}
    | ID {IdVar $1}
    | '(' ExpressaoAritmetica ')' {$2}
    | ID '(' ListaParametros ')' {Chamada $1 $3}

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)
}