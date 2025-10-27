{
module Lex where

import Token
import System.IO
}

%wrapper "basic"

$digit = [0-9] -- digits
$alpha = [a-Za-z]
@id = $alpha [$alpha $digit]*
@int = $digit+
@double = $digit+(\.$digit+)
@literal = \"[^ \"]*\"

tokens :-
<0> $white+ ; 

<0> "int" {\s -> TINT}
<0> "double" {\s -> TDOUBLE}
<0> "string" {\s -> TSTRING}
<0> "void" {\s -> TVOID}

<0> @double {\s -> CDOUBLE (read s)} 
<0> @int {\s -> CINT (read s)} 
<0> @literal {\s -> LITERAL (read s)}

<0> "+" {\s -> ADD} 
<0> "-" {\s -> SUB} 
<0> "*" {\s -> MUL} 
<0> "/" {\s -> DIV} 
<0> "(" {\s -> LPAR} 
<0> ")" {\s -> RPAR} 
<0> "{" {\s -> LBRA} 
<0> "}" {\s -> RBRA}

<0> "&&" {\s -> AND} 
<0> "||" {\s -> OR} 
<0> "!" {\s -> NOT}

<0> "if" {\s -> IF} 
<0> "else" {\s -> ELSE} 
<0> "while" {\s -> WHILE} 
<0> "for" {\s -> FOR}

<0> "<=" {\s -> RLEQ} 
<0> ">=" {\s -> RGEQ} 
<0> "<" {\s -> RL} 
<0> ">" {\s -> RG} 
<0> "==" {\s -> REQ} 
<0> "/=" {\s -> RNEQ}

<0> "," {\s -> COMMA} 
<0> ";" {\s -> SEMI} 
<0> "=" {\s -> ASSIGN} 
<0> "read" {\s -> READ} 
<0> "print" {\s -> PRINT}
<0> "return" {\s -> RETURN}
<0> @id {\s -> ID s}

