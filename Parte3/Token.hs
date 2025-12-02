module Token where

data Token
  = 
  -- tipo
  TINT
  | TDOUBLE
  | TSTRING
  | TVOID
  
  -- const
  | CINT Int
  | CDOUBLE Double
  | LITERAL String

  -- ops
  | ADD
  | SUB
  | MUL
  | DIV

  | LPAR -- (
  | RPAR -- )
  | LBRA -- {
  | RBRA -- }

  | AND -- && 
  | OR -- ||
  | NOT -- !

  | IF
  | ELSE
  | WHILE
  | FOR

  | RL -- <
  | RG -- > 
  | RLEQ -- <=
  | RGEQ -- >=
  | REQ -- ==
  | RNEQ -- /=

  | COMMA -- ,
  | SEMI -- ;
  | ASSIGN -- =
  | READ
  | PRINT
  | RETURN
  | ID String
  deriving (Eq, Show)
