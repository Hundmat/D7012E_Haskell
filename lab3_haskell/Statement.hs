module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement


-- program ::= statements
--    statement ::= variable ':=' expr ';'
--            | 'skip' ';'
--            | 'begin' statements 'end'
--            | 'if' expr 'then' statement 'else' statement
--            | 'while' expr 'do' statement
--            | 'read' variable ';'
--            | 'write' expr ';'
--    statements ::= {statement}
--    variable ::= letter {letter}

data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement | 
    Skip | 
    Begin [Statement] | 
    While Expr.T Statement |
    Read String | 
    Write Expr.T
    deriving Show


--    read k;
--    read n;
--    m := 1;
--    while n-m do
--      begin
--        if m - m/k*k then
--          skip;
--        else
--          write m;
--        m := m + 1;
--      end

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

read' = accept "read" -# word #- require ";" >-> buildRead
buildRead w = Read w 

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,p) = While e p

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin s = Begin s

if' = (accept "if" -# Expr.parse) # (require "then" -# parse # require "else" -# parse) >-> buildIf
buildIf (i, (t, e)) = If i t e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! if' ! skip ! while ! read' ! write ! begin
  toString = error "Statement.toString not implemented"




