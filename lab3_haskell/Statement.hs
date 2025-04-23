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
    Write Expr.T |
    Repeat Statement Expr.T
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
buildWhile (con,p) = While con p

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin s = Begin s

if' = (accept "if" -# Expr.parse) # (require "then" -# parse # require "else" -# parse) >-> buildIf
buildIf (i, (t, e)) = If i t e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

repeat' = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s, e) = Repeat s e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []


exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Assignment v e: stmts) dict input =
    exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input

exec (Read w : stmts) dict input = 
    exec stmts (Dictionary.insert(w, head input) dict) (tail input)

exec (While con p: stmts) dict input=
    if(Expr.value con dict) > 0
        then exec (p:While con p: stmts) dict input
        else exec stmts dict input


exec (Begin s: stmts) dict input = 
    exec (s++stmts) dict input

exec (Skip: stmts) dict input =
    exec stmts dict input

exec (Write e: stmts) dict input =
    Expr.value e dict:exec stmts dict input

exec (Repeat s e : stmts) dict input =
    exec (s : control : stmts) dict input
  where 
    control = if (Expr.value e dict) > 0
              then Skip
              else Repeat s e


-- exec (Repeat s e: stmts) dict input =
--     if (Expr.value e dict) > 0
--     then exec stmts dict input
--     else exec (s:Repeat s e:stmts) dict input





instance Parse Statement where
  parse = assignment ! if' ! skip ! while ! read' ! write ! begin ! repeat'
  toString = (\x -> createString x)

createString :: T -> String
createString(Assignment v e) = v ++ " := " ++ toString e ++ ";\n"
createString(Read w) = "read " ++ w ++ ";\n"
createString(While con p) = "while " ++ toString con ++ " do\n" ++ toString p
createString(Begin s) = "begin\n" ++ concat [toString oneS|oneS <- s] ++ "end\n"
createString(Skip) = "skip;\n"
createString(Write e) = "write " ++ toString e ++ ";\n"
createString(If i t e) = "if " ++ toString i ++ " then\n" ++ toString t ++ "else\n" ++ toString e
createString(Repeat s e) = "repeat\n" ++ toString s ++ "until " ++ toString e ++";\n" 



