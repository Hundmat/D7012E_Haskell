module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = (\x -> createString x)

exec :: T -> [Integer] -> [Integer]        
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input


createString :: T -> String
createString (Program stmts) = concat [Statement.toString s | s <- stmts]