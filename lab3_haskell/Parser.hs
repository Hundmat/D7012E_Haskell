module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-), semicolon) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 
cons(a, b) = a:b

-- Håkan iterate

iterate' :: Parser a -> Int -> Parser [a]
iterate' m 0 = return []
iterate' m i = m # iterate' m(i-1)>->cons
  where 
    cons(a,b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> (\(a, b) -> b)

(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> (\(a, b) -> a)

spaces :: Parser String
spaces = iter (char ? isSpace)

-- spaces :: Parser String
-- spaces string = case space string of
--   Nothing -> Just("", string)
--   Just(c, rest) -> case spaces rest of 
--     Just(cs, final) -> Just(c:cs, final)

token :: Parser a -> Parser a
token m = m #- spaces

-- letter :: Parser Char
-- letter [] = Nothing
-- letter (s:cs)
--   | isAlpha s = Just (s, cs)
--   | otherwise = Nothing

letter :: Parser Char
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars amount = iterate' char amount 

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w  = accept w ! err w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n

number :: Parser Integer
number = token (digitVal #> number')


--- Exercise 
semicolon :: Parser Char
semicolon = char ? (==';')

-- char' :: Parser Char
-- char' (s:cs) = Just(s,cs)
-- char' [] = Nothing


space :: Parser Char
space [] = Nothing
space (s:cs)
  | isSpace s = Just (s, cs)
  | otherwise = Nothing

alphanum :: Parser Char
alphanum [] = Nothing
alphanum (s:cs)
  | isAlpha s = Just (s, cs)
  | isDigit s = Just (s, cs)
  | otherwise = Nothing


lit' :: Char -> Parser Char
lit' c = char ? (==c)

semicolon' :: Parser Char  
semicolon' = lit' ';'









