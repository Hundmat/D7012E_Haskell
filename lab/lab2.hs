-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
--------------------this is so we can write App as the code only worked for Op before------------------------

unparse (App apper e2) = "(" ++ apper ++ unparse e2 ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env

--------------------sin cos log exp------------------------
eval (App "sin" right) env = sin (eval right env)
eval (App "cos" right) env = cos (eval right env)
eval (App "log" right) env = log (eval right env)
eval (App "exp" right) env = exp (eval right env)


diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)

--------------------sin cos log exp------------------------
diff v (App "sin" e2) = 
  Op "*" (App "cos" e2) (diff v e2)
diff v (App "cos" e2) = 
  Op "*" (Op "*" (Const (-1)) (App "sin" e2)) (diff v e2)
diff v (App "log" e2) = 
  Op "/" (diff v e2) e2
diff v (App "exp" e2) = 
  Op "*" (App "exp" e2) (diff v e2)


diff _ _  = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
--------------------this is so we can write App as the code only worked for Op before------------------------
simplify (App apper right) = App apper (simplify right)


-- lambda is used as we only take in a EXPR and then want to apply a flaot value to it so it is easy to just use lambda 
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (func, Var var) = \value -> eval func [(var, value)]

-- mkfun (func, Var var) value = eval func [(var, value)]



--   The formula used is:
--   @x1 = x0 - (f(x0) / f'(x0))@
--  So if the abs of the difference between f and f' is smaller than 0.0001 or equal we will stop searching and return the x_n value
findzero :: String -> String -> Float -> Float
findzero var func x0 =
  let 
    parseFunc = parse func
    f = mkfun (parseFunc, Var var)
    f' = mkfun (diff (Var var) (parseFunc), Var var) 
  in 
    findzero' var f f' x0
      where 
        findzero' var f f' x0 =
          let 
            x1 = x0 - f x0 / f' x0
            difference = abs (x1 - x0)
          in 
            if difference <= 0.0001 
              then x1
              else findzero' var f f' (x1)











main :: IO()
main = do


  print("Task 1")
  print(unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))) -- should print the derivative
  print(unparse (simplify (diff (Var "x") (parse "x*x")))) -- should print the derivative
  print(unparse (simplify (diff (Var "x") (parse "cos(x)")))) -- should print the derivative
  print(unparse (simplify (diff (Var "x") (parse "sin(2*x)")))) -- should print the derivative

  print("Task 2")
  print((mkfun (parse "x*x+2", Var "x")) 3.0) -- evaluates the function at x = 3.0
  print((mkfun (parse "x+2", Var "x")) 3.0) -- evaluates the function at x = 3.0
  print((mkfun (parse "log(x)", Var "x")) 1.0) -- evaluates the function at x = 3.0


  print("Task 3")
  print(findzero "x" "x*x*x+x-1" 1.0) -- should evaluate to 0.68232775
  print(findzero "y" "cos(y)*sin(y)" 2.0) -- should evaluate to 1.5707964
  print(findzero "x" "exp(x)-10" 2.0) -- Should evaluate to 2.3025851
  print(findzero "x" "log(x)-2" 5.0) -- Should evaluate to 7.389056

