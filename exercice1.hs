main :: IO ()
main = do
    print (threeDifferent 3 4 3)  
    print (fourDifferent 3 4 3 2)
    print (fourDifferent 3 2 1 4)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = (n/=m) && (m/=p) && (p/=n)

fourDifferent :: Int -> Int -> Int -> Int -> Bool
fourDifferent m n p o = (n/=m) && (m/=p) && (p/=n) && (m /=o) && (n /= o) && (p/=o)

fourDifferent2 :: Int -> Int -> Int -> Int -> Bool
fourDifferent2 m n p o = (threeDifferent m n p )&& (m /=o) && (n /= o) && (p/=o)

numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
    | a == 0 = 0
    | b^2 - 4*a*c > 0 = 2
    | b^2 - 4*a*c == 0 = 1
    | otherwise = 0