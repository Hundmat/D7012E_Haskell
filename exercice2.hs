


-- 9.2

-- map f xs = [f x | x <- xs]
-- we will do const 1 for every element in xs
-- than calculate the sum
myLength :: [a] -> Int
myLength xs = sum(map(const 1) xs)

-- 9.4

-- Firstly we will go through the first map addOne ns which will iterate over ns and do addOne each time, after this map addOne will iterate over the anwser from before

-- 9.6
squares :: Int -> Int
squares x = x^2


checkGreaterZero :: Int -> Bool
checkGreaterZero x 
    | x > 0 = True
    | otherwise = False   

sumSquares :: [Int] -> Int
sumSquares ns = sum (map squares ns)

allPositive :: [Int] -> Bool 
allPositive ns = all checkGreaterZero ns

-- 9.7

allEqual :: [Int] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

minVal :: [Int] -> Int 
minVal [] = maxBound :: Int
minVal (x:xs)
    | x < minVal xs = x
    | otherwise = minVal xs

increasing :: [Int] -> Bool
increasing [] = True
increasing [_] = True
increasing (x:y:xs) = x < y && increasing (y:xs)


allGreaterZero :: [Int] -> Bool
allGreaterZero ns = all checkGreaterZero ns


