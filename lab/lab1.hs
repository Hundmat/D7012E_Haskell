printSubLists :: [(Int, Int, Int, [Int])] -> IO ()
printSubLists xs = do
    putStrLn "size  i   j   sublist"
    printRows xs
  where
    printRows [] = return ()
    printRows ((i, j, sumVal, sub):xs) = do
        putStrLn (show sumVal ++ "   " ++ show i ++ "   " ++ show j ++ "   " ++ show sub)
        printRows xs
    
    
    



mySum :: [Int] -> Int
mySum []= 0
mySum (x:xs) = x + mySum xs

myTake :: Int -> [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
myTake 0 _      = []
myTake _ []     = []
myTake k (x:xs) = x : myTake (k-1) xs

lastElement :: (Int,Int,Int,[Int]) -> Int
lastElement (_,_,last,_) = last

myIns :: (Int,Int,Int,[Int]) -> [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
myIns x [] = [x]
myIns x (h:t)
    | lastElement x <= lastElement h = x : h : t
    | otherwise = h : myIns x t

myISort :: [(Int,Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
myISort [] = []
myISort (x:xs) = myIns x (myISort xs)

-- This just iterate over [i..j] and give that value to k which filter that index value from the list xs example [1,2,3,4] -> [3,2,-4,3]
subList :: Int -> Int -> [Int] -> [Int]
-- https://discourse.haskell.org/t/list-comprehension-on-indices/7465
--  indexes lists "!!"
subList i j xs = [xs !! k | k <- [i..j]]

--             i    j
-- First index 0 -> 1 then 0 -> 2 ... 0 -> n-1
-- Second index 1 -> 1 then 1 -> 2 ... 1 -> n-1
--              . -> .      . -> . ... . -> n-1
--              . -> .      . -> . ... . -> n-1
--              . -> .      . -> . ... . -> n-1
-- Last index n -> n (so last sublist)

-- Example from test : [(0,0,[3]),(0,1,[3,2]),(0,2,[3,2,-4]),(0,3,[3,2,-4,3]),(1,1,[2]),(1,2,[2,-4]),(1,3,[2,-4,3]),(2,2,[-4]),(2,3,[-4,3]),(3,3,[3])]
-- https://stackoverflow.com/questions/50451401/using-list-comprehension-with-two-variables-in-haskell
allSubList :: [Int] -> [(Int,Int,[Int])]
allSubList xs = [(i, j, subList i j xs) | i <- [0 .. length xs - 1],j <- [i .. length xs - 1]]

sumSubList :: [(Int,Int,[Int])] -> [(Int,Int,Int,[Int])]
sumSubList xs = [(i, j,mySum lst, lst) | (i, j, lst) <- xs]



smallestKset :: Int -> [Int] -> [(Int, Int, Int, [Int])]
smallestKset k = myTake k . myISort . sumSubList . allSubList


main :: IO ()
main = do
    putStrLn "Test 1\n"
    let k = 15
    let list = [x*(-1)^x | x <- [1..100]]
    let result = smallestKset k list
    printSubLists result
    putStrLn "\nTest 2\n"
    let k = 6
    let list = [24,-11,-34,42,-24,7,-19,21]
    let result = smallestKset k list
    printSubLists result
    putStrLn "\nTest 3\n"
    let k = 8
    let list = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]
    let result = smallestKset k list
    printSubLists result


    
