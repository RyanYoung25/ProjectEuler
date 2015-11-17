--Create a collatz sequence based on a number. 
collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq n 
    | ((n `rem` 2) == 0 ) = (n : (collatzSeq (n `quot` 2)))
    | otherwise = (n : (collatzSeq ((3*n+1))))

--Generate a list of all collatz sequence lengths and numbers upto n
makeCollatzLists :: Int -> [(Int, Int)]
makeCollatzLists n = zip [1..n] (map length (map collatzSeq [1..n]))

--Find the max of the second element of a zipped list and return the first elem
findMaxZipped :: [(Int, Int)] -> Int
findMaxZipped ((a,b):[]) = a
findMaxZipped ((a, b):(c,d):xs)
    | b > d = findMaxZipped ((a,b):xs)
    | otherwise = findMaxZipped ((c,d):xs)

main= print (findMaxZipped (makeCollatzLists 1000000))