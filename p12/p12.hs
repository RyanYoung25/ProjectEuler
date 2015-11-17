--make a triangular number
makeTriangleNum :: Int -> Int
makeTriangleNum n = sum [1..n]

makeTriangleNumList :: Int -> [Int]
makeTriangleNumList lim = map makeTriangleNum [1..lim]

findFactorCount :: Int -> Int
findFactorCount n = length (filter (\a -> n `rem` a == 0) [1..lim]) * 2
    where
        lim = floor (sqrt (fromIntegral n))

isFactorLessThan :: Int-> Int -> Bool
isFactorLessThan lim n = findFactorCount n <= lim

l = (takeWhile (isFactorLessThan 500) (map makeTriangleNum [1..]))

--The  program stops collecting the list for the first  element with more than 500 factors, 
--  this means that the length of the list is the number of triangle numbers there were before
--  so to get the next we can add 1 plus the length of the list to the last element in the list. 
main= print ((maximum l) + (length l) + 1)