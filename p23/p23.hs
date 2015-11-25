import qualified Data.Set as Set

--Make a list of proper factors for a number
findFactorList :: Int -> [Int]
findFactorList n = filter (\a -> n `rem` a == 0) [1..n-1]

--Function to tell if a number is abundant 
isAbundant :: Int -> Bool
isAbundant n = (factSum > n)
    where
        factSum = sum (findFactorList n)

--Create a set of abundant numbers upto n
createAbundantSet :: Int -> Set Int
createAbundantSet = Set.fromList abundantList
    where
        abundantList = filter isAbundant [1..n]

--Check if 