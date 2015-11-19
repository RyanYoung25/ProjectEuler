--Function to filter out prime numbers
ifPrime :: Int -> Bool
ifPrime 1 = False
ifPrime 2 = True
ifPrime n = (all (\a -> n `rem` a /= 0) [2..lim])
    where
        lim = (ceiling (sqrt (fromIntegral n)))

--Make a sive of prime numbers upto a number

--Make a list of factors for a number
findFactorList :: Int -> [Int]
findFactorList n = filter (\a -> n `rem` a == 0) [1..n]


--Found it in way too long of a time
main=print (maximum (filter ifPrime (findFactorList 600851475143)))