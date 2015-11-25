--function that returns true if the number passed is a part of an amicable pair
isAmicable :: Int -> Bool
isAmicable a = dB == a && a /= b
    where
        b = sum (findFactorList a)
        dB = sum (findFactorList b)


--Make a list of proper factors for a number
findFactorList :: Int -> [Int]
findFactorList n = filter (\a -> n `rem` a == 0) [1..n-1]

main= print (sum (filter isAmicable [1..10000]))