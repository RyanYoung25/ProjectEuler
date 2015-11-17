--Function to filter out prime numbers
ifPrime :: Int -> Bool
ifPrime 2 = True
ifPrime n = (all (\a -> n `rem` a /= 0) [2..lim])
    where
        lim = (ceiling (sqrt (fromIntegral n)))

--Make a sive of prime numbers
makeSive :: [Int] -> [Int]
makeSive [] = []
makeSive (x:xs) = [x] ++ makeSive ( filter (\a -> a `rem` x /= 0) xs)

ans = sum (filter ifPrime [2..2000000])
siveSum  = sum (makeSive [2..2000000])

main= print (ans)