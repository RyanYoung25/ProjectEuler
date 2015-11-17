--Create a function to check if prime
ifPrime :: Int -> Bool
ifPrime n = (all (\a -> n `rem` a /= 0) [2..lim])
    where
        lim = (ceiling (sqrt (fromIntegral n)))

--Find the nth prime number

--findPrime :: Int -> Int
--findPrime n = takeWhile (length < 100001) (filter ifPrime [2..])

--Recursively find prime
-- Takes the current number, the number of primes to find, the current number of primes we found
findPrime' :: Int -> Int -> Int -> Int
findPrime' num max count 
    | count == max = num - 1
    | isPrime = findPrime' (num+1) max (count+1)
    | otherwise = findPrime' (num+1) max count
    where 
        isPrime = ifPrime num

findPrime :: Int -> Int
findPrime n = (findPrime' 2 n 1)

main = putStrLn (show (findPrime 10001))