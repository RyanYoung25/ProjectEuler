--Function to return the nth value of Fibonacci
fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2) 

--Function to get a list of fibonacci numbers under a certian value
getFibListUnderN :: Int -> [Int]
getFibListUnderN n = takeWhile (<n) (map fib [1..])

--Funtion to get only even numbers in a list
getEvens :: [Int] -> [Int]
getEvens li = filter isEven li
    where
        isEven n = (n `rem` 2) == 0

main=print (sum (getEvens (getFibListUnderN 4000000)))