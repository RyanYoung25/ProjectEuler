--Create a function to find the lcd between two numbers. 
lcm' :: Int -> Int -> Int
lcm' a b = head (take 1 (dropWhile isNotMultiple [minVal..maxVal]))
    where
        minVal = max a b
        maxVal = a*b
        isNotMultiple n = (( n `rem` a) /= 0) || ((n `rem` b) /= 0)

main=print (foldl lcm' 1 [1..20]) 