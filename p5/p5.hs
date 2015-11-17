--function to return if a number is evenly divisible by all numbers 1..20
divisBy :: Int -> Bool
divisBy n = all evenDivs [1..20]
    where evenDivs a = (n `rem` a) == 0

main=print (takeWhile (not . divisBy) [1..])