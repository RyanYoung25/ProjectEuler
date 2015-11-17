--Create a list of numbers under n that are multiples of 3 or 5
makeMultipleList :: Int -> [Int]
makeMultipleList n = filter (\a -> (evenDivs a 3) || (evenDivs a 5)) [1..lim]
    where
        evenDivs a n = (a `rem` n) == 0 --Helper function
        lim = n-1

--Sum the numbers of the list under 1000
main= print (sum (makeMultipleList 1000)) 