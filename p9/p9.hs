
--multiplyTuple ::  -> Int
multiplyTuple ((a,b):[]) = a*b*(sqrt (a^2 + b^2))


main = print (multiplyTuple (filter (\ (a, b) -> a+b+(sqrt (a^2 + b^2)) == 1000) [(a, b) | a <- [1..500], b <- [a..500]]))
