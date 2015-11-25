--Make a factorial function
fact :: (Integral a) => a -> a
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

--break an integer into a list of digits
numberToDigitList :: Integer -> [Integer]
numberToDigitList 0 = []
numberToDigitList x = (numberToDigitList rest) ++ [digit]
    where
        digit = x `rem` 10
        rest = x `quot` 10

main= print (sum (numberToDigitList (fact 100)))