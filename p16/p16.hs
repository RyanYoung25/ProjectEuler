--break an integer into a list of digits
numberToDigitList :: Integer -> [Integer]
numberToDigitList 0 = []
numberToDigitList x = (numberToDigitList rest) ++ [digit]
    where
        digit = x `rem` 10
	rest = x `quot` 10

main=print (sum (numberToDigitList (2^1000)))
