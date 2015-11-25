--break an integer into a list of digits
numberToDigitList :: Integer -> [Integer]
numberToDigitList 0 = []
numberToDigitList x = (numberToDigitList rest) ++ [digit]
    where
        digit = x `rem` 10
        rest = x `quot` 10

digitListToNum :: [Integer] -> Integer
digitListToNum [] = 0
digitListToNum digits = sum scaledDigits
    where 
        digitsRev = reverse digits
        scaledDigits =  zipWith (*)  (map (10^) [0..]) digitsRev

digitsAreOdd :: [Integer] -> Bool
digitsAreOdd digits = all (\n-> n`rem` 2 /= 0) digits

numberIsReversible :: Integer -> Bool
numberIsReversible n 
    | (length revDigits) == (length digits) = digitsAreOdd revSum
    | otherwise = False
        where 
            digits = numberToDigitList n
            rev = digitListToNum (reverse digits)
            revDigits = numberToDigitList rev
            revSum = numberToDigitList (n + rev)

main=print (length (filter numberIsReversible [1..1000000000]))