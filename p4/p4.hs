--break an integer into a list of digits
numberToDigitList :: Int -> [Int]
numberToDigitList 0 = []
numberToDigitList x = (numberToDigitList rest) ++ [digit]
    where
        digit = x `rem` 10
	rest = x `quot` 10

--Check if a list is a palendrome
isPalendrome :: Eq a =>  [a] -> Bool
isPalendrome x = all eqls zipped
    where
        eqls (a,b) = (a == b)
        zipped = zip x (reverse x)  

--Check if a number is a palendrome
isPalendromeNum :: Int -> Bool
isPalendromeNum n = isPalendrome (numberToDigitList n)

--findPalendromeNumbers :: Int -> [Int]
palendromeNumbers = filter isPalendromeNum [a*b | a <- [100..999], b <- [100..999]]

main=print (maximum palendromeNumbers)
