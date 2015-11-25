import Data.List.Ordered

--break an integer into a list of digits
numberToDigitList :: Int -> [Int]
numberToDigitList 0 = []
numberToDigitList x = (numberToDigitList rest) ++ [digit]
    where
        digit = x `rem` 10
        rest = x `quot` 10

-- Tell if a number is bouncy
isBouncy :: Int -> Bool
isBouncy n = not ((isSortedBy (<=) numList) || (isSortedBy (>=) numList))
    where
        numList = numberToDigitList n

--Find the proportion of bouncy numbers from 100 upto the value. 
findBouncyProportion :: Int -> Double
findBouncyProportion num = (fromIntegral (length (filter isBouncy [1..num]))) / (fromIntegral num)

--Old way took too long. Unfortunately lists aren't always the best way I guess. 

--Recursively do it. 
findBouncyPercent :: Double -> Int -> Int -> Int
findBouncyPercent goal bounceCount num 
    | bounceProportion < goal = findBouncyPercent goal updatedCount (num+1)
    | otherwise = num
    where
        updatedCount 
            | isBouncy num = bounceCount + 1
            | otherwise = bounceCount
        bounceProportion = (fromIntegral updatedCount) / (fromIntegral num)

--Found a crazy thing, when you use a float you get 3 below the correct answer, when you use a double you get the answer. Always use doubles...

main =print (findBouncyPercent 0.99000 0 100)