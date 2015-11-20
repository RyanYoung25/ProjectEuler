--Make a function that puts a number into a string with no spaces
numberToWords :: Int -> String 
numberToWords n 
    | n < 20 = simpleNums !! n 
    | n < 100 && evenTens = tens !! tenVal
    | n < 100 = tens !! tenVal ++ numberToWords restOnes
    | n < 1000 && evenHund = hundreds !! hundredVal
    | n < 1000 = hundreds !! hundredVal ++ "and" ++ numberToWords restTens
    | n == 1000 = "onethousand"
    where
        simpleNums = ["zero", "one","two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
        tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
        hundreds = ["onehundred", "twohundred", "threehundred", "fourhundred", "fivehundred", "sixhundred", "sevenhundred", "eighthundred", "ninehundred"]
        tenVal = (n `quot` 10) - 2
        hundredVal = n `quot` 100  - 1
        evenTens = (n `rem` 10) == 0
        evenHund = (n `rem` 100) == 0
        restOnes = n `rem` 10
        restTens = n `rem` 100

--Function to count the letters of all numbers upto n
getLetterCount :: Int -> Int
getLetterCount n = foldl (+) 0 (map length (map numberToWords [1..n]))

main= print (getLetterCount 1000)