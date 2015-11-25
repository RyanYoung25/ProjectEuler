import Data.List

digitListToNum :: [Integer] -> Integer
digitListToNum [] = 0
digitListToNum digits = sum scaledDigits
    where 
        digitsRev = reverse digits
        scaledDigits =  zipWith (*)  (map (10^) [0..]) digitsRev

main= print (digitListToNum ((sort (permutations [0..9])) !! 999999))