sumNumbers :: (Integral a) =>  a -> a
sumNumbers n = foldl (\acc x -> acc + x) 0 [1..n]

sumNumSquard :: Int -> Int
sumNumSquard num = foldl (\acc x -> acc + x) 0 (map (\x -> x*x) [1..num])


main = putStrLn (show (((sumNumbers 10000)^2)  - (sumNumSquard 10000)))

