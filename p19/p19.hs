--My plan for this problem is to work from a list of years to a list of months to a list of days 
-- then zip the list of days with a repeating list of 1 through 7, I'll filter the zipped list with any day (1,6) 
-- corresponding to a first month sunday, sunday being the 6th day

--from a year number create a list of months
yearToMonths :: Int -> [Int]
yearToMonths n
    | (n `rem` 100 == 0) && (n `rem` 400 /= 0) = [1..12]
    | (n `rem` 4 == 0) = [1,13] ++ [3..12]
    | otherwise = [1..12]

monthToDays :: Int -> [Int]
monthToDays 13 = [1..29]
monthToDays 2 = [1..28]
monthToDays 4 = [1..30]
monthToDays 6 = [1..30]
monthToDays 9 = [1..30]
monthToDays 11 = [1..30]
monthToDays n = [1..31]

countSundays :: Int -> Int -> Int
countSundays start end = length (filter dayFilter zippedList)
    where
        monthList = concatMap yearToMonths [start..end]
        dayList = concatMap monthToDays monthList
        zippedList = zip (concat (repeat [1..7])) dayList
        dayFilter (a,b) = (a == 6) && (b == 1)

main=print (countSundays 1901 2000)