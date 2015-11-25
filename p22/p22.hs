import Text.CSV
import Data.List
import qualified Data.Map as Map

handleError e = []
--Create a function to convert a CSV to a List
csvToList n = either handleError getCSVrecord n

getCSVrecord n = head n

--Create a static map of letters to numbers
letterToNum = Map.fromList (zip ['A'..'Z'] [1..])

--Function to calculate the score of a name
calcNameScore :: String -> Int
calcNameScore name = sum (map (letterToNum Map.!) name)

--Function to calculate the score of a name position tuple
calculateScore :: (Int, String) -> Int
calculateScore (n, name) = n * (calcNameScore name)


calculateListScore :: [String] -> Int
calculateListScore nameList = sum (map calculateScore sortedIndexedList)
    where
        sorted = sort nameList
        sortedIndexedList = zip [1..] sorted


--Load the csv file
main=do
            fileData <- parseCSVFromFile "names.txt"
            print (calculateListScore (csvToList fileData))

