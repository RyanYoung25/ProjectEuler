import Data.List

--function that traverses a triangle list, passed in reversed summing as it goes
-- it access the max of the two possible neigbors from the first max

--This is wrong, I was racing and thought on a whim it would work but it's wrong
findMaxPath' :: Int -> [[Int]] -> [Int]
findMaxPath' _ [x:[]] = [x] --Base case, list of one element
findMaxPath' 0 tree = (findMaxPath' 0 (tail tree)) ++ [(head (head tree))]
findMaxPath' index tree = (findMaxPath' maxIndex smallerTree) ++ [maxValue] 
    where
        smallerTree = tail tree
        layer = head tree
        maxValue = max (layer !! index) (layer !! (index-1))
        maxIndex = maybe 0 id (elemIndex maxValue layer)

findMaxPath :: [[Int]] -> [Int]
findMaxPath tree = findMaxPath' maxIndex smallerTree ++ [maxVal]
    where
        revTree = reverse tree
        smallerTree = tail revTree
        layer = head revTree
        maxVal = maximum layer
        maxIndex = maybe 0 id (elemIndex maxVal layer)

--Recursively decend depth first and prune kf possible. 
depthFirstMaxSum :: [[Int]] -> Int -> Int
depthFirstMaxSum (xs:[]) index = xs !! index
depthFirstMaxSum (x:xs) index = (x !! index) + (max (depthFirstMaxSum xs index) (depthFirstMaxSum xs (index + 1)))

bigTree = 
    [[75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]


main = print (depthFirstMaxSum bigTree 0)