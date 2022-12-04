import Data.List

assigmentsToList :: [String] -> [[Int]]
assigmentsToList (a:rst) = do
    let f = read (takeWhile (\x -> x /= '-') a)
    let s = read (tail(dropWhile (\x -> x /= '-') a))
    ([f..s] : assigmentsToList rst) 
assigmentsToList [] = [] 

findDuplicatePairs :: [[Int]] -> Int
findDuplicatePairs (a:b:rst) = isDuplicate a b + findDuplicatePairs rst
findDuplicatePairs [] = 0

isDuplicate :: [Int] -> [Int] -> Int
isDuplicate a b
    | elem (head a) b && elem (last a) b = 1
    | elem (head b) a && elem (last b) a = 1
    | otherwise = 0

isOverlap :: [Int] -> [Int] -> Int
isOverlap a b
    | elem (head a) b || elem (last a) b = 1
    | elem (head b) a || elem (last b) a = 1
    | otherwise = 0

findOverlapPairs :: [[Int]] -> Int
findOverlapPairs (a:b:rst) = isOverlap a b + findOverlapPairs rst
findOverlapPairs [] = 0


p1 assigments = do
    findDuplicatePairs assigments

p2 assigments = do
    findOverlapPairs assigments

main = do
    file <- readFile "input\\day_4.txt"
    let assigments = words file
    let assigmentsList = assigmentsToList assigments
    
    print $ p1 assigmentsList
    print $ p2 assigmentsList

