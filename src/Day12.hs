import Data.Char
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

tARGET = (120, 20)
-- tARGET = (0, 19)
sTART = (0, 20)

--tARGET = (5, 2)
--sTART = (0, 0)


listToHeightMap :: [String] -> (Int, Int) -> Map (Int, Int) Int
listToHeightMap [[]] _ = Map.empty
listToHeightMap ([]:rst) (x, y) = listToHeightMap rst (0, (y+1))
listToHeightMap ((item:rstLine):rst) (x, y) = Map.insert (x, y) (ord item - 97) (listToHeightMap (rstLine:rst) ((x+1), y))

findPathLenght :: Map (Int, Int) Int -> [(Int, Int)] -> [((Int, Int), [(Int, Int)])] -> [(Int, Int)]
findPathLenght heightMap visited ((currPos, explored):toVisit)
    | currPos == tARGET = explored
    | elem currPos visited = findPathLenght heightMap visited toVisit
    | otherwise = findPathLenght heightMap (visited ++ [currPos]) (toVisit ++ nextSteps)
    where 
        currHeight = fromJust (Map.lookup currPos heightMap)
        nextSteps = zip (filter (validateStep currHeight heightMap explored) (allSteps currPos)) $ repeat (explored++[currPos])

findA_Lenght :: Map (Int, Int) Int -> [(Int, Int)] -> [((Int, Int), [(Int, Int)])] -> [(Int, Int)]
findA_Lenght heightMap visited ((currPos, explored):toVisit)
    | currHeight == 0 = explored
    | elem currPos visited = findA_Lenght heightMap visited toVisit
    | otherwise = findA_Lenght heightMap (visited ++ [currPos]) (toVisit ++ nextSteps)
    where 
        currHeight = fromJust (Map.lookup currPos heightMap)
        nextSteps = zip (filter (validateStepReverse currHeight heightMap explored) (allSteps currPos)) $ repeat (explored++[currPos])

allSteps (x, y) = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)] 
validateStep :: Int -> Map (Int, Int) Int -> [(Int, Int)] -> (Int, Int) -> Bool
validateStep currHeight heightMap explored step = do
    let newHeight = fromJust (Map.lookup step heightMap)
    Map.member step heightMap && notElem step explored && (newHeight - currHeight) <= 1

validateStepReverse :: Int -> Map (Int, Int) Int -> [(Int, Int)] -> (Int, Int) -> Bool
validateStepReverse currHeight heightMap explored step = do
    let newHeight = fromJust (Map.lookup step heightMap)
    Map.member step heightMap && notElem step explored && (currHeight - newHeight) <= 1


main = do
    file <- readFile "input\\day_12.txt"
    let heighs = lines file
    let heightMap = listToHeightMap heighs (0, 0)
    -- print $ length (findPathLenght heightMap [] [(sTART, [])]) --p1
    print $ length (findA_Lenght heightMap [] [(tARGET, [])]) --p1

