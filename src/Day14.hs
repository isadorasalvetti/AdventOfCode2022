import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

sandSpawn = (500, 0)
caveFloor = 171
-- caveFloor = 11

parseCommaPair :: String -> (Int, Int)
parseCommaPair str = do
    let (a, b) = break (==',') str
    (read a, read (tail b))

parseInput :: [String] -> Set (Int, Int)
parseInput = foldr (Set.union . makeAllInBetweens . parseInputLine) Set.empty

parseInputLine :: String -> [(Int, Int)]
parseInputLine [] = []
parseInputLine (' ':'-':'>':' ':rst) = parseInputLine rst
parseInputLine str = parseCommaPair fst : parseInputLine rst
    where (fst, rst) = span isDigitOrComma str

makeAllInBetweens :: [(Int, Int)] -> Set (Int, Int)
makeAllInBetweens [num] = Set.empty
makeAllInBetweens (num:nextNum:rst) = Set.union (makeInBetweens num nextNum) (makeAllInBetweens (nextNum:rst))

makeInBetweens :: (Int, Int) -> (Int, Int) -> Set (Int, Int)
makeInBetweens (x1, y1) (x2, y2)
    | x1 == x2 = Set.fromList $ map (x1, ) [min y1 y2 .. max y1 y2]
    | otherwise = Set.fromList $ map (, y1) [min x1 x2 .. max x1 x2]

isDigitOrComma a = a `elem` "1234567890,"

addSand :: Set (Int, Int) -> Int -> Int
addSand cave counter
    | counter > 100000 = counter
    | snd newSandPosition == (caveFloor-1) = counter
    | otherwise = addSand (Set.insert newSandPosition cave) (counter+1)
    where newSandPosition = sandRestPos sandSpawn cave

addSandStack :: Set (Int, Int) -> Int -> Int
addSandStack cave counter
    | counter > 100000 = -1
    | newSandPosition == (500, 0) = counter+1
    | otherwise = addSandStack (Set.insert newSandPosition cave) (counter+1)
    where newSandPosition = sandRestPos sandSpawn cave

sandRestPos :: (Int, Int) -> Set (Int, Int) -> (Int, Int)
sandRestPos (x, y) cave
    | y == (caveFloor-1) = (x, y)
    | Set.notMember (x, y+1) cave = sandRestPos (x, y+1) cave
    | Set.notMember (x-1, y+1) cave= sandRestPos (x-1, y+1) cave
    | Set.notMember (x+1, y+1) cave = sandRestPos (x+1, y+1) cave
    | otherwise = (x, y)

getBottom :: Set (Int, Int) -> Int
getBottom rocks = maximum (map snd (Set.toList rocks))

addSandDebug :: Set (Int, Int) -> Set (Int, Int)
addSandDebug cave
    | newSandPosition == (500, 0) = Set.insert newSandPosition cave
    | otherwise = addSandDebug (Set.insert newSandPosition cave)
    where newSandPosition = sandRestPos sandSpawn cave

displayCave :: ((Int, Int), Int) -> Set (Int, Int) -> Set (Int, Int) -> String
displayCave ((minX, maxX), maxY) rocks all = concatMap (\y -> displayCaveLine [minX..maxX] y rocks all) [0..maxY]

displayCaveLine :: [Int] -> Int -> Set (Int, Int) -> Set (Int, Int) -> String
displayCaveLine (el:rst) row rocks all
    | null rst = "\n"
    | (el, row) `elem` rocks = "#" ++ displayCaveLine rst row rocks all
    | (el, row) `elem` all = "o" ++ displayCaveLine rst row rocks all
    | otherwise = "." ++ displayCaveLine rst row rocks all

main = do
    file <- readFile "input\\day_14.txt"
    let rocksSet = parseInput (lines file)
    -- print $ getBottom rocksSet
    -- print $ addSand rocksSet 0 --p1
    print $ addSandStack rocksSet 0 --p2
    -- let finalCave = addSandDebug rocksSet
    -- putStrLn $ displayCave ((485,515), 11) rocksSet finalCave