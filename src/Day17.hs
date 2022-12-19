import Debug.Trace
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

parseDir '<' = -1
parseDir '>' = 1

hRock = [(0, 0), (1, 0), (2, 0), (3, 0)]
xRock = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
lRock = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
iRock = [(0, 0), (0, 1), (0, 2), (0, 3)]
cRock = [(0, 0), (0, 1), (1, 0), (1, 1)]

rocks = [hRock, xRock, lRock, iRock, cRock]

movePointSide amm (x, y) = (x+amm, y)
movePointUp amm (x, y) = (x, y+amm)
inBoundary (x, y) = x >= 0 && x <= 6

fallRock :: (Set (Int, Int), Int) -> [(Int, Int)] -> [Int] -> (Set (Int, Int), Int) 
fallRock (state, step) rock wind
    | any (`Set.member` state) fallenRock = (Set.union (Set.fromList pushedRock) state, step+1)
    | otherwise = fallRock (state, step+1) fallenRock wind
    where windTurn = step `mod` length wind
          currWind = wind!!windTurn  
          pushedRock = moveRockSide rock state currWind
          fallenRock = map (\(x, y) -> (x, y-1)) pushedRock

fallNRocks :: Int -> Int -> [Int] -> (Set (Int, Int), Int) -> (Set (Int, Int), Int)
fallNRocks curr n wind state@(s, _)
    | curr == n = state
    | otherwise = fallNRocks (curr+1) n wind updatedState
        where rockTurn = curr `mod` length rocks
              updatedState = fallRock state (spawnRock (rocks!!rockTurn) s) wind

fallNRocksCicle :: Int -> Int -> [Int] -> (Set (Int, Int), Int) -> (Set (Int, Int), Int)
fallNRocksCicle curr n wind state@(s, _)
    | curr == n = state
    | otherwise = fallNRocks (curr+1) n wind updatedState
        where rockTurn = curr `mod` length rocks
              updatedState = fallRock state (spawnRock (rocks!!rockTurn) s) wind

moveRockSide :: [(Int, Int)] -> Set (Int, Int) -> Int -> [(Int, Int)]
moveRockSide rock state windDir
    | all (\x -> x `Set.notMember` state && inBoundary x) newRock = newRock
    | otherwise = rock
        where newRock = map (movePointSide windDir) rock

spawnRock :: [(Int, Int)] -> Set (Int, Int) -> [(Int, Int)]
spawnRock toSpawn state = map (movePointUp top . movePointSide right) toSpawn
    where (top, right) = (talest state + 4, 2)

talest :: Set (Int, Int) -> Int
talest state = maximum (Set.map snd state)

makeStringRocks :: Int -> Int -> Set (Int, Int) -> String
makeStringRocks width height state = do
    let arrays = zip (repeat (reverse [0 .. width])) [0..height]
    let tuples = reverse (concatMap (\(vals, y) -> map (, y) vals) arrays)
    intercalate "\n" (chunksOf (width+1) (map (`stateToChar` state) tuples))

stateToChar char state 
    | Set.member char state = '#'
    | otherwise = '.'

normalizedLast10 :: Set (Int, Int) -> Set (Int, Int)
normalizedLast10 state = do
    let height = talest state
    let last10 = Set.filter (\x -> snd x > (height-10)) state
    trace (show last10) Set.map (\(x, y) -> (x, y-(height-10))) last10

main = do
    file <- readFile "input\\day_17.txt"
    let dirs = map parseDir file
    let floorState = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)]
    -- let (p1, _) = fallNRocks 0 2022 dirs (floorState, 0)
    -- print (talest p1)

    let (simulate, _) = fallNRocks 0 30 dirs (floorState, 0)
    print simulate
    print $ normalizedLast10 simulate
    -- putStrLn $ makeStringRocks 6 20 simulate




