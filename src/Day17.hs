import Debug.Trace
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

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
fallRock (state, currWindTurn) rock wind
    | any (`Set.member` state) fallenRock = (Set.union (Set.fromList pushedRock) state, windTurn+1)
    | otherwise = fallRock (state, windTurn+1) fallenRock wind
    where windTurn = currWindTurn `mod` length wind
          currWind = wind!!windTurn  
          pushedRock = moveRockSide rock state currWind
          fallenRock = map (\(x, y) -> (x, y-1)) pushedRock

fallNRocks :: Int -> Int -> [Int] -> (Set (Int, Int), Int) -> (Set (Int, Int), Int)
fallNRocks curr n wind state@(s, _)
    | curr >= n = state
    | otherwise = fallNRocks (curr+1) n wind updatedState
        where rockTurn = curr `mod` length rocks
              updatedState = fallRock state (spawnRock (rocks!!rockTurn) s) wind

fallRocksCicle :: Int -> [Int] -> (Set (Int, Int), Int) -> Map (Set (Int, Int), Int, Int) (Int, Int) -> ((Int, Int), (Int, Int), (Set (Int, Int), Int, Int))
fallRocksCicle curr wind state@(s, windStep) previousStates
    -- | curr > 5000 = ((curr, -1), (-1, -1), state)
    -- | curr == 26 = trace(makeStringRocks 6 25 (normalizedLast10 uState)) fallRocksCicle (curr+1) wind updatedState (Map.insert normalizedState (curr, talest s) previousStates)
    -- | curr == 61 = trace(makeStringRocks 6 25 (normalizedLast10 uState)) fallRocksCicle (curr+1) wind updatedState (Map.insert normalizedState (curr, talest s) previousStates)
    
    | Map.member normalizedState previousStates = ((curr, talest s), previousStates Map.!normalizedState, normalizedState)
    | otherwise = fallRocksCicle (curr+1) wind updatedState (Map.insert normalizedState (curr, talest s) previousStates)
        where rockTurn = curr `mod` length rocks
              updatedState@(uState, endWind) = fallRock state (spawnRock (rocks!!rockTurn) s) wind
              normalizedState = (normalizedLast10 uState, endWind, rockTurn+1)

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
    let arrays = zip (repeat (reverse [0 .. width])) [0 .. height]
    let tuples = reverse (concatMap (\(vals, y) -> map (, y) vals) arrays)
    intercalate "\n" (chunksOf (width+1) (map (`stateToChar` state) tuples))

stateToChar char state 
    | Set.member char state = '#'
    | otherwise = '.'

normalizedLast10 :: Set (Int, Int) -> Set (Int, Int)
normalizedLast10 state = do
    let height = talest state
    let last10 = Set.filter (\(_, y) -> y > height-20) state
    Set.map (\(x, y) -> (x, y-(height-20))) last10

rebuildLast10 :: Set (Int, Int) -> Int -> Set (Int, Int)
rebuildLast10 state talness = Set.map (\(x, y) -> (x, y+(talness-20))) state

main = do
    file <- readFile "input\\day_17.txt"
    let dirs = map parseDir file
    let floorState = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)]
    let (p1, _) = fallNRocks 0 2022 dirs (floorState, 0)
    -- print (talest p1)

    let ((cicleEnd, lastHeight), (cicleStart, startHeight), (lastState, nextWind, nextRock)) = fallRocksCicle 0 dirs (floorState, 0) Map.empty
    print (cicleStart, cicleEnd, nextWind, nextRock)
    let cicleLength = cicleEnd - cicleStart
    let cicleHeight = lastHeight - startHeight
    let toRepeat = 1000000000000 - cicleStart
    let (repetitions, toSimulate) = quotRem toRepeat cicleLength
    let (p2, _)  = fallNRocks nextRock (nextRock+toSimulate+1) dirs (rebuildLast10 lastState (repetitions*cicleHeight + startHeight), nextWind)
    print $ talest p2 -- Right answer on input, but not on sample?...

    -- putStrLn $ makeStringRocks 6 25 (fst $ fallNRocks 0 15 dirs (floorState, 0))
    -- print "-----"
    -- putStrLn $ makeStringRocks 6 25 (normalizedLast10 $ fst $ fallNRocks 0 15 dirs (floorState, 0))



