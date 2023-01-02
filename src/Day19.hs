import Data.Char
import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set

data Inventory = I Int Int Int Int deriving (Eq, Ord, Show)
data Bots = B Int Int Int Int deriving (Eq, Ord, Show)

g (I g o c b) = g

-- ore robot costs [0] ore,  clay robot costs [1] ore,  obsidian robot costs [2] ore, [3] clay, geode robot costs [4] ore, [5] obsidian
parseLine :: String -> [Int]
parseLine str = nums
    where wds = words str
          nums = map read $ filter (all isDigit) wds

findBestGeodes :: [Int] -> [Int] -> Int -> Set ((Inventory, Bots), Int) -> Int
findBestGeodes bp maxCosts currBest states
    | Set.size states > 0 = max currBest continue
    | otherwise = currBest
    where curr@(currState@(currInv, currBots), currTime) = Set.findMax states
          nextStates = Set.unions $ map (Set.singleton . buildNextBot bp currState currTime) (filter (worthBuilding maxCosts currInv currTime) ['g', 'o', 'c', 'b'])
          relevantStates = Set.filter (uncurry (filterStates currBest)) nextStates
          (toExplore, completed) = Set.partition (\(_, timeLeft) -> timeLeft > 0) relevantStates
          bestGeodes
            | Set.size completed > 0 = g $ fst $ fst $ Set.findMax completed
            | otherwise = currBest
          continue = findBestGeodes bp maxCosts bestGeodes (Set.union (Set.delete curr states) toExplore)
          debug = (currBest, curr)

filterStates currBest state@(I g o c b, B gb ob cb bb) time = g + (gb+(time `div` 2))*time > currBest

worthBuilding maxCosts (I g o c b) time 'g' = True
worthBuilding maxCosts (I g o c b) time 'o' = o <= maxCosts!!0 + 1
worthBuilding maxCosts (I g o c b) time 'c' = o <= maxCosts!!1 + 1
worthBuilding maxCosts (I g o c b) time 'b' = o <= maxCosts!!2 + 1

buildNextBot :: [Int] -> (Inventory, Bots) -> Int -> Char -> ((Inventory, Bots), Int)
buildNextBot bp state@(currInv, currBots) time newBot
    | timeToBot > time = ((advanceTime time currInv currBots, currBots), 0)
    | otherwise = ((newInventoryState, newBotState), time-timeToBot)
    where timeToBot = botRequirement bp newBot state
          newInventoryState = removeMats bp newBot (advanceTime timeToBot currInv currBots)
          newBotState = addBot newBot currBots
          debug = (state, (newInventoryState, newBotState), newBot, time, time-timeToBot)

roundUpFloatDiv :: Int -> Int -> Int
roundUpFloatDiv a b = ceiling (fromIntegral a / fromIntegral b) + 1

advanceTime :: Int -> Inventory -> Bots -> Inventory
advanceTime t (I g o c b) (B gb ob cb bb) = I (g+(gb*t)) (o+(ob*t)) (c+(cb*t)) (b+(bb*t))

removeMats :: [Int] -> Char -> Inventory -> Inventory
removeMats bp 'o' (I g o c b) = I g (o-bp!!0) c b
removeMats bp 'c' (I g o c b) = I g (o-bp!!1) c b
removeMats bp 'b' (I g o c b) = I g (o-bp!!2) (c-bp!!3) b
removeMats bp 'g' (I g o c b) = I g (o-bp!!4) c (b-bp!!5)

botRequirement :: [Int] -> Char -> (Inventory, Bots) -> Int
botRequirement bp 'o' (I g o c b, B gb ob cb bb) = oNeeded `roundUpFloatDiv` ob
    where oNeeded = max (bp!!0 - o) 0
botRequirement bp 'c' (I g o c b, B gb ob cb bb) = oNeeded `roundUpFloatDiv` ob
    where oNeeded = max (bp!!1 - o) 0
botRequirement bp 'b' (I g o c b, B gb ob cb bb)
    | cb == 0 = maxBound
    | otherwise = max (oNeeded `roundUpFloatDiv` ob) (cNeeded `roundUpFloatDiv` cb)
    where oNeeded = max (bp!!2 - o) 0
          cNeeded = max (bp!!3 - c) 0
botRequirement bp 'g' (I g o c b, B gb ob cb bb)
    | bb == 0 = maxBound
    | otherwise = max (oNeeded `roundUpFloatDiv` ob) (bNeeded `roundUpFloatDiv` bb)
    where oNeeded = max (bp!!4 - o) 0
          bNeeded = max (bp!!5 - b) 0

addBot 'o' (B g o c b) = B g (o+1) c b
addBot 'c' (B g o c b) = B g o (c+1) b
addBot 'b' (B g o c b) = B g o c (b+1)
addBot 'g' (B g o c b) = B (g+1) o c b

maxCosts bp = [maximum [bp!!0, bp!!1, bp!!2, bp!!4], bp!!3, bp!!5]

main = do
    file <- readFile "input\\day_19.txt"
    let bps = map parseLine (lines file)
    let startingState = Set.singleton ((I 0 0 0 0, B 0 1 0 0), 24)
    let maxGeodes24 = map (\bp -> findBestGeodes bp (maxCosts bp) 0 startingState) bps
    -- print $ sum $ map (uncurry (*)) (zip maxGeodes24 [1..]) -- p1

    let startingState2 = Set.singleton ((I 0 0 0 0, B 0 1 0 0), 32)
    let maxGeodes32 = map (\bp -> findBestGeodes bp (maxCosts bp) 0 startingState2) (take 3 bps) -- p2
    print $ maxGeodes32
    print $ product maxGeodes32


