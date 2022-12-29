import Data.Maybe
import Data.Char
import Data.List
import Debug.Trace
import Data.Function

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (prec)


parseValve ('V':'a':'l':'v':'e':' ':a:b:rst) = (a:[b], rst)
parseFlowRate :: String -> Int
parseFlowRate str = read (takeWhile isDigit (dropWhile (not . isDigit) str))
parseTunnels str = words (dropWhile (\a -> isLower a|| isSpace a) (tail (dropWhile (/= ';') str)))

parseInput :: String -> (String, (Int, [String]))
parseInput str = (valveName, (valveFlow, valveConnections))
    where
        (valveName, valveNameRst) = parseValve str
        valveFlow = parseFlowRate valveNameRst
        valveConnections = parseTunnels valveNameRst

findPath :: String -> Map String (Int, [String]) -> Set ((Int, String), [String]) -> [String]
findPath target graph candidates
    | target `elem` nextNodes = currPos:explored
    | currPos `elem` explored = findPath target graph toVisit
    | otherwise = findPath target graph (Set.union toVisit newOptions)
    where
        (currPosSet, toVisit) = Set.splitAt 1 candidates
        ((currCost, currPos), explored) = head (Set.toList currPosSet)
        (_, nextNodes) = unwrapLookup currPos graph
        nextToExplore = zip (repeat (currCost+1)) (filter (`notElem` explored) nextNodes)
        newOptions = Set.union toVisit (Set.fromList (zip  nextToExplore (repeat (currPos:explored))))

unwrapLookup :: String -> Map String (Int, [String]) -> (Int, [String])
unwrapLookup key map = fromJust (Map.lookup key map)

precomputeDists :: [String] -> Map String (Int, [String]) -> Map (String, String) Int
precomputeDists [] allCaves = Map.empty
precomputeDists (a:rst) allCaves = Map.union (precomputeDists rst allCaves) pathsMap
    where combinations = map (, a) rst ++ map (a, ) rst
          paths = map (\(a, b) -> length $ findPath b allCaves (Set.singleton ((0, a), []))) combinations
          pathsMap = Map.fromList $ zip combinations paths

getToNextValve :: Map String (Int, [String]) -> Map (String, String) Int -> (Int, Set String, String, Int) -> String -> (Int, Set String, String, Int)
getToNextValve allCaves dists (pressure, valveCandidates, currName, time) nextValve
    | time+timeToNextValve > 26 = (pressure, remainingValves, nextValve, 26)
    | Set.size remainingValves == 0 = (pressure+addedPressure, Set.empty, nextValve, 26)
    | otherwise = (pressure+addedPressure, remainingValves, nextValve, time+timeToNextValve)
    where timeToNextValve = 1 + dists Map.! (currName, nextValve) --  time to walk to valve + open valve
          remainingValves = Set.delete nextValve valveCandidates
          valveOpened = nextValve `valvePressure` allCaves
          addedPressure = valveOpened * (26-(time+timeToNextValve))
          debug = (pressure, currName, nextValve, time, timeToNextValve)

findMaxValvePaths :: Set (Int, Set String, String, Int) -> Map String (Int, [String]) -> Map (String, String) Int -> (Int, Set String) -> (Int, Set String)
findMaxValvePaths toExplore allCaves dists best
    | Set.size nextStates > 0 = findMaxValvePaths continue allCaves dists currBest
    | otherwise = currBest
    where completed = Set.filter (\(_, valvesLeft, _, time) -> time >= 26) toExplore
          nextStates = Set.difference toExplore completed
          (bestCandidate, bestValvesLeft, _, _) = Set.findMax completed
          statesCandidate = Set.foldr (\st@(_, valvesLeft, _, _) acc -> Set.union (zipCandidates valvesLeft st) acc) Set.empty nextStates
          continue = Set.map (uncurry (getToNextValve allCaves dists)) statesCandidate
          currBest
            | Set.size completed > 0 && bestCandidate > fst best = (bestCandidate, bestValvesLeft)
            | otherwise = best

zipCandidates :: Set String -> (Int, Set String, String, Int) -> Set ((Int, Set String, String, Int), String)
zipCandidates candidates st = Set.map (st, ) candidates

valvePressure :: String -> Map String (Int, [String]) -> Int
valvePressure valve allCaves = pressure
    where (pressure, _) = allCaves Map.! valve

main = do
    file <- readFile "input\\day_16.txt"
    let allCaves = Map.fromList (map parseInput (lines file))
    let relevantValves = Map.filter (\val -> fst val > 0) allCaves
    let dists = precomputeDists ("AA":Map.keys relevantValves) allCaves

    let (pressure1, valvesLeft1) = findMaxValvePaths (Set.singleton (0, Set.fromList (Map.keys relevantValves), "AA", 0)) allCaves dists (0, Set.empty)
    let (pressure2, valvesLeft2) = findMaxValvePaths (Set.singleton (0, valvesLeft1, "AA", 0)) allCaves dists (0, Set.empty)
    print ((pressure1, valvesLeft1), (pressure2, valvesLeft2), pressure1+pressure2)

    --print dists
    --print $ findPath "OM" allCaves (Set.singleton ((0, "VR"), []))
    --print relevantValves

    -- let step1 = getToNextValve allCaves dists (0, Set.fromList $ Map.keys relevantValves, "AA", 0, 0) "OM"
    -- let step2 = getToNextValve allCaves dists step1 "VR"
    -- let step3 = getToNextValve allCaves dists step2 "SP"
    -- let step4 = getToNextValve allCaves dists step3 "RO"
    -- let step5 = getToNextValve allCaves dists step4 "KZ"
    -- let step6 = getToNextValve allCaves dists step5 "DI"
    -- let step7 = getToNextValve allCaves dists step6 "SO"
    -- let step8 = getToNextValve allCaves dists step7 "SC"
    -- let step9 = getToNextValve allCaves dists step8 "AJ"


    -- let step1 = getToNextValve allCaves dists (0, Set.fromList $ Map.keys relevantValves, "AA", 0, 0) "DD"
    -- let step2 = getToNextValve allCaves dists step1 "BB"
    -- let step3 = getToNextValve allCaves dists step2 "JJ"
    -- let step4 = getToNextValve allCaves dists step3 "HH"
    -- let step5 = getToNextValve allCaves dists step4 "EE"
    -- let step6 = getToNextValve allCaves dists step5 "CC"
    --let step7 = getToNextValve allCaves dists step6 "SO"
    --let step8 = getToNextValve allCaves dists step7 "SC"
    --let step9 = getToNextValve allCaves dists step8 "AJ"

    -- print step1
    -- print step2
    -- print step3
    -- print step4
    -- print step5
    -- print step6
    -- print step7
    -- print step8
    -- print step9