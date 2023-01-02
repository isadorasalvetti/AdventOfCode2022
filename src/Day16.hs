import Data.Maybe
import Data.Char
import Data.List
import Debug.Trace
import Data.Function

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


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

getToNextValve :: Map String (Int, [String]) -> Map (String, String) Int -> (Int, Set String, String, Int, Int) -> String -> (Int, Set String, String, Int, Int)
getToNextValve allCaves dists (pressure, valveCandidates, currName, time, valvesOpen) nextValve
    | time+timeToNextValve > 30 = (pressure, Set.empty, nextValve, time, valvesOpen)
    | Set.size remainingValves == 0 = (pressure+addedPressure, Set.empty, nextValve, time+timeToNextValve, valveOpened+valvesOpen)
    | otherwise = (pressure+addedPressure, remainingValves, nextValve, time+timeToNextValve, valveOpened+valvesOpen)
    where timeToNextValve = 1 + dists Map.! (currName, nextValve) --  time to walk to valve + open valve
          remainingValves = Set.delete nextValve valveCandidates
          valveOpened = nextValve `valvePressure` allCaves
          addedPressure = valveOpened * (30-(time+timeToNextValve))
          debug = (pressure, currName, nextValve, valvesOpen, time, timeToNextValve)

findMaxValvePaths :: Set (Int, Set String, String, Int, Int) -> Map String (Int, [String]) -> Map (String, String) Int -> Int -> Int
findMaxValvePaths toExplore allCaves dists best
    | Set.size nextStates > 0 = findMaxValvePaths continue allCaves dists currBest
    | otherwise = currBest
    where completed = Set.filter (\(_, valvesLeft, _, _, _) -> Set.size valvesLeft == 0) toExplore
          nextStates = Set.difference toExplore completed
          (bestCandidate, _, _, _, _) = Set.findMax completed
          statesCandidate = Set.foldr (\st@(_, valvesLeft, _, _, _) acc -> Set.union (zipCandidates valvesLeft st) acc) Set.empty nextStates
          continue = Set.map (uncurry (getToNextValve allCaves dists)) statesCandidate
          currBest
            | Set.size completed > 0 = max bestCandidate best
            | otherwise = best

zipCandidates :: Set String -> (Int, Set String, String, Int, Int) -> Set ((Int, Set String, String, Int, Int), String)
zipCandidates candidates st = Set.map (st, ) candidates

valvePressure :: String -> Map String (Int, [String]) -> Int
valvePressure valve allCaves = pressure
    where (pressure, _) = allCaves Map.! valve

main = do
    file <- readFile "input\\day_16.txt"
    let allCaves = Map.fromList (map parseInput (lines file))
    let relevantValves = Map.filter (\val -> fst val > 0) allCaves
    let dists = precomputeDists ("AA":Map.keys relevantValves) allCaves

    print $ findMaxValvePaths (Set.singleton (0, Set.fromList (Map.keys relevantValves), "AA", 0, 0)) allCaves dists 0 -- p1
