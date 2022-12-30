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

findAllPaths :: Set (Int, Set String, String, Int) -> Map String (Int, [String]) -> Map (String, String) Int -> Map (Set String) Int
findAllPaths toExplore allCaves dists
    | Set.size nextStates > 0 = Map.union (findAllPaths continue allCaves dists) toKeep
    | otherwise = toKeep
    where toKeep = Map.unionsWith max (map (\(pressure, valvesLeft, _, _) -> Map.singleton valvesLeft pressure) (Set.toList toExplore))
          completed = Set.filter (\(_, valvesLeft, _, time) -> time >= 26) toExplore
          nextStates = Set.difference toExplore completed
          (bestCandidate, bestValvesLeft, _, _) = Set.findMax completed
          statesCandidate = Set.foldr (\st@(_, valvesLeft, _, _) acc -> Set.union (zipCandidates valvesLeft st) acc) Set.empty nextStates
          continue = Set.map (uncurry (getToNextValve allCaves dists)) statesCandidate

zipCandidates :: Set String -> (Int, Set String, String, Int) -> Set ((Int, Set String, String, Int), String)
zipCandidates candidates st = Set.map (st, ) candidates

valvePressure :: String -> Map String (Int, [String]) -> Int
valvePressure valve allCaves = pressure
    where (pressure, _) = allCaves Map.! valve

pairStates :: Map (Set String) Int -> Set String -> Int -> Int
pairStates foundStates relevantCaves currBest
    | Map.null foundStates = currBest
    | otherwise = max newBest continue
    where ((toTest, a), continueMap) = fromJust $ Map.maxViewWithKey foundStates
          toTestValvesOpen = Set.difference relevantCaves toTest
          secondOptions = Map.filterWithKey (\k _ -> Set.isSubsetOf toTestValvesOpen k) continueMap
          bestOption = maximum (Map.elems secondOptions)
          newBest
            | Map.null secondOptions = currBest
            | otherwise = max currBest (bestOption + a)
          continue = pairStates continueMap relevantCaves newBest

main = do
    file <- readFile "input\\day_16.txt"
    let allCaves = Map.fromList (map parseInput (lines file))
    let relevantValves = Map.filter (\val -> fst val > 0) allCaves
    let dists = precomputeDists ("AA":Map.keys relevantValves) allCaves

    let paths = findAllPaths (Set.singleton (0, Set.fromList (Map.keys relevantValves), "AA", 0)) allCaves dists
    print $ pairStates paths (Set.fromList (Map.keys relevantValves)) 0 
