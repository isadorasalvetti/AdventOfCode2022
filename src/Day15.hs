{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char
import Data.Maybe
import Control.Concurrent.STM (check)

parsePosition :: String -> (Int, Int)
parsePosition str = (readNum xStr, readNum yStr)
    where (xStr, yStr) = (extractVar 'x' str, extractVar 'y' str)

readNum ('-':num) = - read num
readNum num = read num

extractVar var string = takeWhile (\x -> isDigit x || x == '-') $ drop 2 (dropWhile (/= var) string)

parseSensorBeacons :: [String] -> [((Int, Int), (Int, Int))]
parseSensorBeacons [] = []
parseSensorBeacons (line:rst) = (parsePosition sensorLine, parsePosition beaconLine):parseSensorBeacons rst
    where (sensorLine, beaconLine) = break (==':') line

minMaxX :: [(a, (Int, Int))] -> (Int, Int)
minMaxX [] = (999999999, -999999999)
minMaxX ((s, (bx, by)):rst) = (min bx minX, max bx maxX)
    where (minX, maxX) = minMaxX rst

beaconDistaceSensor :: (Int, Int) -> (Int, Int) -> Int
beaconDistaceSensor (sx, sy) (bx, by) = abs (sx-bx) + abs (sy-by)

pointsAtDistance :: (Int, Int) -> Int -> Map Int (Int, Int)
pointsAtDistance _ 0 = Map.empty
pointsAtDistance (sx, sy) d = Map.fromList pointsDist
    where lines = zip [(sy-d)..(sy+d)] ([0..d] ++ reverse [0..d-1])
          culledLines = filter (\(y, _) -> y > minY && y < maxY) lines
          pointsDist = map (\(line, d) -> (line, (sx-d, sx+d))) culledLines

beaconsAt y [] = Set.empty
beaconsAt y ((_,(bx, by)):rst)
    | by == y = Set.insert (bx,by) (beaconsAt y rst)
    | otherwise = beaconsAt y rst

eliminatedPositions sensor beacon = pointsAtDistance sensor dist
    where dist = beaconDistaceSensor sensor beacon

noBeaconsLine :: Int -> [((Int, Int), (Int, Int))] -> [(Int, Int)]
noBeaconsLine _ [] = []
noBeaconsLine yLine ((sensor, beacon):rst)
    | yLine `Map.member` eliminatedLines = join (fromJust $ Map.lookup yLine eliminatedLines) (noBeaconsLine yLine rst)
    | otherwise = noBeaconsLine yLine rst
    where eliminatedLines = eliminatedPositions sensor beacon

join :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
join new [] = [new]
join new@(n0, n1) state@(curr@(b0, b1):bNext)
    | n1 < b0-1 = new:state
    | b1 < n0-1 = curr:join new bNext
    | otherwise = join (lowBound, highBound) bNext
    where lowBound = min n0 b0
          highBound = max n1 b1

joinAll :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
joinAll [] b = b
joinAll (a:rst) b = joinAll rst (join a b)

allTakenPos :: [((Int, Int), (Int, Int))] -> Map Int [(Int, Int)]
allTakenPos [] = Map.empty
allTakenPos ((sensor, beacon):rst) = Map.unionWith joinAll (allTakenPos rst) (Map.map (: []) (eliminatedPositions sensor beacon))
    where eliminatedLines = eliminatedPositions sensor beacon

(minY, maxY) = (0, 4000000)
checkLine = 2000000
-- checkLine = 10

main = do
    file <- readFile "input\\day_15.txt"
    let sensorBeacons = parseSensorBeacons (lines file)
    let beaconsInLine = noBeaconsLine checkLine sensorBeacons
    let allTakenPosRes = allTakenPos sensorBeacons

    print (snd (head beaconsInLine) - fst (head beaconsInLine)) -- P1 4883971
    print $ Map.filter (\x -> length x > 1) allTakenPosRes -- P2 [(2767556,[(-412699,3172755),(3172757,4437053)])]
    -- P2 algebra, cause lazy: 2767556 + (4000000 * 3172756) = 12691026767556

