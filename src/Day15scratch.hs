import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char

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

pointsAtDistance :: (Int, Int) -> Int -> [(Int, Int)]
pointsAtDistance _ 0 = []
pointsAtDistance (sx, sy) d = diDist ++ di2Dist ++ posDist ++ negDist ++ pointsAtDistance (sx, sy) (d-1)
    where distPoints = map (\x -> (x, d-x)) [0..d]
          posDist = map (\(x, y) -> (sx+x, sy+y)) distPoints
          negDist = map (\(x, y) -> (sx-x, sy-y)) distPoints
          diDist = map (\(x, y) -> (sx+x, sy-y)) distPoints
          di2Dist = map (\(x, y) -> (sx-x, sy+y)) distPoints

beaconsAt y [] = Set.empty
beaconsAt y ((_,(bx, by)):rst)
    | by == y = Set.insert (bx,by) (beaconsAt y rst)
    | otherwise = beaconsAt y rst

eliminatedPositions sensor beacon = pointsAtDistance sensor dist
    where dist = beaconDistaceSensor sensor beacon

noBeaconsLine :: Int -> [((Int, Int), (Int, Int))] -> Set Int
noBeaconsLine _ [] = Set.empty
noBeaconsLine yLine ((sensor, beacon):rst) = Set.union (Set.fromList (map fst eliminatedLine)) (noBeaconsLine yLine rst)
    where eliminatedLine = filter (\(x, y) -> y == yLine) (eliminatedPositions sensor beacon)

main = do
    file <- readFile "input\\day_15.txt"
    let sensorBeacons = parseSensorBeacons (lines file)
    let beaconsInLine = noBeaconsLine 2000000 sensorBeacons
    let beaconsToRemove = beaconsAt 2000000 sensorBeacons
    print (length beaconsInLine) -- P1
    
