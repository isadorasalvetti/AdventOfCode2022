import Debug.Trace
import Data.List 

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


parseLine :: [Char] -> Int -> Int -> Map (Int, Int) String
parseLine [] _ _ = Map.empty
parseLine (char:rst) row col
    | char == '.' = continue
    | otherwise = Map.union (Map.fromList [((row, col), [char])]) continue
    where continue = parseLine rst (row+1) col

parseLines [] _ = Map.empty
parseLines (line:rst) col = Map.union (parseLine line 0 col) (parseLines rst (col+1))

viablePaths (x, y) mountain = filter (isViable mountain) paths
    where paths = [(x,y), (x+1, y), (x-1, y), (x, y+1), (x, y-1)]

isViable :: Map (Int, Int) String -> (Int, Int) -> Bool
isViable mountain point = Map.notMember point mountain

-- updateMountain ((pos, stuff):rst) = map (\x -> (moveBlizzard pos x, [x])) stuff
updateMountain :: Map (Int, Int) String -> Int -> Int -> Map (Int, Int) String
updateMountain mp sizeH sizeV = Map.foldrWithKey (\pos stuff acc -> Map.unionWith (++) (updateSpot pos stuff sizeH sizeV) acc) Map.empty mp

updateSpot :: (Int, Int) -> [Char] -> Int -> Int -> Map (Int, Int) String
updateSpot pos stuff sizeH sizeV = Map.fromList (map (\b -> (moveBlizzard pos b sizeH sizeV, [b])) stuff)

moveBlizzard :: (Int, Int) -> Char -> Int -> Int -> (Int, Int)
moveBlizzard (x, y) '>' sizeH sizeV = (warp 1 sizeH (x+1), y)
moveBlizzard (x, y) '<' sizeH sizeV = (warp 1 sizeH (x-1), y)
moveBlizzard (x, y) '^' sizeH sizeV = (x, warp 1 sizeV (y-1))
moveBlizzard (x, y) 'v' sizeH sizeV = (x, warp 1 sizeV (y+1))
moveBlizzard (x, y) '#' _ _ = (x, y)

warp :: Int -> Int -> Int -> Int
warp mn mx pos
    | pos < mn = mx 
    | pos > mx = mn
    | otherwise = pos

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x, y) (tx, ty) = (tx-x) + (ty-y)

sortMinSpot (a1, b1) (a2, b2) end
  | a1 == a2 = compare (dist b1 end) (dist b2 end)
  | otherwise = compare a1 a2

findPath :: [Map (Int, Int) String] -> (Int, Int) -> Int -> Int -> Set (Int, (Int, Int)) -> (Int, [Map (Int, Int) String])
findPath mountains end sizeH sizeV pathHeads
    | current == end = (minute, mountains)
    | minute `mod` 50 == 0 = continue
    | otherwise = continue
    where nextMountains
            | (minute+1) < length mountains = mountains
            | otherwise = mountains ++ [updateMountain (mountains!!minute) sizeH sizeV]
          curr@(minute, current) = Set.findMin pathHeads
          newToExplore = viablePaths current (nextMountains!!(minute+1))
          toExplore = Set.fromList $ map (minute+1,) newToExplore
          allToExplore = Set.union toExplore (Set.delete curr pathHeads)
          continue = findPath nextMountains end sizeH sizeV allToExplore

main = do
    file <- readFile "input\\day_24.txt"
    let sizeLine = length $ head $ lines file
    let sizeCol = length $ lines file
    let mp = Map.insert (sizeLine-2, sizeCol) "#" $ Map.insert (1, -1) "#" (parseLines (lines file) 0)
    let startPos = (1, 0)
    let endPos = (sizeLine -2 , sizeCol - 1)
    let sizeH = sizeLine - 2
    let sizeV = sizeCol - 2
    
    let (timeToFinish, finishState) = findPath [mp] endPos sizeH sizeV (Set.singleton (0, startPos))
    print timeToFinish --p1

    let (timeToBack, backState) = findPath finishState startPos sizeH sizeV (Set.singleton (timeToFinish, endPos))
    print (timeToBack - timeToFinish)

    let (timeToFinish2, _) = findPath backState endPos sizeH sizeV (Set.singleton (timeToBack, startPos))
    print timeToFinish2 --p2




