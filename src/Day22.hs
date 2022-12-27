import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


parseLine :: [Char] -> Int -> Int -> Map (Int, Int) Bool
parseLine [] _ _ = Map.empty
parseLine (char:rst) row col
    | char == ' ' = continue
    | otherwise = Map.union continue (Map.fromList [((row, col), isEmpty)])
    where continue = parseLine rst (row+1) col
          isEmpty = char == '.'

parseHEdges :: [String] -> Int -> Map ((Int, Int), (Int, Int)) (Int, Int)
parseHEdges [] _  = Map.empty
parseHEdges (line:rst) col = Map.union (Map.fromList [(((1, 0), (rowEnd, col)), (rowStart, col)), (((1, 0), (rowStart, col)), (rowEnd, col))]) continue
    where (lead, rstLine) = break (/=' ') line
          (board, end) =  break (==' ') rstLine
          rowStart = length lead
          rowEnd = length lead + length board + 1
          continue = parseHEdges rst (col+1)

parseVEdges :: [String] -> Int -> Map ((Int, Int), (Int, Int)) (Int, Int)
parseVEdges [] _  = Map.empty
parseVEdges (line:rst) row = Map.union (Map.fromList [(((0, 1), (row, colEnd)), (row, colStart)), (((0, 1), (row, colStart)), (row, colEnd))]) continue
    where (lead, rstLine) = break (/=' ') line
          (board, end) =  break (==' ') rstLine
          colStart = length lead
          colEnd = length lead + length board + 1
          continue = parseVEdges rst (row+1)

parseLines [] _ = Map.empty
parseLines (line:rst) col = Map.union (parseLine line 1 col) (parseLines rst (col+1))

parseDirections :: [Char] -> [(Int, Char)]
parseDirections [] = []
parseDirections string = do
    let (stringN, rst) = span isDigit string
    let (number, dir) = (read stringN, head rst)
    (number, dir):parseDirections (tail rst)

findEdge map = 0

rotate (x, y) dir
    | dir == 'R' = (-y, x)
    | dir == 'L' = (y, -x)
    | otherwise = (x, y)


dirToVal (1, 0) = 0
dirToVal (0, 1) = 1
dirToVal (-1, 0) = 2
dirToVal (0, -1) = 3

absDir (x, y) = (abs x, abs y)

moveNextSpot :: Map (Int, Int) Bool -> Map ((Int, Int), (Int, Int)) (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
moveNextSpot _ _ (row, col) _ 0 = (row, col)
moveNextSpot mp edges (row, col) (x, y) amm
    | posExist && not (mp `mm` expectedPos) = (row, col)
    | not posExist && not (mp `mm` wrappedPos) = (row, col)
    | posExist = moveNextSpot mp edges expectedPos (x, y) (amm-1)
    | otherwise = moveNextSpot mp edges wrappedPos (x, y) (amm-1)
    where posExist = Map.member expectedPos mp
          expectedPos = (row+x, col+y)
          wP = edges `mm` (absDir (x, y), expectedPos)
          wrappedPos = (fst wP + x, snd wP + y)

playMoves :: Map (Int, Int) Bool -> Map ((Int, Int), (Int, Int)) (Int, Int) -> [(Int, Char)] -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
playMoves mp edges [] currDir currPos = (currPos, currDir)
playMoves mp edges ((amm, dir):dirRst) currDir currPos = (nextMove, nextDir)
    where moveDir = rotate currDir dir
          nextPos = moveNextSpot mp edges currPos currDir amm
          (nextMove, nextDir) = playMoves mp edges dirRst moveDir nextPos

makeStringMap :: Int -> Int -> Set (Int, Int) -> String
makeStringMap width height points = do
    let arrays = zip (repeat (reverse [0 .. width])) [0..height]
    let tuples = reverse (concatMap (\(vals, y) -> map (, y) vals) arrays)
    let lines = chunksOf (width+1) (map (`stateToChar` points) tuples)
    intercalate "\n" (zipWith (\ a b -> a ++ show b) lines [0 ..])
stateToChar char state
    | Set.member char state = '#'
    | otherwise = '.'

mm mp index
    | index `Map.notMember` mp = error ("key " ++ show index ++ " not found")
    | otherwise = mp Map.! index

listToSet :: [((Int, Int), (Int, Int))] -> Set (Int, Int)
listToSet lst = Set.fromList (map snd lst)

main = do
    file <- readFile "input\\day_22.txt"
    let (board, directions) = break (=="") (lines file)

    let mp = parseLines board 1
    let vEdges = parseVEdges (transpose board) 1
    let hEdges = parseHEdges board 1
    let edges = Map.union hEdges vEdges
    let dir = parseDirections (last directions)
    let ((finalRow, finalCol), finalDir) = playMoves mp edges dir (1, 0) (54, 1)

    let p1 = (finalCol*1000) + (finalRow*4) + dirToVal finalDir
    print (finalRow, finalCol, finalDir, p1)

