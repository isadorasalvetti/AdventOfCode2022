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

warper (-1, y) (-1, 0)
    | y >= 100 && y < 150 = ((50, abs (y-149)), (1, 0)) -- A
    | y >= 150 && y < 200 = ((y-100, 0), (0, 1)) -- B
warper (49, y) (-1, 0)
    | y >= 0 && y < 50 = ((0, abs (y-49) + 100), (1, 0)) -- C
    | y >= 50 && y < 100 = ((y-50, 100), (0, 1)) -- D
warper (100, y) (1, 0)
    | y >= 50 && y < 100 = ((y+50, 49), (0, -1)) -- D
    | y >= 100 && y < 150 = ((149, abs (y-149)), (-1, 0)) -- E
warper (150, y) (1, 0)
    | y >= 0 && y < 50 = ((99, abs (y-49) + 100), (-1, 0)) -- F
warper (50, y) (1, 0)
    | y >=150 && y < 200 = ((y-100, 149), (0, -1))

warper (x, 99) (0, -1)
    | x >= 0 && x < 50 = ((50, x+50), (1, 0)) -- A
warper (x, 200) (0, 1)
    | x >= 0 && x < 50 = ((x+100, 0), (0, 1)) -- B
warper (x, -1) (0, -1)
    | x >= 50 && x < 100 = ((0, x+100), (1, 0)) -- C
    | x >= 100 && x < 150 = ((x-100, 199), (0, -1)) -- F
warper (x, 50) (0, 1)
    | x >= 100 && x < 150 = ((99, x-50), (0, -1)) -- F
warper (x, 150) (0, 1)
    | x >= 50 && x < 100 = ((49, x+100), (-1, 0)) -- E
warper pos dir = error $ "Bad wrapping" ++ show (pos, dir)
    
parseLines [] _ = Map.empty
parseLines (line:rst) col = Map.union (parseLine line 0 col) (parseLines rst (col+1))

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

moveNextSpot :: Map (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> Int -> ((Int, Int), (Int, Int))
moveNextSpot _ pos dir 0 = (pos, dir)
moveNextSpot mp pos@(row, col) dir@(x, y) amm
    | posExist && not (mp `mm` expectedPos) = (pos, dir)
    | not posExist && not (mp `mm` wrappedPos) = (pos, dir)
    | posExist = moveNextSpot mp expectedPos (x, y) (amm-1)
    | otherwise = trace ("Warp" ++ show (wrappedPos, pos, wrappedDir, dir)) moveNextSpot mp wrappedPos wrappedDir (amm-1)
    
    where posExist = Map.member expectedPos mp
          expectedPos = (row+x, col+y)
          (wrappedPos, wrappedDir) = warper expectedPos dir

playMoves :: Map (Int, Int) Bool -> [(Int, Char)] -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
playMoves mp [] currDir currPos = (currPos, currDir)
playMoves mp ((amm, dir):dirRst) currDir currPos = trace (show (currPos, dir, amm)) (nextMove, nextDir)
    where (nextPos, midDir) = moveNextSpot mp currPos currDir amm
          moveDir = rotate midDir dir
          (nextMove, nextDir) = playMoves mp dirRst moveDir nextPos

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

    let mp = parseLines board 0
    let dir = parseDirections (last directions)
    let ((finalRow, finalCol), finalDir) = playMoves mp dir (1, 0) (50, 0)

    let p2 = ((finalCol+1)*1000) + ((finalRow+1)*4) + dirToVal finalDir
    print p2

