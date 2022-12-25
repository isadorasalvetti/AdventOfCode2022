import Data.List
import Data.List.Split
import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


parseLine :: [Char] -> Int -> Int -> Set (Int, Int)
parseLine [] _ _ = Set.empty
parseLine (char:rst) row col
    | char == '.' = continue
    | otherwise = Set.union continue (Set.fromList [(row, col)])
    where continue = parseLine rst (row+1) col

getNorth (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1)]
getSouth (x, y) = [(x-1, y+1), (x, y+1), (x+1, y+1)]
getEast (x, y) = [(x+1, y-1), (x+1, y), (x+1, y+1)]
getWest (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1)]

proposeMove :: (Int, Int) -> Int -> Set (Int, Int) -> (Bool, (Int, Int))
proposeMove elf@(x, y) turn elves
    | fst $ dirs!!(turn `mod` 4) = dirs!!(turn `mod` 4)
    | fst $ dirs!!((turn+1) `mod` 4) = dirs!!((turn+1) `mod` 4)
    | fst $ dirs!!((turn+2) `mod` 4) = dirs!!((turn+2) `mod` 4)
    | fst $ dirs!!((turn+3) `mod` 4) = dirs!!((turn+3) `mod` 4)
    | otherwise = (False, (x, y))

    where dirs = [(northFree, (x, y-1)), (southFree, (x, y+1)), (westFree, (x-1, y)), (eastFree, (x+1, y))]
          northFree = all (\x -> not (Set.member x elves)) (getNorth elf)
          southFree = all (\x -> not (Set.member x elves)) (getSouth elf)
          westFree = all (\x -> not (Set.member x elves)) (getWest elf)
          eastFree = all (\x -> not (Set.member x elves)) (getEast elf)

isAlone elf@(x, y) elves = all (\x -> not $ x `Set.member` elves) allNeigh
    where allNeigh = delete elf [(x,y) | x <-[x-1, x, x+1], y <-[y-1, y, y+1]]


proposeMoves :: Set (Int, Int) -> Set (Int, Int) -> Int -> Map (Int, Int) (Int, Int) -> Map (Int, Int) (Int, Int)
proposeMoves elves allElves turn otherProposals
    | Set.size elves == 0 = otherProposals
    | isAlone eCurr allElves = proposeMoves eRst allElves turn otherProposals
    | not hasProposal = proposeMoves eRst allElves turn otherProposals
    | proposal `Map.member` otherProposals = proposeMoves eRst allElves turn (Map.delete proposal otherProposals)
    | otherwise = proposeMoves eRst allElves turn (Map.insert proposal eCurr otherProposals) 
    where (eCurrStupid, eRst) = Set.splitAt 1 elves
          eCurr = head (Set.elems eCurrStupid)
          (hasProposal, proposal) = proposeMove eCurr turn allElves

doMoves :: Map (Int, Int) (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
doMoves proposals elves
    | null proposals = Set.empty
    | otherwise = Map.foldrWithKey (\key val acc -> doMove (key, val) acc) elves proposals

doMove (proposal, elf) elves = Set.insert proposal (Set.delete elf elves)

parseLines [] _ = Set.empty
parseLines (line:rst) col = Set.union (parseLine line 1 col) (parseLines rst (col+1))

makeStringMap :: Int -> Int -> Set (Int, Int) -> String
makeStringMap width height points = do
    let arrays = zip (repeat (reverse [0 .. width])) (reverse [0 .. height])
    let tuples = reverse (concatMap (\(vals, y) -> map (, y) vals) arrays)
    let lines = chunksOf (width+1) (map (`stateToChar` points) tuples)
    intercalate "\n" (zipWith (\ a b -> a ++ show b) lines [0 ..])
stateToChar char state
    | Set.member char state = '#'
    | otherwise = '.'

doNMoves elves turn max 
    | turn == max = elves
    | null newElves = trace(show turn) Set.singleton (turn, turn)
    | otherwise = doNMoves newElves (turn+1) max
    where newElves = doMoves (proposeMoves elves elves turn Map.empty) elves
    

boundSet :: Set (Int, Int) -> Int
boundSet s = trace (show (maximum f, minimum f, maximum d, minimum d)) (maximum f - minimum f + 1) * (maximum d - minimum d + 1)
    where f = Set.map fst s
          d = Set.map snd s

main = do
    file <- readFile "input\\day_23.txt"
    let elves = parseLines (lines file) 1
    
    let ftsMove = doMoves (proposeMoves elves elves 0 Map.empty) elves
    let finalState = doNMoves elves 0 10
    print $ boundSet finalState - length finalState -- p1

    let finalState = doNMoves elves 0 1000
    print finalState --p2


