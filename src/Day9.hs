import Data.Set (Set)
import qualified Data.Set as Set

parseMoves :: [String] -> [(Char, Int)]
parseMoves [] = []
parseMoves (curr:rst) = do
    let dir = head curr
    let amm = read $ drop 2 curr
    [(dir, amm)] ++ parseMoves rst

moveHead :: (Int, Int) -> Char -> (Int, Int)
moveHead (xCurr, yCurr) dir
    | dir == 'L' = (xCurr-1, yCurr)
    | dir == 'R' = (xCurr+1, yCurr)
    | dir == 'U' = (xCurr, yCurr+1)
    | otherwise = (xCurr, yCurr-1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (xHead, yHead) (xCurr, yCurr)
    | abs (xCurr-xHead) <= 1 && abs (yCurr-yHead) <= 1 = (xCurr, yCurr)
    | otherwise = do 
        let xDir = safeDiv (xHead-xCurr)
        let yDir = safeDiv (yHead-yCurr)
        ((xCurr + xDir), (yCurr + yDir))

safeDiv x
    | x == 0 = 0
    | otherwise = x `div` abs x

doMove :: (Char, Int) -> (((Int, Int), (Int, Int)), Set (Int, Int)) -> (((Int, Int), (Int, Int)), Set (Int, Int))
doMove (dir, 0) state = state
doMove (dir, amm) ((currHead, currTail), tracker) = do
    let newHead = moveHead currHead dir
    let newTail = moveTail newHead currTail
    doMove (dir, amm-1) ((newHead, newTail), (Set.insert newTail tracker))

doMoves :: [(Char, Int)] -> (((Int, Int), (Int, Int)), Set (Int, Int)) -> (((Int, Int), (Int, Int)), Set (Int, Int))
doMoves [] state = state 
doMoves (mv:rst) state = do
    let thisMoveResult = doMove mv state
    mergeStates (doMoves rst thisMoveResult) thisMoveResult

moveTail2 :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
moveTail2 (xHead, yHead) [] = []
moveTail2 (xHead, yHead) ((xCurr, yCurr):rst)
    | abs (xCurr-xHead) <= 1 && abs (yCurr-yHead) <= 1 = [(xCurr, yCurr)] ++ moveTail2 (xCurr, yCurr) rst
    | otherwise = do
        let newCurrTail = ((xCurr + safeDiv (xHead-xCurr)), (yCurr + safeDiv (yHead-yCurr)))
        [newCurrTail]  ++ moveTail2 newCurrTail rst


doMove2 :: (Char, Int) -> (((Int, Int), [(Int, Int)]), Set (Int, Int)) -> (((Int, Int), [(Int, Int)]), Set (Int, Int))
doMove2 (dir, 0) state = state
doMove2 (dir, amm) ((currHead, currTail), tracker) = do
    let newHead = moveHead currHead dir
    let newTail = moveTail2 newHead currTail
    doMove2 (dir, amm-1) ((newHead, newTail), (Set.insert (last newTail) tracker))


doMoves2 :: [(Char, Int)] -> (((Int, Int), [(Int, Int)]), Set (Int, Int)) -> (((Int, Int), [(Int, Int)]), Set (Int, Int))
doMoves2 [] state = state 
doMoves2 (mv:rst) state = do
    let thisMoveResult = doMove2 mv state
    mergeStates (doMoves2 rst thisMoveResult) thisMoveResult

mergeStates a b = (fst a, Set.union (snd a) (snd b)) 

p1 lines = do
    let moves = parseMoves lines
    let pos = doMoves moves (((0, 0), (0, 0)), Set.empty)
    length (snd pos)

p2 lines = do
    let moves = parseMoves lines
    let tailRope = take 9 $ repeat (0, 0)
    let pos = doMoves2 moves (((0, 0), tailRope), Set.empty)
    length (snd pos)

main = do
    file <- readFile "input\\day_9.txt"
    print $ p2 $ lines file