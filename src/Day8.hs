import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- size = 5
size = 99

treeMap :: [String] -> Int -> Int -> Map (Int, Int) Int
treeMap [[]] _ _ = Map.empty
treeMap ([]:rst) x y = treeMap rst 0 (y+1)
treeMap ((item:rstLine):rst) x y = Map.insert (x, y) (read [item]) (treeMap (rstLine:rst) (x+1) y)

visibleTrees :: Int -> Int -> Map (Int, Int) Int -> Int
visibleTrees 97 97 trees = fromEnum (isVisible (size-2) (size-2) trees)
visibleTrees 97 y trees = (visibleTrees 1 (y+1) trees) + fromEnum (isVisible (size-2) y trees)
visibleTrees x y trees = (visibleTrees (x+1) y trees) + fromEnum (isVisible x y trees)

viewScoreTrees :: Int -> Int -> Map (Int, Int) Int -> [Int]
viewScoreTrees 97 97 trees = [viewScore (size-2) (size-2) trees]
viewScoreTrees 97 y trees = (viewScoreTrees 1 (y+1) trees) ++ [(viewScore (size-2) y trees)]
viewScoreTrees x y trees = (viewScoreTrees (x+1) y trees) ++ [(viewScore x y trees)]


isVisible x y trees = do
    let row = all (== True) (visibleTreeRow x y trees)
    let rowRev = all (== True) (visibleTreeRowRev x y trees)
    let col = all (== True) (visibleTreeCol x y trees)
    let colRev = all (== True) (visibleTreeColRev x y trees)   
    row || rowRev || col || colRev

viewScore :: Int -> Int -> Map (Int, Int) Int -> Int
viewScore x y trees = do
    let row = visibleScoreRow $ reverseList (visibleTreeRow x y trees)
    let rowRev = visibleScoreRow (visibleTreeRowRev x y trees)
    let col = visibleScoreRow $ reverseList (visibleTreeCol x y trees)
    let colRev = visibleScoreRow (visibleTreeColRev x y trees)
    row * rowRev * col * colRev

viewScoreTest x y trees = do
    let row = visibleScoreRow $ reverseList (visibleTreeRow x y trees)
    let rowRev = visibleScoreRow (visibleTreeRowRev x y trees)
    let col = visibleScoreRow $ reverseList (visibleTreeCol x y trees)
    let colRev = visibleScoreRow (visibleTreeColRev x y trees)
    reverseList (visibleTreeRow x y trees)


visibleTreeRow x y trees = do
    let inds = [0..(x-1)]
    let preceedingTrees = map (\x -> Map.lookup (x, y) trees) inds
    map (< Map.lookup (x, y) trees) preceedingTrees

visibleTreeRowRev x y trees = do
    let inds = [(x+1)..(size-1)]
    let preceedingTrees = map (\x -> Map.lookup (x, y) trees) inds
    map (< Map.lookup (x, y) trees) preceedingTrees

visibleTreeCol x y trees = do
    let inds = [0..(y-1)]
    let preceedingTrees = map (\y -> Map.lookup (x, y) trees) inds
    map (< Map.lookup (x, y) trees) preceedingTrees

visibleTreeColRev x y trees = do
    let inds = [(y+1)..(size-1)]
    let preceedingTrees = map (\y -> Map.lookup (x, y) trees) inds
    map (< Map.lookup (x, y) trees) preceedingTrees

visibleScoreRow :: [Bool] -> Int
visibleScoreRow [] = 0
visibleScoreRow (tree:rst) 
    | tree == True = 1 + visibleScoreRow rst
    | otherwise = 1

justLookup key map = fromJust (Map.lookup key map)
reverseList = \list ->
    case list of
        [] -> []
        x:xs -> reverseList xs ++ [x]

main = do
    file <- readFile "input\\day_8.txt"
    let treeLines = lines file
    let myTreeMap = treeMap (lines file) 0 0

    print $ (visibleTrees 1 1 myTreeMap) + (4*(size-1)) -- P1
    print $ head $ sortBy (\a b -> compare b a) $ viewScoreTrees 1 1 myTreeMap -- P2
    