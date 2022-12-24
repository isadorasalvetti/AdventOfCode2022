import Data.Maybe
import Debug.Trace

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

sample = [1, 2, -3, 3, -2, 0, 4]
sample2 = [0, 1, 1, 8]

listToMap :: [Int] -> Int -> Map Int (Int, Int, Int)
listToMap [num] index = Map.fromList [(index, (num, index-1, 0))]
listToMap (num:rst) 0 = Map.union (Map.fromList [(0, (num, length rst, 1))]) (listToMap rst 1)
listToMap (num:rst) index = Map.union (Map.fromList [(index, (num, index-1, index+1))]) (listToMap rst (index+1))

shiftMap :: Map Int (Int, Int, Int) -> Int -> Int -> Map Int (Int, Int, Int)
shiftMap mp v key
    | t == v = mp
    | otherwise = insertPos (deletePos mp v) t (v, val) 
    where (val, vPrev, vNext) = mp `mm` v
          t = goNext v ((val*key) `mod` (Map.size mp -1)) mp

deletePos :: Map Int (Int, Int, Int) -> Int -> Map Int (Int, Int, Int)
deletePos mp toDelete = do
    let m1 = Map.delete toDelete mp 
    let m2 = Map.insert prev (bVal, bPrev, next) m1
    Map.insert next (nVal, prev, nNext) m2
    where (tVal, prev, next) = mp `mm` toDelete
          (bVal, bPrev, bNext) = mp `mm` prev
          (nVal, nPrev, nNext) = mp `mm` next

insertPos :: Map Int (Int, Int, Int) -> Int -> (Int, Int) -> Map Int (Int, Int, Int)
insertPos mp newPos (v, val) = do
    let mp1 = Map.insert v (val, newPos, oldNext) mp
    let mp2 = Map.insert newPos (oldVal, oldPrev, v) mp1
    Map.insert oldNext (bVal, v, bNext) mp2

    where (oldVal, oldPrev, oldNext) = mp `mm` newPos
          (bVal, bPrev, bNext) = mp `mm` oldNext

goNext :: Int -> Int -> Map Int (Int, Int, Int) -> Int
goNext index 0 mp = index
goNext index 1 mp = next
    where (_, _, next) = mp `mm` index
goNext index (-1) mp = pPrev
    where (_, prev, _) = mp `mm` index
          (_, pPrev, _) = mp `mm` prev

goNext index amm mp
    | amm < 0  = goNext prev (amm+1) mp
    | otherwise = goNext next (amm-1) mp
    where (val, prev, next) = mp `mm` index

makeList :: Map Int (Int, Int, Int) -> Int -> Int -> [Int]
makeList mp num fst
    | next == fst = [val]
    | otherwise = val:makeList mp next fst
    where (val, _, next) = mp `mm` num
          rst = makeList mp next

shiftAll :: Map Int (Int, Int, Int) -> Int -> Int -> Int -> Map Int (Int, Int, Int)
shiftAll mp count end key
    | count == end = mp
    | otherwise = shiftAll (shiftMap mp count key) (count+1) end key
    -- trace(show (count, makeList mp 0 0, mp)) 

mixNtimes mp 0 key = mp
mixNtimes mp n key = mixNtimes (shiftAll mp 0 (Map.size mp) key) (n-1) key

mm mp index
    | index `Map.notMember` mp = error ("key " ++ show index ++ " not found in " ++ show mp)
    | otherwise = mp Map.! index

main = do
    file <- readFile "input\\day_20.txt"
    let ln = lines file
    let index0 = fromJust (elemIndex 0 sample)
    let numMp = listToMap sample 0    
    
    let index0 = fromJust (elemIndex "0" ln)
    let numMp = listToMap (map read (lines file)) 0

    let mixed = mixNtimes numMp 1 1

    -- print mixed
    -- print $ makeList mixed 0 0
    let index1000 = goNext index0 1000 mixed 
    let index2000 = goNext index1000 1000 mixed
    let index3000 = goNext index2000 1000 mixed
    print $ mixed `mm` index1000 --p1
    print $ mixed `mm` index2000
    print $ mixed `mm` index3000

    let mixed = mixNtimes numMp 10 811589153
    let index1000 = goNext index0 1000 mixed
    let index2000 = goNext index1000 1000 mixed
    let index3000 = goNext index2000 1000 mixed
    print $ mixed `mm` index1000 --p2
    print $ mixed `mm` index2000
    print $ mixed `mm` index3000