import Data.List.Split

parseCrates :: [String] -> [[Char]]
parseCrates input = do
    let lineIds = [x*4 + 1 | x <- [0..8]]
    let lines = [0..(length input)-1]
    let colums = [input!!y!!x | x <- lineIds, y <-lines]
    let letterCols = chunksOf 8 colums
    [filter(/= ' ') slst | slst <- letterCols]


parseMove :: String -> (Int, Int, Int)
parseMove line = do
    let xline = dropWhile (\x -> elem x (' ':['a'..'z'])) line
    let (x, restx) = span (\x -> elem x ['0'..'9']) xline

    let yline = dropWhile (\x -> elem x (' ':['a'..'z'])) restx
    let (y, resty) = span (\x -> elem x ['0'..'9']) yline

    
    let zline = dropWhile (\x -> elem x (' ':['a'..'z'])) resty
    let (z, restz) = span (\x -> elem x ['0'..'9']) zline

    (read x, read y, read z)

move :: [String] -> (Int, Int, Int) -> [String]
move crates (amm, src, dst) = do
    let (cratesMove, srcState) = splitAt amm (crates!!(src-1))
    let dstState = (reverse cratesMove) ++ crates!!(dst-1)
    let crateInds = zip crates [0..]
    map (\(curr, x) -> mapNewCrateState x curr src dst srcState dstState) crateInds

move9001 :: [String] -> (Int, Int, Int) -> [String]
move9001 crates (amm, src, dst) = do
    let (cratesMove, srcState) = splitAt amm (crates!!(src-1))
    let dstState = cratesMove ++ crates!!(dst-1)
    let crateInds = zip crates [0..]
    map (\(curr, x) -> mapNewCrateState x curr src dst srcState dstState) crateInds


mapNewCrateState x curr src dst newSrc newDst
                | x == (src-1) = newSrc
                | x == (dst-1) = newDst
                | otherwise = curr

makeMoves state [] = state
makeMoves state (m:ms) = makeMoves (move state m) ms

makeMoves9001 state [] = state
makeMoves9001 state (m:ms) = makeMoves9001 (move9001 state m) ms

main = do
    file <- readFile "input\\day_5.txt"
    let input = lines file
    let inputStacks = takeWhile (\x-> x!!1 /= '1') input
    let inputMoves = dropWhile (\x-> (length x) < 1 || x!!0 /= 'm') input
    let crateState = parseCrates inputStacks
    let moves = map parseMove inputMoves
    print crateState
    -- print moves
    print $ makeMoves9001 crateState moves

