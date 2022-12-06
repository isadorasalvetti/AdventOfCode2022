import qualified Data.Set as Set

startOfPacket :: String -> Int -> Int
startOfPacket (a:b:c:d:line) pos
    | length (Set.fromList [a, b, c, d]) == 4 = pos
    | otherwise = startOfPacket (b:c:d:line) pos + 1


startOfMessage :: String -> Int -> Int
startOfMessage line pos
    | length (Set.fromList(take 14 line)) == 14 = pos
    | otherwise = startOfMessage (drop 1 line) pos + 1


main = do
    file <- readFile "input\\day_6.txt"
    print file
    print $ startOfPacket file 4
    print $ startOfMessage file 14
