
roundPoints1 :: Char -> Char -> Int

roundPoints1 'A' 'X' = 3 + 1
roundPoints1 'B' 'X' = 0 + 1
roundPoints1 'C' 'X' = 6 + 1

roundPoints1 'A' 'Y' = 6 + 2
roundPoints1 'B' 'Y' = 3 + 2
roundPoints1 'C' 'Y' = 0 + 2

roundPoints1 'A' 'Z' = 0 + 3
roundPoints1 'B' 'Z' = 6 + 3
roundPoints1 'C' 'Z' = 3 + 3

roundPoints2 :: Char -> Char -> Int

roundPoints2 'A' 'X' = 0 + 3
roundPoints2 'B' 'X' = 0 + 1
roundPoints2 'C' 'X' = 0 + 2

roundPoints2 'A' 'Y' = 3 + 1
roundPoints2 'B' 'Y' = 3 + 2
roundPoints2 'C' 'Y' = 3 + 3

roundPoints2 'A' 'Z' = 6 + 2
roundPoints2 'B' 'Z' = 6 + 3
roundPoints2 'C' 'Z' = 6 + 1

part1 :: String -> Int
part1 (p1:p2:games) = roundPoints1 p1 p2 + part1 games
part1 [] = 0 

part2 :: String -> Int
part2 (p1:p2:games) = roundPoints2 p1 p2 + part2 games
part2 [] = 0 


main = do
    file <- readFile "input\\day_2.txt"
    let chars = map head (words file)
    print $ part1 chars
    print $ part2 chars
    



