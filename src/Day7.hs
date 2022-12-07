import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

num_chars = ['0'..'9']

data Content = Size Int | Folder String
view :: Content -> String
view (Size n) = "File: " ++ show n
view (Folder f) = show f
instance Show Content where
    show = view

moveFolder :: String -> [String] -> [String]
moveFolder newLocation oldLocation
    | newLocation == "/" = [newLocation]
    | newLocation == ".." = tail oldLocation
    | otherwise = newLocation:oldLocation

folderStructure :: [String] -> [String] -> Map [String] [String] -> Map [String] [String]
folderStructure _ [] currStructure = currStructure
folderStructure location (line:rst) currStructure
    | take 4 line == "$ cd" = folderStructure (moveFolder (drop 5 line) location) rst currStructure
    | take 4 line == "$ ls" = folderStructure location rst currStructure
    | take 3 line == "dir" = do
        let newStructure = Map.insertWith (++) location [(drop 4 line)] currStructure
        folderStructure location rst newStructure
    | elem (head line) num_chars = do
        let newStructure = Map.insertWith (++) location [line] currStructure
        folderStructure location rst newStructure

    | otherwise = Map.empty

directorySize :: Map [String] [String] -> ([String], [String]) -> Int
directorySize structure (dirname, (el:dir))
    | elem (head el) num_chars = read (takeWhile (\x -> elem x num_chars) el) + directorySize structure (dirname, dir)
    | otherwise = directorySize structure ((el:dirname), fromJust (Map.lookup (el:dirname) structure)) + directorySize structure (dirname, dir)
directorySize structure (dirname, []) = 0



main = do
    file <- readFile "input\\day_7.txt"
    let cmdlines = lines file
    let folderStruct = folderStructure ["/"] cmdlines Map.empty
    let sizes = sort $ map (directorySize folderStruct) (Map.toList folderStruct)
    print $ sum (takeWhile (<= 100000) sizes) -- Part 1

    let neededSize = 30000000 - (70000000 - (last sizes))
    print $ head (dropWhile (< neededSize) sizes)
    


