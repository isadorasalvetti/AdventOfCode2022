import Data.List
import Data.Maybe

splitSack :: String -> (String, String)
splitSack sack = splitAt ((length sack) `div` 2) sack

splitGroup :: [String] -> [[String]]
splitGroup (s1:s2:s3:sacks) = [[s1, s2, s3]] ++ splitGroup sacks
splitGroup [] = []

getMatch :: (String, String) -> Char
getMatch (p1, p2) = fromJust $ find (\x -> elem x p1) p2

getGroupMatch :: [String] -> Char
getGroupMatch (s1:s2:s3:rst) = fromJust $ find (isIn s1 s2) s3

isIn lst1 lst2 x = elem x lst1 && elem x lst2

getPriority :: Char -> Int
getPriority c
          | elem c ['a'..'z'] = fromJust (elemIndex c ['a'..'z']) + 1
          | elem c ['A'..'Z'] = fromJust (elemIndex c ['A'..'Z']) + 27

p1 rucksacks = do
    let splitSacks = map splitSack rucksacks
    let matches = map getMatch splitSacks
    let vals = map getPriority matches
    sum vals

p2 rucksacks = do
    let groups = splitGroup rucksacks
    let matches = map getGroupMatch groups
    let vals = map getPriority matches
    sum vals

main = do
    file <- readFile "input\\day_3.txt"
    let rucksacks = words file
    print $ p2 rucksacks
    