import Data.Maybe
import Data.Char
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

parseValve ('V':'a':'l':'v':'e':' ':a:b:rst) = (a:[b], rst)
parseFlowRate :: String -> Int
parseFlowRate str = read (takeWhile isDigit (dropWhile (not . isDigit) str))
parseTunnels str = words (dropWhile (\a -> isLower a|| isSpace a) (tail (dropWhile (/= ';') str)))

parseInput :: String -> (String, (Int, [String]))
parseInput str = (valveName, (valveFlow, valveConnections))
    where
        (valveName, valveNameRst) = parseValve str
        valveFlow = parseFlowRate valveNameRst
        valveConnections = parseTunnels valveNameRst

findPath :: String -> Map String (Int, [String]) -> [(String, [String])] -> [String]
findPath target graph ((currPos, explored):toVisit)
    | target `elem` nextNodes = currPos:explored
    | currPos `elem` explored = findPath target graph toVisit
    | otherwise = findPath target graph (toVisit ++ newOptions)
    where
        (_, nextNodes) = unwrapLookup currPos graph
        newOptions = zip (filter (`notElem` explored) nextNodes) (repeat (currPos:explored)) ++ toVisit


unwrapLookup :: String -> Map String (Int, [String]) -> (Int, [String])
unwrapLookup key map = fromJust (Map.lookup key map)

main = do
    file <- readFile "input\\mock_day_16.txt"
    let mappedInput = Map.fromList (map parseInput (lines file))
    print $ findPath "EE" mappedInput [("DD", [])]
    print $ findPath "JJ" mappedInput [("AA", [])]
    print $ findPath "JJ" mappedInput [("EE", [])]
    print mappedInput

