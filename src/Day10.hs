import Data.Maybe
import Data.Map (Map)
import Data.List.Split
import qualified Data.Map as Map

execCmd :: (String, Int) -> (Int, Int) -> (Int, Int)
execCmd (cmd, amm) (tick, val)
    | cmd == "noop" = (tick + 1, val)
    | otherwise = (tick + 2, val + amm)

parseCmd :: String -> (String, Int)
parseCmd cmd
    | take 4 cmd == "noop" = ("noop", 0)
    | otherwise = (take 4 cmd, read $ drop 5 cmd)

runProgram :: [(String, Int)] -> (Int, Int) -> Map Int Int
runProgram [] _ = Map.fromList [(1, 1)]
runProgram (cmd:rst) state = do
    let newState = execCmd cmd state
    Map.union (Map.fromList ([newState])) (runProgram rst newState)

cicleVal :: Map Int Int -> Int -> Int
cicleVal states cicle
    | Map.member cicle states = fromJust (Map.lookup cicle states)
    | otherwise = cicleVal states (cicle-1)

fillList :: [(Int, Int)] -> [(Int, Int)]
fillList [] = []
fillList [curr] = [curr]
fillList ((i, x):(i2, x2):rst)
    | i2 == i+1 = (i, x) : fillList ((i2, x2):rst)
    | otherwise = (i, x) : fillList ((i+1, x):(i2, x2):rst)

stateToRender :: [(Int, Int)] -> String
stateToRender [] = ""
stateToRender ((tick, val):rst)
    | abs (val - (tick-1) `mod` 40) <= 1 = "#" ++ (stateToRender rst)
    | otherwise = "." ++ (stateToRender rst)

onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines ( x:[] ) = x
onSeparateLines ( x:xs ) = x ++  "\n" ++ onSeparateLines xs

main = do
    file <- readFile "input\\day_10.txt"
    let parsedCmds = map parseCmd (lines file)
    let states = runProgram parsedCmds (1, 1)
    let lookupCicles = [20, 60, 100, 140, 180, 220]
    let vals = map (\x -> x * (cicleVal states x)) lookupCicles
    print $ sum vals -- p1

    let filledVals = fillList (Map.toList states)
    let renderLine = stateToRender filledVals
    print $ take 15 filledVals
    putStrLn $ onSeparateLines (chunksOf 40 renderLine)

