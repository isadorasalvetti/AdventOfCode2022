import Data.Maybe
import Data.Char
import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map

data Exp = N Int | E (Char, (String, String)) deriving Show

parseMonkey :: String -> (String, Exp)
parseMonkey string
    | isDigit (last rst) = (mnky, N (read (tail rst)))
    | otherwise = (mnky, stringToExp (words $ tail rst))
    where (mnky, rst) = break (==':') string


stringToExp :: [String] -> Exp
stringToExp [n1, op, n2] = E (head op, (n1, n2))

doOperation :: Exp -> Map String Exp-> Int
doOperation (N int) mnkys = int
doOperation (E (o, (num1, num2))) mnkys
    | o == '+' = op1 + op2
    | o == '-' = op1 - op2
    | o == '*' = op1 * op2
    | otherwise = op1 `div` op2
    where op1 = doOperation (mnkys Map.! num1) mnkys
          op2 = doOperation (mnkys Map.! num2) mnkys

findHmn :: Exp -> Map String Exp -> Int -> Int
findHmn (N int) mnkys res = int
findHmn (E (o, (num1, num2))) mnkys res
    | num1 == "humn" = solveL o (findHmn ex2 mnkys 0) res
    | num2 == "humn" = solveR o (findHmn ex1 mnkys 0) res
    | hasHmn ex1 mnkys = do
        let otherSide = findHmn ex2 mnkys 0
        let expected = solveL o otherSide res
        findHmn ex1 mnkys expected
    | hasHmn ex2 mnkys = do
        let otherSide = findHmn ex1 mnkys 0
        let expected = solveR o otherSide res
        findHmn ex2 mnkys expected
    | otherwise = pass o op1 op2
    where ex1 = mnkys Map.! num1
          ex2 = mnkys Map.! num2
          op1 = doOperation ex1 mnkys
          op2 = doOperation ex2 mnkys

hasHmn :: Exp -> Map String Exp -> Bool
hasHmn (N int) mnkys = False
hasHmn (E (o, (num1, num2))) mnkys 
    | num1 == "humn" || num2 == "humn" = True
    | otherwise = hasHmn ex1 mnkys || hasHmn ex2 mnkys
    where ex1 = mnkys Map.! num1
          ex2 = mnkys Map.! num2


solveL :: Char -> Int -> Int -> Int
solveL o n1 res 
    | o == '+' = res - n1
    | o == '-' = res + n1
    | o == '*' = res `div` n1
    | otherwise = res * n1

solveR :: Char -> Int -> Int -> Int
solveR o n1 res 
    | o == '+' = res - n1
    | o == '-' = n1 - res
    | o == '*' = res `div` n1
    | otherwise = n1 `div` res

pass :: Char -> Int -> Int -> Int
pass o n1 n2 
    | o == '+' = n1 + n2
    | o == '-' = n1 - n2
    | o == '*' = n1 * n2
    | otherwise = n1 `div` n2


main = do
    file <- readFile "input\\day_21.txt"
    let mapMonkeys = Map.fromList $ map parseMonkey (lines file)
    let p1 = doOperation (mapMonkeys Map.! "root") mapMonkeys
    print p1

    let p2 = findHmn (mapMonkeys Map.! "root") mapMonkeys 0
    print p2


