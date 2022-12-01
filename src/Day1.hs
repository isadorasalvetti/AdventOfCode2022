import Data.List

splitByElves :: [Char] -> [[Char]]
splitByElves ('\n':'\n':ls) = []:splitByElves ls
splitByElves (x:xs) = (x:hd):tl
    where hd = meHead res
          tl = meTail res
          res = splitByElves xs
splitByElves [] = []

meHead [] = []
meHead ls = head ls

meTail [] = []
meTail ls = tail ls

fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

main :: IO ()
main = do
    file <- readFile "input\\day_1.txt"
    let elves_food = splitByElves file
    let elves_food_num = map words elves_food
    let elves_food_int = map (map read) elves_food_num :: [[Int]]
    let sums = sortBy (\a b -> compare b a) $ map (foldl1 (+)) elves_food_int

    print (sums!!0)
    print (sums!!0 + sums!!1 + sums!!2)

