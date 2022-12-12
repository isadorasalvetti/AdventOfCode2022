data Monkey = Monkey {items :: [Int], operation :: Int -> Int, test :: Int -> Bool, friends :: [Int]} 

mockMonkey0 = Monkey {items = [79, 98], operation = (\x -> x * 19), test = (\x -> x `mod` 23 == 0), friends = [3, 2]}
mockMonkey1 = Monkey {items = [54, 65, 75, 74], operation = (\x -> x + 6), test = (\x -> x `mod` 19 == 0), friends = [0, 2]}
mockMonkey2 = Monkey {items = [79, 60, 97], operation = (\x -> x * x), test = (\x -> x `mod` 13 == 0), friends = [3, 1]}
mockMonkey3 = Monkey {items = [74], operation = (\x -> x + 3), test = (\x -> x `mod` 17 == 0), friends = [1, 0]}

monkey0 = Monkey {items = [72, 97], operation = (\x -> x * 13), test = (\x -> x `mod` 19 == 0), friends = [6, 5]}
monkey1 = Monkey {items = [55, 70, 90, 74, 95], operation = (\x -> x * x), test = (\x -> x `mod` 7 == 0), friends = [0, 5]}
monkey2 = Monkey {items = [74, 97, 66, 57], operation = (\x -> x + 6), test = (\x -> x `mod` 17 == 0), friends = [0, 1]}
monkey3 = Monkey {items = [86, 54, 53], operation = (\x -> x + 2), test = (\x -> x `mod` 13 == 0), friends = [2, 1]}
monkey4 = Monkey {items = [50, 65, 78, 50, 62, 99], operation = (\x -> x + 3), test = (\x -> x `mod` 11 == 0), friends = [7, 3]}
monkey5 = Monkey {items = [90], operation = (\x -> x + 4), test = (\x -> x `mod` 2 == 0), friends = [6, 4]}
monkey6 = Monkey {items = [88, 92, 63, 94, 96, 82, 53, 53], operation = (\x -> x + 8), test = (\x -> x `mod` 5 == 0), friends = [7, 4]}
monkey7 = Monkey {items = [70, 60, 71, 69, 77, 70, 98], operation = (\x -> x * 7), test = (\x -> x `mod` 3 == 0), friends = [3, 2]}

instance Show Monkey where
  show (Monkey items _ _ _) = (show items)


monkeysRound :: Int -> Int -> [Monkey] -> [Int] -> ([Int], [Monkey])
monkeysRound 20 _ allMonky counter = (counter, allMonky)
monkeysRound round playing allMonky counter
    | playing == length allMonky = monkeysRound (round+1) 0 allMonky counter
    | otherwise = do
        let thrownItems = monkeyTurn (allMonky!!playing)
        let newCounter = replace playing (counter!!playing + (length thrownItems)) counter
        let thrownAllMonky = replace playing (removeMonkeyItem (allMonky!!playing)) allMonky 
        monkeysRound round (playing+1) (takeItems thrownItems thrownAllMonky) newCounter

takeItems :: [(Int, Int)] -> [Monkey] -> [Monkey]
takeItems [] monkeys = monkeys
takeItems ((iMonkey, item):rst) monkeys = do
    let newMonkey = giveMonkeyItem item (monkeys!!iMonkey)
    takeItems rst (replace iMonkey newMonkey monkeys)

giveMonkeyItem :: Int -> Monkey -> Monkey
giveMonkeyItem item (Monkey items o t f) = Monkey {items = items ++[item], operation=o, test=t, friends=f} 

removeMonkeyItem :: Monkey -> Monkey
removeMonkeyItem (Monkey _ o t f) = Monkey {items = [], operation=o, test=t, friends=f} 

monkeyTurn :: Monkey -> [(Int, Int)]
monkeyTurn (Monkey items op test friends) = do
    let newWorry = map (\x -> (op x) `div` 3) items
    map (\x -> (friends!!(fromEnum (test x)), x)) newWorry

replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

main = do
    --let monkys = [mockMonkey0, mockMonkey1, mockMonkey2, mockMonkey3]
    let monkys = [monkey0, monkey1, monkey2, monkey3, monkey4, monkey5, monkey6, monkey7]
    
    let startCounter = take (length monkys) (repeat 0)
    print $ monkeysRound 0 0 monkys startCounter