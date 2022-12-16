data Nested = I Int | N [Nested] deriving (Show)

isDigit x = x `elem` "0123456789"

parse :: String -> Nested
parse "[]" = N $ parseNumString ""
parse ('[':rst)  
    | '[' `elem` body = N [parse body]
    | otherwise = N (parseNumString body)
    where body = init rst

-- parse ('[':'[':rst) = N [parse ('[':rst)]
-- parse (']':']':rst) = parse (']':rst)
-- parse (a:str) =
--     case rst of
--         ('[':']':some) -> N (nestedNums ++ [parse some])
--         "]" -> N nestedNums
--         ']':_ -> N nestedNums -- TODO
--         '[':_ -> N $ nestedNums ++ [parse (tail (init rst))]
--         _ -> parse rst
--     where (numString, rst) = span (\x -> isDigit x || x == ',') str
--           nestedNums = parseNumString numString
-- parse a = I (-1)

parseNumString :: String -> [Nested]
parseNumString [] = []
parseNumString str = I (read num) : rem
    where (num, rst) = span (/=',') str
          rem = case rst of
            [] -> []
            ',':l -> parseNumString l

main = do
    let all = ["[]", "[0]", "[3]", "[10]", "[1,6]"]
    print $ map parse all
    let h1 = ["[[]]", "[[[]]]", "[[[[]]]]"]
    print $ map parse h1
    let h2 = ["[1,[]]", "[[],1]"]
    print h2
    print $ map parse h2

-- parseNested :: String -> Nested
-- parseNested [] = N []
-- parseNested ('[':rst) = N [parseNested rst]
-- parseNested (']':rst) = parseNested rst

-- parseNested str
--     | length rest > 0 = N ([makeNums numString] ++ [parseNested rest])
--     | otherwise = makeNums numString
--     where
--         (numString, rest) = span (`notElem` "[]") str


-- makeNums :: String -> Nested
-- makeNums [] = N []
-- makeNums (',':rst) = makeNums rst
-- makeNums str = N ([I $ read num] ++ inside (makeNums rst))
--     where 
--         (num, rst) = span (/= ',') str

-- inputPairs :: [String] -> [(Nested, Nested)]
-- inputPairs [] = []
-- inputPairs (f:s:_:rst) = [(parseNested f, parseNested s)] ++ inputPairs rst

-- main = do
--     file <- readFile "input\\mock_day_13.txt"
--     print "Ok"
    -- print $ inputPairs (lines file)
