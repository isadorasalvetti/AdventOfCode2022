import Data.Either
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec
import Data.List
import Data.List (elemIndex)

data Nested = I Int | N [Nested] deriving (Show, Eq)

instance Ord Nested where
    compare :: Nested -> Nested -> Ordering
    compare (I a) (I b) = compare a b
    compare (N []) (N []) = EQ
    compare (N(_:_)) (N []) = GT
    compare (N []) (N(_:_)) = LT

    compare (N a) (I b) = compare (N a) (N [I b])
    compare (I a) (N b) = compare (N [I a]) (N b)

    compare (N (a:arst)) (N (b:brst))
        | (compare a b == EQ) = compare (N arst) (N brst)  -- a == b does not work for reasons
        | otherwise = compare a b

parseNested :: String -> Either ParseError Nested
parseNested = parse value ""
    where
        value = int <|> list
        int  = I . read <$> many1 digit
        list  = N <$> between (symbol '[') (symbol ']') (value `sepBy` symbol ',')

symbol = try . char

parseInput :: [String] -> [(Nested, Nested)]
parseInput [] = []
parseInput (a:b:_:rst) = (fromRight (N []) (parseNested a), fromRight (N []) (parseNested b)):parseInput rst

-- getIndices :: [Ordering] -> Int
getIndices ords = do
    let correctOrds = filter (\(ord, i) -> ord == LT) (zip ords [1..])
    let indexSum = foldl (\acc (ord, i) -> acc + i) 0 correctOrds
    indexSum
    -- correctOrds

notExpected ords = do
    let expected = [3, 6, 7, 9, 11, 13, 16, 17, 19, 21, 23, 24, 28, 29, 30, 32, 33, 37, 38, 39, 41, 43, 44, 46, 50, 51, 54, 58, 61, 63, 64, 65, 66, 68, 73, 74, 75, 76, 77, 79, 82, 85, 93, 97, 98, 99, 100, 101, 106, 108, 110, 111, 117, 119, 120, 123, 125, 126, 128, 129, 130, 132, 134, 135, 136, 138, 140, 141, 143, 144, 146, 148, 149, 150]
    let actual = map snd ords
    filter (`notElem` actual) expected

newPackets = [N [N [I 2]], N [N [I 6]]]

main = do
    file <- readFile "input\\day_13.txt"
    let parsed = parseInput (lines file)
    let allPackets = foldl (\acc (a, b) -> acc ++ a:[b]) [] parsed
    let comparisons = map (uncurry compare) parsed
    let sorted = sort (allPackets ++ newPackets)
    
    -- print $ zip comparisons [1..]
    let p1 = getIndices comparisons
    print p1
    print (elemIndex (head newPackets) sorted, elemIndex (last newPackets) sorted)




