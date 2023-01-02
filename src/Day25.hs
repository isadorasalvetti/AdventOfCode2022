import Debug.Trace
import Data.Char

toNum '=' = -2
toNum '-' = -1
toNum char = read [char]

toChar (-2) = '='
toChar (-1) = '-'
toChar num = intToDigit num

readSNAFU str = sum $ map (\(num, pos) -> toNum num * 5 ^ pos) $ zip str (reverse [0..length str-1])

makeSNAFU 0 = []
makeSNAFU num = makeSNAFU rst ++ [toChar (dig-2)]
    where (rst, dig) = (num+2) `quotRem` 5

main = do
    file <- readFile "input\\day_25.txt"
    let nums = lines file
    let numsList = map readSNAFU nums
    let sumList = sum numsList
    print $ makeSNAFU sumList
