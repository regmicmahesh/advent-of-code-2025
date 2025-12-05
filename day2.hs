
import Data.Char (isSpace, ord, chr)
import Data.List (nub)


wordsWhen :: [Char] -> (Char -> Bool) -> [[Char]]
wordsWhen s f = case dropWhile f s of
  "" -> []
  s' -> w : wordsWhen s'' f
    where
      (w, s'') =
        break f s'

trim :: [Char] -> [Char]
trim x = reverse (dropWhile isSpace (reverse (dropWhile isSpace x)))

splitDash :: [Char] -> (String, String)
splitDash x = (head l, last l) where l = wordsWhen x (== '-')

addOneRev x
    | null x = "1"
    | mcb /= '9' = chr (ord mcb + 1) : tail x
    | mcb == '9' = '0' : addOneRev (tail x)
    where
        mcb = head x

addOne :: [Char] -> [Char]
addOne x = reverse (addOneRev (reverse x))

isItValid :: Eq a => [a] -> Bool
isItValid x
    | odd (length x)  = True
    | take half x == drop half x  = False
    | otherwise = True
    where
        len = length x
        half = len `quot` 2

isItInvalid :: Eq a => [a] -> Bool
isItInvalid x = not (isItValid x)

processTuple :: [Char] -> [Char] -> Int -> Int
processTuple start end sum
    | isItInvalid start && (start == end) = sum + (read start :: Int)
    | isItValid start && (start == end) = sum
    | isItInvalid start = processTuple (addOne start) end (sum + (read start :: Int))
    | isItValid start = processTuple (addOne start) end sum

main :: IO()
main = do
    rawContent <- readFile "input.txt"
    let x= map (splitDash . trim) (wordsWhen rawContent (== ','))
    let rangeResults = map (\(start, end) -> processTuple start end 0) x
    print (sum rangeResults)
    