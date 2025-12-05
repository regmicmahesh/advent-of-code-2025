import Data.Char (chr, isSpace, ord)
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

addOneRev :: [Char] -> [Char]
addOneRev x
  | null x = "1"
  | mcb /= '9' = chr (ord mcb + 1) : tail x
  | mcb == '9' = '0' : addOneRev (tail x)
  where
    mcb = head x

addOne :: [Char] -> [Char]
addOne x = reverse (addOneRev (reverse x))

isEveryNCharSame :: Eq a => [a] -> [a] -> Bool
isEveryNCharSame truth x
  | null x = True
  | length x < tl = False
  | (take tl x) == truth = isEveryNCharSame truth (drop tl x)
  | otherwise = False
  where
    tl = length truth


isEveryNCharSubstring  substrl str
  | substrl > ((length str) `quot` 2) = False
  | isEveryNCharSame (take substrl str) str = True
  | otherwise = isEveryNCharSubstring (substrl+1) str


isItValid = not . isEveryNCharSubstring 1
isItInvalid = isEveryNCharSubstring 1

processTuple :: [Char] -> [Char] -> Int -> Int
processTuple start end sum
  | isItInvalid start && (start == end) = sum + (read start :: Int)
  | isItValid start && (start == end) = sum
  | isItInvalid start = processTuple (addOne start) end (sum + (read start :: Int))
  | isItValid start = processTuple (addOne start) end sum

main :: IO ()
main = do
  rawContent <- readFile "input.txt"
  let x = map (splitDash . trim) (wordsWhen rawContent (== ','))
  let rangeResults = map (\(start, end) -> processTuple start end 0) x
  print (sum rangeResults)
