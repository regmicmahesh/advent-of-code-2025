import Data.Char qualified
import Data.List (transpose)


calculate :: (Char, [String]) -> Integer
calculate (op, xs)
  | op == '+' = addAll xs
  | op == '*' = multiplyAll xs
  | otherwise = error "what is that operator?"

pad :: Int -> String -> String
pad 0 s = s
pad i s
  | i <= sl = s
  | otherwise = s ++ replicate np '0'
  where
    np = i - sl
    sl = length s

padList :: Int -> [String] -> [String]
padList _ [] = []
padList i (x : xs) = pad i x : padList i xs

padGrid :: Int -> [[String]] -> [[String]]
padGrid _ [] = []
padGrid i (x : xs) = padList i x : padGrid i xs

zipX :: [String] -> [String]
zipX s = if not $ null (head s) then map head s : zipX (map tail s) else []

allZero :: String -> Bool
allZero "" = False
allZero "0" = True
allZero (s : xs) = s == '0' && allZero xs

notAllZero x = not $ allZero x

removeAllZerosList :: [String] -> [String]
removeAllZerosList = filter notAllZero

isSpaceString :: (Foldable t) => t Char -> Bool
isSpaceString = all Data.Char.isSpace

chunk' :: [String] -> [[String]]
chunk' [] = [[]]
chunk' [s] = [[]]
chunk' xs = reverse h : chunk' (drop 1 rem)
  where
    (h, rem) = break isSpaceString xs

chunk :: [String] -> [[String]]
chunk x = init (chunk' x)


doAll :: ([Integer] -> Integer) -> [String] -> Integer
doAll f s = f $ map (\x -> read x :: Integer) s

addAll :: [String] -> Integer
addAll = doAll sum

multiplyAll :: [String] -> Integer
multiplyAll = doAll product

extractOperator :: [String] -> (Char, [String])
extractOperator (x:xs) = (op, xWithoutOp:xs)
  where
  op = last x
  xWithoutOp = take (length x - 1) x

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let items = lines contents
  let nums = reverse $ transpose items
  let items = map extractOperator (chunk nums)
  print $ sum  $ map calculate items


index ::  [Int] -> Int -> Int
index [a] b = if (a /= b) then -1 else 1
index (x:xs) b = if x == b then 1 else 1 + index xs b
