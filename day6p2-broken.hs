import Data.List (transpose)

doAll :: ([Integer] -> Integer) -> [String] -> Integer
doAll f s = f $ map (\x -> read x :: Integer) s

addAll :: [String] -> Integer
addAll = doAll sum

multiplyAll :: [String] -> Integer
multiplyAll = doAll product

calculate :: (String, [String]) -> Integer
calculate (op, xs)
  | op == "+" = addAll xs
  | op == "*" = multiplyAll xs
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
allZero (s : xs) = (s == '0') && allZero xs

notAllZero x = not $ allZero x

removeAllZerosList :: [String] -> [String]
removeAllZerosList = filter notAllZero

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let items = map words (lines contents)

  let nums = transpose $ padGrid 4 $ take (length items - 1) items
  let [ops] = drop (length items - 1) items
  print ops
  print (map zipX nums)
  print $ zipWith (curry calculate) ops (map (removeAllZerosList . zipX) nums)
