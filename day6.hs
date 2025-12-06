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

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (ops : ns') = map words (reverse $ lines contents)
  print $ sum $ zipWith (curry calculate) ops (transpose ns')