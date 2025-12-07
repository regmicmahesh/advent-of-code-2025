main :: IO ()
replace :: Char -> Char -> String -> String
replace f r = map repl
  where
    repl e = if e == f then r else e

isAllCharDot [x] = x == '.'
isAllCharDot (x:xs) = (x == '.') && isAllCharDot xs

calculate' :: String -> String -> String
calculate' _ "." = "."
calculate' _ "|" = "|"
calculate' _ "^" = "^"
calculate' (x : xs) (y : ys) = c : calculate' xs ys
  where
    c | (head xs == '|') && (head ys == '^') = '|'
      | otherwise = y

calculate a b
    | not $ isAllCharDot b = calculate' (reverse a) (reverse (calculate' a b))
    | otherwise = replace '^' '.' a

main = do
  contents <- readFile "input.txt"
  let l = map (replace 'S' '|') $ lines contents
  let z = foldl calculate (head l) (take 9 l)
  print z
