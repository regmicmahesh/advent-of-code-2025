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
      | x == '|' && y /= '^' = x
      | otherwise = y

calculate a b
    | not $ isAllCharDot b = reverse $ calculate' (reverse a) (reverse (calculate' a b))
    | otherwise = replace '^' '.' a


beamkotaukocount :: String -> String -> Integer
beamkotaukocount "^" "|" = 1
beamkotaukocount [x] [y] = 0
beamkotaukocount (x:xs) (y:ys) = (if (x == '|') && (y == '^') then 1 else 0) + beamkotaukocount xs ys

main = do
  contents <- readFile "input.txt"
  let l = map (replace 'S' '|') $ lines contents
  let z = map (\x -> foldl1 calculate (take x l)) [1..(length l)]
  print z
  let x = zip (init z) (tail z)
  let d = map (uncurry beamkotaukocount) x
  print $ sum d

