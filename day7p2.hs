{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

replace :: Char -> Char -> String -> String
replace f r = map repl
  where
    repl e = if e == f then r else e

replaceSwithPipe :: Char -> Char
replaceSwithPipe 'S' = '|'
replaceSwithPipe x = x

replaceSwithPipeString :: String -> String
replaceSwithPipeString = map replaceSwithPipe

isAllCharDot [x] = x == '.'
isAllCharDot (x : xs) = (x == '.') && isAllCharDot xs

-- x is current line and should be '|' and y should be '^' for it to be hitting. (x, y)
isItHitting :: String -> String -> Bool
isItHitting "|" "^" = True
isItHitting [_] [_] = False
isItHitting (x : xs) (y : ys) = (x == '|' && y == '^') || isItHitting xs ys

calculate' :: String -> String -> String
calculate' _ "." = "."
calculate' _ "|" = "|"
calculate' _ "^" = "^"
calculate' (x : xs) (y : ys) = c : calculate' xs ys
  where
    c
      | (head xs == '|') && (head ys == '^') = '|'
      | x == '|' && y /= '^' = x
      | otherwise = y

calculate a b
  | not $ isAllCharDot b = calculate' a b
  | otherwise = replace '^' '.' a

calculateR a b
  | not $ isAllCharDot b = reverse $ calculate' (reverse a) (reverse b)
  | otherwise = replace '^' '.' a

calculateBoth a b = if isItHitting a b then [calculate a b, calculateR a b] else [calculate a b]

reducer :: [String] -> Int
reducer [] = 0
reducer [x] = 0
reducer xs = if length res == 1 then 0 + reducer (newleft : rem) else 1 + reducer (newleft : rem) + reducer (newright : rem)
  where
    l1 = head xs -- .......|.......
    l2 = head $ tail xs -- ...............
    rem = tail $ tail xs -- [.......^.......]
    res = calculateBoth l1 l2 -- [.......|.......]
    newleft = head res
    newright = head $ tail res

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = map replaceSwithPipeString $ lines contents
  print $! 1 + reducer l
