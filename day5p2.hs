import Data.Char (isSpace)
import Data.List (nub, sortBy)

wordsWhen :: [Char] -> (Char -> Bool) -> [[Char]]
wordsWhen s f = case dropWhile f s of
  "" -> []
  s' -> w : wordsWhen s'' f
    where
      (w, s'') =
        break f s'

splitDash :: String -> (String, String)
splitDash x = (head l, last l) where l = wordsWhen x (== '-')

sortByFirstValue :: [(Integer, Integer)] -> [(Integer, Integer)]
sortByFirstValue = sortBy (\(al, ah) (bl, bh) -> if al < bl then LT else GT)

mergeOverlapping :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeOverlapping ((cl, ch) : xs)
  | null xs = [(cl, ch)]
  | nl <= ch = mergeOverlapping ((cl,max ch nh):skippedn)
  | otherwise = (cl, ch):mergeOverlapping xs
  where
    skippedn = tail xs
    (nl, nh) = head xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (intervals, ids') = break (== "") (lines contents)
  let ids = filter (/= "") ids'
  let tupIntervals = map ((\(l, h) -> (read l :: Integer, read h :: Integer)) . splitDash) intervals
  let sorted = mergeOverlapping $ sortByFirstValue tupIntervals
  print  $ sum $ map (\(l,h) -> 1+(h-l)) sorted
