import Data.Char (isSpace)

-- These are the helper functions that directly works on string.

wordsWhen :: [Char] -> (Char -> Bool) -> [[Char]]
wordsWhen s f = case dropWhile f s of
  "" -> []
  s' -> w : wordsWhen s'' f
    where
      (w, s'') =
        break f s'

trim :: [Char] -> [Char]
trim x = reverse (dropWhile isSpace (reverse (dropWhile isSpace x)))

splitDash :: String -> (String, String)
splitDash x = (head l, last l) where l = wordsWhen x (== '-')


-- This works on numerical values represented as string.

pad :: Int -> String -> String
pad 0 s = s
pad i s
  | i <= sl = s
  | otherwise = replicate np '0' ++ s
  where
    np = i - sl
    sl = length s

padFix2 :: (String, String) -> (String, String)
padFix2 (s1, s2) = (ps1, ps2)
  where
    (ps1, ps2, _) = padFix3 (s1, s2, "")

padFix3 :: (String, String, String) -> (String, String, String)
padFix3 (s1, s2, s3) = (pad maxL s1, pad maxL s2, pad maxL s3)
  where
    maxL = maximum $ map length [s1, s2, s3]

data Range = Range {low :: String, high :: String} deriving (Show)

mkRange :: String -> String -> Range
mkRange l h
  | pl <= ph = Range {low = pl, high = ph}
  | otherwise = error "invalid range"
  where
    (pl, ph) = padFix2 (pl, ph)

isInRange' :: Range -> String -> Bool
isInRange' r v = (low r <= v) && (v <= high r)

-- this implementation also pads the low and high to make sure the comparison is fair
isInRange :: Range -> String -> Bool
isInRange r v = isInRange' Range {low = pl, high = ph} pv
  where
    (pl, ph, pv) = padFix3 (low r, high r, v)

isInRangeList :: [Range] -> String -> Bool
isInRangeList rs v = any (`isInRange` v) rs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (intervals, ids') = break (== "") (lines contents)
  let ids = filter (/= "") ids'
  let tupIntervals = map ((\(l,h) -> Range{low=l,high=h}) . splitDash) intervals
  print $ length $ filter (isInRangeList tupIntervals) ids

