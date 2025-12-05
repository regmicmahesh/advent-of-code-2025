{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Data.List

maxDigitImpl :: (Num t) => [Char] -> t -> Char -> t -> (t, Char)
maxDigitImpl "" currIndex currMax currMaxIndex = (currMaxIndex, currMax)
maxDigitImpl [x] currIndex currMax currMaxIndex = if x > currMax then (currIndex, x) else (currMaxIndex, currMax)
maxDigitImpl (x : xs) currIndex currMax currMaxIndex = if x > currMax then maxDigitImpl xs (currIndex + 1) x currIndex else maxDigitImpl xs (currIndex + 1) currMax currMaxIndex

findMaxDigit string = maxDigitImpl string 0 '0' (-1)

findTwoMax x = do
  read [maxDigit, secondMaxDigit]
  where
    (maxIndex, maxDigit) = findMaxDigit (take (length x - 1) x)
    remaining = drop (maxIndex + 1) x
    (secondMaxIndex, secondMaxDigit) = findMaxDigit (take (length remaining) remaining)

main :: IO ()
main = do
  rawContent <- readFile "input.txt"
  let lines = words rawContent
  let nums = map findTwoMax lines
  print (sum nums)