maxDigitImpl :: (Num t) => [Char] -> t -> Char -> t -> (t, Char)
maxDigitImpl [] currIndex currMax currMaxIndex = (currMaxIndex, currMax)
maxDigitImpl [x] currIndex currMax currMaxIndex = if x > currMax then (currIndex, x) else (currMaxIndex, currMax)
maxDigitImpl (x : xs) currIndex currMax currMaxIndex = if x > currMax then maxDigitImpl xs (currIndex + 1) x currIndex else maxDigitImpl xs (currIndex + 1) currMax currMaxIndex

findMaxDigit string =
  (maxIndex, maxDigit)
  where
    (maxIndex, maxDigit) = maxDigitImpl string 0 '0' (-1)

findTwoMax :: [Char] -> Int
findTwoMax x = do
  let (maxIndex1, maxDigit1) = findMaxDigit (take (length x - 11) x)
  let remaining1 = drop (maxIndex1 + 1) x

  let (maxIndex2, maxDigit2) = findMaxDigit (take (length remaining1 - 10) remaining1)
  let remaining2 = drop (maxIndex2 + 1) remaining1

  let (maxIndex3, maxDigit3) = findMaxDigit (take (length remaining2 - 9) remaining2)
  let remaining3 = drop (maxIndex3 + 1) remaining2

  let (maxIndex4, maxDigit4) = findMaxDigit (take (length remaining3 - 8) remaining3)
  let remaining4 = drop (maxIndex4 + 1) remaining3

  let (maxIndex5, maxDigit5) = findMaxDigit (take (length remaining4 - 7) remaining4)
  let remaining5 = drop (maxIndex5 + 1) remaining4

  let (maxIndex6, maxDigit6) = findMaxDigit (take (length remaining5 - 6) remaining5)
  let remaining6 = drop (maxIndex6 + 1) remaining5

  let (maxIndex7, maxDigit7) = findMaxDigit (take (length remaining6 - 5) remaining6)
  let remaining7 = drop (maxIndex7 + 1) remaining6

  let (maxIndex8, maxDigit8) = findMaxDigit (take (length remaining7 - 4) remaining7)
  let remaining8 = drop (maxIndex8 + 1) remaining7

  let (maxIndex9, maxDigit9) = findMaxDigit (take (length remaining8 - 3) remaining8)
  let remaining9 = drop (maxIndex9 + 1) remaining8

  let (maxIndex10, maxDigit10) = findMaxDigit (take (length remaining9 - 2) remaining9)
  let remaining10 = drop (maxIndex10 + 1) remaining9

  let (maxIndex11, maxDigit11) = findMaxDigit (take (length remaining10 - 1) remaining10)
  let remaining11 = drop (maxIndex11 + 1) remaining10

  let (maxIndex12, maxDigit12) = findMaxDigit (take (length remaining11) remaining11)
  let remaining12 = drop (maxIndex12 + 1) remaining11
  read [maxDigit1, maxDigit2, maxDigit3, maxDigit4, maxDigit5, maxDigit6, maxDigit7, maxDigit8, maxDigit9, maxDigit10, maxDigit11, maxDigit12]

main :: IO ()
main = do
  rawContent <- readFile "input.txt"
  let lines = words rawContent
  let nums = map findTwoMax lines
  print nums
  print (sum nums)