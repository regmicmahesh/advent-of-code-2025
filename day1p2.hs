parseGear :: Int -> Int -> Char -> Int -> (Int, Int)
parseGear currentVal zCount 'L' changeVal
    | changeVal == 0 = (currentVal, zCount)
    | currentVal == 0 = parseGear 99 (zCount+1) 'L' (changeVal-1)
    | otherwise = parseGear (currentVal-1) zCount 'L' (changeVal-1)

parseGear currentVal zCount 'R' changeVal
    | changeVal == 0 = (currentVal, zCount)
    | currentVal == 99 = parseGear 0 zCount 'R' (changeVal-1)
    | currentVal == 0 = parseGear 1 (zCount+1) 'R' (changeVal-1)
    | otherwise = parseGear (currentVal+1) zCount 'R' (changeVal-1)

main :: IO ()
main = do
    rawContent <- readFile "input.txt"
    let contents = words rawContent
    let (finalVal, zCount) = foldl (\ (currentVal, zCount) el -> parseGear currentVal zCount (head el) (read (tail el) :: Int ) ) (50, 0) contents
    print zCount