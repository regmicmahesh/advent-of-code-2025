roundedSubtract :: Int -> Int -> Int
roundedSubtract x y
    | (x-y) < 0 = (x-y) + 100
    | otherwise = x - y

roundedAdd :: Int -> Int -> Int
roundedAdd x y = mod (x + y) 100

parseGear :: Int -> Int -> Char -> Int -> (Int, Int)
parseGear currentVal zCount 'L' changeVal = 
    (newVal, if newVal == 0 then zCount + 1 else zCount )
    where
        newVal = roundedSubtract currentVal (mod changeVal 100)
    
parseGear currentVal zCount 'R' changeVal = 
    (newVal, if newVal == 0 then zCount + 1 else zCount )
    where
        newVal = roundedAdd currentVal (mod changeVal 100)        


main :: IO ()
main = do
    rawContent <- readFile "input.txt"
    let contents = words rawContent
    let (finalVal, zCount) = foldl (\ (currentVal, zCount) el -> parseGear currentVal zCount (head el) (read (tail el) :: Int ) ) (50, 0) contents
    print zCount