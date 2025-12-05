data GridObject = Empty | Roll deriving (Show, Eq)

type GridRow = [GridObject]

type Grid = [GridRow]

isEmpty :: GridObject -> Bool
isEmpty Empty = True
isEmpty _ = False

isFilled :: GridObject -> Bool
isFilled = not . isEmpty

readCellChar :: Char -> GridObject
readCellChar '.' = Empty
readCellChar '@' = Roll
readCellChar x = error "wtf is that object you placing??"

readString :: String -> GridRow
readString = map readCellChar

readStringArray :: [String] -> Grid
readStringArray = map readString

readStringNewLine :: String -> Grid
readStringNewLine xs = readStringArray (lines xs)

-- returns (col, row)
getGridSize :: Grid -> (Int, Int)
getGridSize [[]] = (0, 0)
getGridSize g = (x, y)
  where
    x = length g
    y = length (head g)

getElementFromGridRow :: GridRow -> Int -> GridObject
getElementFromGridRow [] _ = error "grid row is empty!"
getElementFromGridRow r i = r !! i

-- first is column, then row.
getElementFromGrid :: Grid -> Int -> Int -> GridObject
getElementFromGrid [[]] _ _ = error "grid is empty!"
getElementFromGrid g c r = getElementFromGridRow (g !! c) r

getElementFromGridSafe :: Grid -> Int -> Int -> GridObject
getElementFromGridSafe [[]] _ _ = error "grid is empty!"
getElementFromGridSafe g c r
  | c < 0 || r < 0 || c >= cl || r >= rl = Empty
  | otherwise = getElementFromGrid g c r
  where
    (cl, rl) = getGridSize g

isElementSafeToMove :: Grid -> Int -> Int -> Bool
isElementSafeToMove g c r = length (filter isEmpty [left, right, up, down, upleft, upright, downleft, downright]) > 4
  where
    left = getElementFromGridSafe g c (r - 1)
    right = getElementFromGridSafe g c (r + 1)
    up = getElementFromGridSafe g (c - 1) r
    down = getElementFromGridSafe g (c + 1) r
    upleft = getElementFromGridSafe g (c - 1) (r - 1)
    upright = getElementFromGridSafe g (c - 1) (r + 1)
    downleft = getElementFromGridSafe g (c + 1) (r - 1)
    downright = getElementFromGridSafe g (c + 1) (r + 1)

countSafeToMove :: Grid -> Int -> Int -> Int
countSafeToMove g c r
  | c >= cl && r >= rl = 0
  | c < cl && r >= rl = (if isSafe then 1 else 0) + countSafeToMove g (c + 1) 0
  | otherwise = (if isSafe then 1 else 0) + countSafeToMove g c (r + 1)
  where
    isSafe = isElementSafeToMove g c r && isFilled (getElementFromGridSafe g c r)
    (cl, rl) = getGridSize g

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = readStringNewLine contents
  print (countSafeToMove grid 0 0)