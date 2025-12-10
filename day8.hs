import Data.Function (on)
import Data.List (iterate', minimumBy, nub, sort, sortBy, sortOn)
import Data.Map qualified as Map
import Data.Set qualified as Set


type Point = Int
type Point3D = (Int, Int, Int)

main :: IO ()
wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen f s = case dropWhile f s of
  "" -> []
  s' -> w : wordsWhen f s''
    where
      (w, s'') =
        break f s'

splitComma :: [Char] -> [[Char]]
splitComma = wordsWhen (== ',')

listToInt :: [String] -> Point3D
listToInt lstr = (x, y, z)
  where
    [x, y, z] = map read lstr

distance :: Point3D -> Point3D -> Int
distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

findDistance ::  (Point3D, Point3D) -> Int
findDistance (fc, sc) = distance fc sc

filterV :: (Eq a1) => a1 -> [(a2, a1)] -> [(a2, a1)]
filterV v = filter (\(i, si) -> si == v)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

findMatchingGrouporMakeOne :: Set.Set(Set.Set Point3D) -> Point3D -> Set.Set Point3D
findMatchingGrouporMakeOne bs el = case Set.toList $ Set.filter (Set.member el) bs of
  [] -> Set.fromList [el]
  [a] -> a
  _ -> error "cannot be multiple sets having one element"


popNMatch :: (Set.Set(Set.Set Point3D),  [(Int, Point3D, Point3D)]) -> (Set.Set(Set.Set Point3D),  [(Int, Point3D, Point3D)])
-- if there are no points just return the current accumulator
popNMatch (acc, []) = (acc, [])

popNMatch (acc, points)
  = (newacc, tail points) where
  (_, p1, p2) = head points
  p1g = findMatchingGrouporMakeOne acc p1
  p2g = findMatchingGrouporMakeOne acc p2
  newelToAdd = Set.union p1g p2g
  cleanacc = Set.filter (\x -> x /= p1g && x /= p2g) acc
  newacc = Set.insert newelToAdd cleanacc


main = do
  contents <- readFile "input.txt"

  let coords = map (listToInt . splitComma) (lines contents)
  let indexedCoords = zip coords [0 .. (length coords)]

  let coordPairs = [(fst a, fst b) | a <- indexedCoords, b <- indexedCoords, snd a < snd b]
  let coordPairsWithDist = map (\x -> (findDistance x, fst x, snd x)) coordPairs
  let sortedCoordPairsWithDist = Data.List.sortOn fst3 coordPairsWithDist

  let firstItrn = iterate' popNMatch (Set.fromList [], sortedCoordPairsWithDist)

  let el =  (firstItrn !! 6208)
  --print $ snd el
  print $ Set.map length (fst el)
  print $ head $ snd el