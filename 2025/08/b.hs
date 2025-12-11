{- cabal:
build-depends:
  base,
  split
-}

import Data.List (delete, find, findIndex, sort, sortBy, tails)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as S

type Point = (Int, Int, Int)

type Circuit = S.Set Point

main :: IO ()
main = do
  content <- readFile "in"
  let points = map readPoint $ lines content
      lastConnection = findLastConnection points
      ((a, _, _), (b, _, _)) = lastConnection
      result = a * b
  print result

findLastConnection :: [Point] -> (Point, Point)
findLastConnection points =
  let connections = combinations2 points
      connectionsByDistance = sortBy (comparing distance) connections
      disjointCircuits = [S.fromList [p] | p <- points]
   in foldUntillEverythingConnects disjointCircuits connectionsByDistance

foldUntillEverythingConnects :: [Circuit] -> [(Point, Point)] -> (Point, Point)
foldUntillEverythingConnects circuits (connection : connections)
  | length circuits' == 1 = connection
  | otherwise = foldUntillEverythingConnects circuits' connections
  where
    circuits' = circuitsInsert connection circuits

circuitsInsert :: (Point, Point) -> [Circuit] -> [Circuit]
circuitsInsert (a, b) circuits =
  case ( find (S.member a) circuits,
         find (S.member b) circuits
       ) of
    (Nothing, Nothing) -> S.fromList [a, b] : circuits
    (Just c, Nothing) -> S.insert b c : delete c circuits
    (Nothing, Just c) -> S.insert a c : delete c circuits
    (Just c1, Just c2)
      | c1 == c2 -> circuits
      | otherwise -> S.union c1 c2 : delete c2 (delete c1 circuits)

-- util

combinations2 :: [a] -> [(a, a)]
combinations2 points = [(a, b) | (a : bs) <- tails points, b <- bs]

distance :: (Point, Point) -> Float
distance ((x, y, z), (a, b, c)) =
  sqrt ((x' - a') ^ 2 + (y' - b') ^ 2 + (z' - c') ^ 2)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    z' = fromIntegral z
    a' = fromIntegral a
    b' = fromIntegral b
    c' = fromIntegral c

-- parsing

readPoint :: String -> Point
readPoint s = (x, y, z)
  where
    [a, b, c] = splitOn "," s
    x = read a
    y = read b
    z = read c
