{- cabal:
build-depends:
  base,
  split
-}

import Data.List (findIndex, sort, sortBy, tails, find, delete)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as S

type Point = (Int, Int, Int)

type Circuit = S.Set Point

main :: IO ()
main = do
  content <- readFile "in"
  let points = map readPoint $ lines content
      maxConnections = 1000
      circuits = shortestDistanceConnect points maxConnections
      sizes = map S.size circuits
      largest = take 3 $ sortBy (comparing Down) sizes
      result = product largest
  print result

shortestDistanceConnect :: [Point] -> Int -> [Circuit]
shortestDistanceConnect points maxConnections =
  let pairs = combinations2 points
      sorted = sortBy (comparing distance) pairs
      connections = take maxConnections sorted
   in foldl (flip circuitsInsert) [] connections

-- circuitsInsert :: (Point, Point) -> [Circuit] -> [Circuit]
-- circuitsInsert (a, b) [] = [S.fromList [a, b]]
-- circuitsInsert (a, b) (circuit : circuits)
--   | a `S.member` circuit = S.insert b circuit : circuits
--   | b `S.member` circuit = S.insert a circuit : circuits
--   -- TODO when a is part of one circuint and b is parto of another, then JOIN
--   | otherwise = circuit : circuitsInsert (a, b) circuits

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
