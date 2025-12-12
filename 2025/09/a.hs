import Data.List (tails)
import Data.List.Split (splitOn)

type Point = (Int, Int)

main :: IO ()
main = do
  content <- readFile "in"
  let points = map parseLine $ lines content
      result = findLargestArea points
  print result

findLargestArea :: [Point] -> Int
findLargestArea points =
  let pairs = combinations2 points
      areas = map area pairs
   in maximum areas

-- helpers

combinations2 :: [a] -> [(a, a)]
combinations2 points = [(a, b) | (a : bs) <- tails points, b <- bs]

area :: (Point, Point) -> Int
area ((x, y), (a, b)) = w * h
  where
    w = abs (x - a) + 1
    h = abs (y - b) + 1

-- parsing

parseLine :: String -> Point
parseLine s =
  let [a, b] = splitOn "," s
   in (read a, read b)
