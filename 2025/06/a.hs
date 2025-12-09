import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  content <- readFile "in"
  print $ main' content

main' input =
  let rows = map (filter (not . null) . splitOn " ") (lines input)
      operands = transpose $ map (map read) (init rows)
      operators = last rows
      run xs o
        | o == "+" = foldl1 (+) xs
        | o == "*" = foldl1 (*) xs
      results = zipWith run operands operators
   in sum results
