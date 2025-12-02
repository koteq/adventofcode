main :: IO ()
main = do
  content <- readFile "in"
  let instructions = map parseLine (lines content)
  print (length (filter (== 0) (dialCountingZero 100 50 instructions)))

parseLine :: String -> (Char, Int)
parseLine (d : n) = (d, read n)

dialCountingZero :: Int -> Int -> [(Char, Int)] -> [Int]
dialCountingZero dialRange = scanl move
  where
    move pos (direction, distance) =
      case direction of
        'L' -> (pos - distance) `mod` dialRange
        'R' -> (pos + distance) `mod` dialRange
