-- (direction, distance)
type Instruction = (Char, Int)

-- (position, clicksTotal)
type State = (Int, Int)

main = readFile "in" >>= print . snd . last . dialWithClickCounting 100 50 . map parseLine . lines

parseLine :: String -> Instruction
parseLine (d : n) = (d, read n)

dialWithClickCounting :: Int -> Int -> [Instruction] -> [State]
dialWithClickCounting dialRange dialPos = scanl move (dialPos, 0)
  where
    move (pos, c) (direction, distance) =
      case direction of
        'L' -> ((pos - distance) `mod` dialRange, (((dialRange - pos) `mod` dialRange) + distance) `div` dialRange + c)
        'R' -> ((pos + distance) `mod` dialRange, (((dialRange + pos) `mod` dialRange) + distance) `div` dialRange + c)
