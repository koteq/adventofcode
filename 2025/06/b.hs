import Data.List (transpose)
import Data.List.Split (splitWhen)

main :: IO ()
main = do
  content <- readFile "in"
  print $ main' content

main' input =
  let allRows = map (++ [' ']) (lines input) -- pad right with space to fix parsing edge case
      digitRows = init allRows
      opsRow = last allRows
      ops = parseOpsWithOffsets 0 opsRow
      run (op, startIndex, endIndex) =
        -- list comprehensions are (not) easy to read
        -- it walks rows column by column right-to-left
        -- building numbers accordig to the cephalopod math rules
        let xs = [read $ map (!! idx) digitRows | idx <- [(endIndex - 1), (endIndex - 2) .. startIndex]]
         in foldl1 (parseOp op) xs
      results = map run ops
   in sum results

parseOpsWithOffsets startIndex line
  | startIndex >= length line = []
  | otherwise = (op, startIndex, endIndex) : parseOpsWithOffsets (endIndex + 1) line
  where
    op = line !! startIndex
    charsWithIndexes = zip line [0 ..]
    charsWithIndexes' = drop (startIndex + 1) charsWithIndexes
    (_, endIndex) = last $ takeWhile ((== ' ') . fst) charsWithIndexes'

parseOp c
  | c == '+' = (+)
  | c == '*' = (*)
